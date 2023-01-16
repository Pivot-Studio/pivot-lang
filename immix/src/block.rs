use int_enum::IntEnum;

use crate::consts::{BLOCK_SIZE, LINE_SIZE, NUM_LINES_PER_BLOCK};

/// # Object type
///
/// Object types. Used to support precise GC.
///
/// need 2 bits to represent.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, IntEnum)]
pub enum ObjectType {
    /// Atomic object, means the object does not contain any pointers.
    Atomic = 0,
    /// Trait object, only contains one heap pointer at offset 1.
    Trait = 1,
    /// Complex object, contains multiple heap pointers.
    ///
    /// A complex object must provide a `visit` method to iterate through all heap pointers.
    Complex = 2,
    /// Pointer object, contains one heap pointer.
    Pointer = 3,
}

type LineHeader = u8;

pub trait LineHeaderExt {
    fn get_used(&self) -> bool;
    fn get_marked(&self) -> bool;
    fn get_obj_type(&self) -> ObjectType;
    fn set_used(&mut self, used: bool);
    fn set_marked(&mut self, marked: bool);
    fn set_obj_type(&mut self, obj_type: ObjectType);
}

/// A block is a 32KB memory region.
///
/// A block is divided into 256 lines, each line is 128 bytes.
///
/// **the leading 3 lines are reserved for metadata.**
pub struct Block {
    /// |                           LINE HEADER(1 byte)                         |
    /// |    7   |    6   |    5   |    4   |    3   |    2   |    1   |    0   |
    /// |            not used               |    object type  | marked |  used  |
    line_map: [LineHeader; NUM_LINES_PER_BLOCK],
    /// 第一个hole的起始行号
    first_hole_line_idx: u8,
    /// 第一个hole的长度（行数
    first_hole_line_len: u8,
    /// 是否被标记
    pub marked: bool,
}

impl LineHeaderExt for LineHeader {
    fn get_used(&self) -> bool {
        self & 0b1 == 0b1
    }

    fn get_marked(&self) -> bool {
        self & 0b10 == 0b10
    }

    fn get_obj_type(&self) -> ObjectType {
        match (self >> 2) & 0b11 {
            0 => ObjectType::Atomic,
            1 => ObjectType::Trait,
            2 => ObjectType::Complex,
            _ => unreachable!(),
        }
    }

    fn set_used(&mut self, used: bool) {
        if used {
            *self |= 0b1;
        } else {
            *self &= !0b1;
        }
    }

    fn set_marked(&mut self, marked: bool) {
        if marked {
            *self |= 0b10;
        } else {
            *self &= !0b10;
        }
    }

    fn set_obj_type(&mut self, obj_type: ObjectType) {
        *self &= !0b110;
        *self |= (obj_type as u8) << 2;
    }
}

impl Block {
    /// Create a new block.
    ///
    /// at must be a `BLOCK_SIZE` aligned pointer.
    pub fn new(at: *mut u8) -> &'static mut Self {
        unsafe {
            let ptr = at as *mut Self;
            debug_assert!(ptr as usize % BLOCK_SIZE == 0);
            ptr.write(Self {
                line_map: [0; NUM_LINES_PER_BLOCK],
                first_hole_line_idx: 3, // 跳过前三行，都用来放metadata。浪费一点空间（metadata从0.8%->1.2%）
                first_hole_line_len: (NUM_LINES_PER_BLOCK - 3) as u8,
                marked: false,
            });

            &mut *ptr
        }
    }

    /// show first several lines of the block
    // pub fn show(&self) {
    //     for i in 3..7 {
    //         println!("line_map[{}]: {:08b}", i, self.line_map[i]);
    //     }
    // }

    /// return the used size of the block
    pub fn get_size(&self) -> usize {
        self.line_map[0..NUM_LINES_PER_BLOCK]
            .iter()
            .filter(|&&x| x & 1 == 1)
            .map(|_| 1)
            .sum::<usize>()
    }
    pub fn reset_header(&mut self) {
        self.first_hole_line_idx = 3;
        self.first_hole_line_len = (NUM_LINES_PER_BLOCK - 3) as u8;
        self.marked = false;
    }

    /// # correct_header
    /// 重置block的header
    /// 1. 根据line_map中存储的mark重置line_map中的used
    /// 2. 重置first_hole_line_idx
    /// 3. 重置first_hole_line_len
    /// 4. 重置marked
    pub fn correct_header(&mut self) {
        let mut idx = 3;
        let mut len = 0;
        let mut first_hole_line_idx = 3;
        let mut first_hole_line_len = 0;

        while idx < NUM_LINES_PER_BLOCK {
            // 如果是空行
            if self.line_map[idx] & 1 == 0 {
                len += 1;
            } else {
                // 非空行
                // 如果没被标记了，那么将used bit置为0
                if self.line_map[idx] & 2 == 0 {
                    self.line_map[idx] &= !1;
                }
                // 重置mark bit
                self.line_map[idx] &= !2;
                // 这里遇到了第一个洞的结尾，设置第一个洞的数据
                if len > 0 {
                    if first_hole_line_len == 0 {
                        first_hole_line_idx = idx as u8 - len;
                        first_hole_line_len = len;
                    }
                    len = 0;
                }
            }
            idx += 1;
        }

        if len > 0 {
            if first_hole_line_len == 0 {
                first_hole_line_idx = (idx - len as usize) as u8;
                first_hole_line_len = len;
            }
        }

        self.first_hole_line_idx = first_hole_line_idx;
        self.first_hole_line_len = first_hole_line_len;
        self.marked = false;
    }

    /// # find_next_hole
    ///
    /// imput a tuple (u8, u8) representing previous hole
    ///
    /// Find the next hole in the block.
    ///
    /// Return the start line index and the length of the hole (u8, u8).
    ///
    /// If no hole found, return `None`.
    pub fn find_next_hole(&self, prev_hole: (u8, u8)) -> Option<(u8, u8)> {
        let mut idx = prev_hole.0 as usize + prev_hole.1 as usize;
        let mut len = 0;

        while idx < NUM_LINES_PER_BLOCK {
            // 如果是空行
            if self.line_map[idx] & 1 == 0 {
                len += 1;
            } else {
                if len > 0 {
                    return Some((idx as u8 - len, len));
                }
            }
            idx += 1;
        }

        if len > 0 {
            return Some(((idx - len as usize) as u8, len));
        }
        None
    }

    /// # find_first_hole
    ///
    /// Find the first hole in the block.
    ///
    /// Return the start line index and the length of the hole (u8, u8).
    ///
    /// If no hole found, return `None`.
    pub fn find_first_hole(&self) -> Option<(u8, u8)> {
        if self.first_hole_line_len == 0 {
            return None;
        }
        (self.first_hole_line_idx, self.first_hole_line_len).into()
    }

    /// # get_nth_line
    ///
    /// get the line at nth index as * mut u8
    ///
    /// # Safety
    ///
    /// The caller must ensure that the index is valid.
    pub unsafe fn get_nth_line(&mut self, idx: usize) -> *mut u8 {
        debug_assert!(idx < NUM_LINES_PER_BLOCK);
        (self as *mut Self as *mut u8).add(idx * LINE_SIZE)
    }

    /// # from_obj_ptr
    ///
    /// get the block from a pointer
    ///
    /// note that the pointer does not need to be exactly at the start of the block
    ///
    /// # Safety
    ///
    /// The caller must ensure that the pointer is valid.
    pub unsafe fn from_obj_ptr(ptr: *mut u8) -> &'static mut Self {
        // ptr may not be at the start of the block
        // the block start address is the nearest multiple of BLOCK_SIZE
        // get the block start address
        let ptr = ptr as usize;
        let block_start = ptr - (ptr % BLOCK_SIZE);
        &mut *(block_start as *mut Self)
    }

    pub unsafe fn get_line_header_from_addr(&mut self, addr: *mut u8) -> &mut LineHeader {
        let idx = self.get_line_idx_from_addr(addr);
        self.get_nth_line_header(idx)
    }

    pub unsafe fn get_nth_line_header(&mut self, idx: usize) -> &mut LineHeader {
        debug_assert!(idx < NUM_LINES_PER_BLOCK);
        self.line_map.get_mut(idx).unwrap()
    }

    /// # get_line_idx_from_addr
    ///
    /// get the line index from the given address
    ///
    /// # Safety
    ///
    /// The caller must ensure that the address is in the block.
    unsafe fn get_line_idx_from_addr(&mut self, addr: *mut u8) -> usize {
        debug_assert!(addr >= self as *mut Self as *mut u8);
        debug_assert!(addr < (self as *mut Self as *mut u8).add(BLOCK_SIZE));
        (addr as usize - self as *mut Self as usize) / LINE_SIZE
    }

    /// # alloc
    ///
    /// start from the cursor, use get_next_hole to find a hole of the given size. Return
    /// the start line index and the length of the hole (u8, u8) and the
    /// new cursor position. If no hole found, return `None`. If the cursor is at the end of the block after the
    /// allocation, return `Some((x, y, None))`.
    ///
    /// # Safety
    ///
    /// The caller must ensure that the size is valid.
    pub unsafe fn alloc(
        &mut self,
        size: usize,
        cursor: *mut u8,
        obj_type: ObjectType,
    ) -> Option<(u8, u8, Option<*mut u8>)> {
        let cursor = self.get_line_idx_from_addr(cursor) as u8;
        let mut hole = self.find_next_hole((cursor, 0));
        while let Some((start, len)) = hole {
            if len as usize * LINE_SIZE >= size {
                let line_size = ((size - 1) / LINE_SIZE + 1) as u8;
                // 标记为已使用
                for i in start..=start - 1 + line_size {
                    let header = self.line_map.get_mut(i as usize).unwrap();
                    header.set_used(true);
                }
                // 设置起始line header的obj_type
                let header = self.line_map.get_mut(start as usize).unwrap();
                header.set_obj_type(obj_type);
                // 更新first_hole_line_idx和first_hole_line_len
                if start == self.first_hole_line_idx {
                    self.first_hole_line_idx += line_size;
                    self.first_hole_line_len -= line_size;
                }
                if self.first_hole_line_len == 0 {
                    if let Some((idx, len)) =
                        self.find_next_hole((self.first_hole_line_idx, self.first_hole_line_len))
                    {
                        self.first_hole_line_idx = idx;
                        self.first_hole_line_len = len;
                    }
                }
                let next_cursor_line = start as usize + line_size as usize;
                if next_cursor_line >= NUM_LINES_PER_BLOCK {
                    return Some((start, line_size, None));
                }
                let ptr = self.get_nth_line(next_cursor_line);
                return Some((start, line_size, Some(ptr)));
            }
            if size > LINE_SIZE {
                // 中对象，尝试一次就跳出
                break;
            }
            hole = self.find_next_hole((start, len));
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::{allocator::GlobalAllocator, block::LineHeaderExt, consts::LINE_SIZE};

    use super::Block;

    #[test]
    fn test_block_hole() {
        unsafe {
            let mut ga = GlobalAllocator::new(1024 * 1024 * 1024);
            let block = &mut *ga.get_block();
            // 第一个hole应该是从第三行开始，长度是253
            assert_eq!(block.find_first_hole(), Some((3, 253)));
            // 标记hole隔一行之后的第一行为已使用
            let header = block.get_nth_line_header(4);
            header.set_used(true);
            // 获取下一个hole，应该是从第五行开始，长度是251
            assert_eq!(block.find_next_hole((3, 1)), Some((5, 251)));
            // 标记hole隔一行之后五行为已使用
            for i in 6..=10 {
                let header = block.get_nth_line_header(i as usize);
                header.set_used(true);
            }
            // 获取下一个hole，应该是从第十一行开始，长度是245
            assert_eq!(block.find_next_hole((5, 5)), Some((11, 245)));
        };
    }
    #[test]
    fn test_alloc() {
        unsafe {
            let mut ga = GlobalAllocator::new(1024 * 1024 * 1024);
            let block = &mut *ga.get_block();
            // 设置第5行已被使用
            block.get_nth_line_header(5).set_used(true);
            block.first_hole_line_len = 2;
            // 从第三行开始分配，长度为128
            // 分配前：
            // --------
            // |  0   | meta
            // |  1   | meta
            // |  2   | meta
            // |  3   | 空
            // |  4   | 空
            // |  5   | 已使用
            // |  6   | 空
            // |  7   | 空
            // ......
            // 分配后：
            // --------
            // |  0   | meta
            // |  1   | meta
            // |  2   | meta
            // |  3   | 已使用
            // |  4   | 空
            // |  5   | 已使用
            // |  6   | 空
            // |  7   | 空
            // ......
            let cursor = (block as *mut Block as usize + LINE_SIZE * 3) as *mut u8;
            let (start, len, newcursor) = block
                .alloc(128, cursor, crate::block::ObjectType::Atomic)
                .expect("cannot alloc new line");
            assert_eq!(start, 3);
            assert_eq!(len, 1);
            assert_eq!(newcursor, Some(block.get_nth_line(4)));
            assert_eq!(block.first_hole_line_idx, 4);
            assert_eq!(block.first_hole_line_len, 1);
            let l = block.get_nth_line_header(3).get_obj_type();
            assert_eq!(l, crate::block::ObjectType::Atomic);
            let cursor = newcursor.unwrap();
            // 从第4行开始分配，长度为129
            // 分配前：
            // --------
            // |  0   | meta
            // |  1   | meta
            // |  2   | meta
            // |  3   | 已使用
            // |  4   | 空
            // |  5   | 已使用
            // |  6   | 空
            // |  7   | 空
            // ......
            // 分配失败
            // ......
            assert_eq!(
                block.alloc(129, cursor, crate::block::ObjectType::Atomic),
                None
            );
            assert_eq!(block.first_hole_line_idx, 4);
            assert_eq!(block.first_hole_line_len, 1);

            let cursor = (block as *mut Block as usize + LINE_SIZE * 6) as *mut u8;
            let (start, len, newcursor) = block
                .alloc(
                    (256 - 6) * LINE_SIZE,
                    cursor,
                    crate::block::ObjectType::Complex,
                )
                .expect("cannot alloc new line");
            // 从第6行开始分配，长度为256-6
            // 分配后：
            // --------
            // |  0   | meta
            // |  1   | meta
            // |  2   | meta
            // |  3   | 已使用
            // |  4   | 空
            // |  5   | 已使用
            // |  6   | 已使用
            // |  7   | 已使用
            // |  8   | 已使用
            // ......
            // |  255 | 已使用
            let l = block.get_nth_line_header(6).get_obj_type();
            assert_eq!(l, crate::block::ObjectType::Complex);
            assert_eq!(start, 6);
            assert_eq!(len, (256 - 6) as u8);
            assert_eq!(newcursor, None);
            assert_eq!(block.first_hole_line_idx, 4);
            assert_eq!(block.first_hole_line_len, 1);
            let cursor = (block as *mut Block as usize + LINE_SIZE * 3) as *mut u8;
            let (start, len, newcursor) = block
                .alloc(128, cursor, crate::block::ObjectType::Atomic)
                .expect("cannot alloc new line");
            // 从第4行开始分配，长度为1
            // 分配后：
            // --------
            // |  0   | meta
            // |  1   | meta
            // |  2   | meta
            // |  3   | 已使用
            // |  4   | 已使用
            // |  5   | 已使用
            // |  6   | 已使用
            // |  7   | 已使用
            // |  8   | 已使用
            // ......
            // |  255 | 已使用
            assert_eq!(start, 4);
            assert_eq!(len, 1);
            assert_eq!(newcursor, Some(block.get_nth_line(5)));
            // assert_eq!(block.first_hole_line_idx, 255); 这个时候没hole了，此值无意义，len为0
            assert_eq!(block.first_hole_line_len, 0);
        }
    }
}
