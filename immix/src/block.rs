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
    line_map: [u8; NUM_LINES_PER_BLOCK],
    /// 第一个hole的起始行号
    first_hole_line_idx: u8,
    /// 第一个hole的长度（行数
    first_hole_line_len: u8,
    /// 是否被标记
    pub marked: bool,
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

    /// # set_line_used
    ///
    /// set the line at nth index as used
    ///
    /// # Safety
    ///
    /// The caller must ensure that the index is valid.
    unsafe fn set_line_used(&mut self, idx: usize) {
        debug_assert!(idx < NUM_LINES_PER_BLOCK);
        self.line_map[idx] |= 1;
    }

    /// # set_line_unused
    ///
    /// set the line at nth index as unused
    ///
    /// # Safety
    ///
    /// The caller must ensure that the index is valid.
    unsafe fn set_line_unused(&mut self, idx: usize) {
        debug_assert!(idx < NUM_LINES_PER_BLOCK);
        self.line_map[idx] &= !1;
    }

    /// # set_line_mark
    ///
    /// set the line at nth index as marked
    ///
    /// # Safety
    ///
    /// The caller must ensure that the index is valid.
    pub unsafe fn set_line_mark(&mut self, idx: usize) {
        debug_assert!(idx < NUM_LINES_PER_BLOCK);
        self.line_map[idx] |= 0b10;
    }

    /// # get_obj_line_size
    ///
    /// get the size of the object start at the line at nth index
    ///
    /// # Safety
    ///
    /// The caller must ensure that the index is valid.
    unsafe fn get_obj_line_size(&self, idx: usize) -> usize {
        debug_assert!(idx < NUM_LINES_PER_BLOCK);
        if self.line_map[idx] & 0b100 == 0 {
            // small object
            1
        } else {
            // medium object
            ((self.line_map[idx] >> 3) | (self.line_map[idx + 1] & !0b111)) as usize + 1
        }
    }

    /// # get_obj_line_size_from_ptr
    ///
    /// get the line size of the object by the pointer
    ///
    /// # Safety
    ///
    /// The caller must ensure that the pointer is valid.
    pub unsafe fn get_obj_line_size_from_ptr(&self, ptr: *mut u8) -> (usize, usize) {
        let line_idx = (ptr as usize - self as *const Self as usize) / LINE_SIZE;
        (line_idx, self.get_obj_line_size(line_idx))
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

    /// # clear_line_mark
    ///
    /// clear the line at nth index mark
    ///
    /// # Safety
    ///
    /// The caller must ensure that the index is valid.
    unsafe fn clear_line_mark(&mut self, idx: usize) {
        debug_assert!(idx < NUM_LINES_PER_BLOCK);
        self.line_map[idx] &= !0b10;
    }

    /// # is_line_used
    ///
    /// check if the line at nth index is used
    ///
    /// # Safety
    ///
    /// The caller must ensure that the index is valid.
    pub unsafe fn is_line_used(&mut self, idx: usize) -> bool {
        debug_assert!(idx < NUM_LINES_PER_BLOCK);
        self.line_map[idx] & 1 == 1
    }

    /// # is_line_marked
    ///
    /// check if the line at nth index is marked
    ///
    /// # Safety
    ///
    /// The caller must ensure that the index is valid.
    pub unsafe fn is_line_marked(&mut self, idx: usize) -> bool {
        debug_assert!(idx < NUM_LINES_PER_BLOCK);
        self.line_map[idx] & 0b10 == 0b10
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
    ) -> Option<(u8, u8, Option<*mut u8>)> {
        let cursor = self.get_line_idx_from_addr(cursor) as u8;
        let mut hole = self.find_next_hole((cursor, 0));
        while let Some((start, len)) = hole {
            if len as usize * LINE_SIZE >= size {
                let line_size = ((size - 1) / LINE_SIZE + 1) as u8;
                // 标记为已使用
                for i in start..=start - 1 + line_size {
                    self.set_line_used(i as usize);
                }
                // 中对象
                if line_size > 1 {
                    // 存的数字实际上是line_size-1，因为长度不能为0，且这样byte正好能存下256
                    let line_size = line_size - 1;
                    self.line_map[start as usize] |= 0b100;
                    self.line_map[start as usize] |= line_size << 3;
                    self.line_map[start as usize + 1] |= line_size & 0b11100000;
                }
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
    use crate::{allocator::GlobalAllocator, consts::LINE_SIZE};

    use super::Block;

    #[test]
    fn test_block_hole() {
        unsafe {
            let mut ga = GlobalAllocator::new(1024 * 1024 * 1024);
            let block = &mut *ga.get_block();
            // 第一个hole应该是从第三行开始，长度是253
            assert_eq!(block.find_first_hole(), Some((3, 253)));
            // 标记hole隔一行之后的第一行为已使用
            block.set_line_used(4);
            // 获取下一个hole，应该是从第五行开始，长度是251
            assert_eq!(block.find_next_hole((3, 1)), Some((5, 251)));
            // 标记hole隔一行之后五行为已使用
            for i in 6..=10 {
                block.set_line_used(i);
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
            block.set_line_used(5);
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
            let (start, len, newcursor) = block.alloc(128, cursor).expect("cannot alloc new line");
            assert_eq!(start, 3);
            assert_eq!(len, 1);
            assert_eq!(newcursor, Some(block.get_nth_line(4)));
            assert_eq!(block.first_hole_line_idx, 4);
            assert_eq!(block.first_hole_line_len, 1);
            let l = block.get_obj_line_size(3);
            assert_eq!(l, 1);
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
            assert_eq!(block.alloc(129, cursor), None);
            assert_eq!(block.first_hole_line_idx, 4);
            assert_eq!(block.first_hole_line_len, 1);

            let cursor = (block as *mut Block as usize + LINE_SIZE * 6) as *mut u8;
            let (start, len, newcursor) = block
                .alloc((256 - 6) * LINE_SIZE, cursor)
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
            let l = block.get_obj_line_size(6);
            assert_eq!(l, 256 - 6);
            assert_eq!(start, 6);
            assert_eq!(len, (256 - 6) as u8);
            assert_eq!(newcursor, None);
            assert_eq!(block.first_hole_line_idx, 4);
            assert_eq!(block.first_hole_line_len, 1);
            let cursor = (block as *mut Block as usize + LINE_SIZE * 3) as *mut u8;
            let (start, len, newcursor) = block.alloc(128, cursor).expect("cannot alloc new line");
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
