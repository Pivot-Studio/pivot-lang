use std::sync::atomic::{AtomicU8, Ordering};

use int_enum::IntEnum;
use vector_map::VecMap;

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

pub trait HeaderExt {
    fn get_used(&self) -> bool;
    fn get_marked(&self) -> bool;
    fn get_obj_type(&self) -> ObjectType;
    fn set_used(&mut self, used: bool);
    fn set_marked(&mut self, marked: bool);
    fn set_obj_type(&mut self, obj_type: ObjectType);
}

pub trait LineHeaderExt {
    fn set_is_head(&mut self, is_head: bool);
    fn get_is_used_follow(&self) -> bool;
    fn get_obj_line_size(&self, idx: usize, block: &mut Block) -> usize;
    fn set_forwarded(&mut self, forwarded: bool);
    fn get_forwarded(&self) -> bool;
}

/// A block is a 32KB memory region.
///
/// A block is divided into 256 lines, each line is 128 bytes.
///
/// **the leading 3 lines are reserved for metadata.**
pub struct Block {
    /// |                           LINE HEADER(1 byte)                         |
    /// |    7   |    6   |    5   |    4   |    3   |    2   |    1   |    0   |
    /// | is head|   eva  |     not used    |    object type  | marked |  used  |
    line_map: [LineHeader; NUM_LINES_PER_BLOCK],
    /// 第一个hole的起始行号
    cursor: usize,
    /// 第一个hole的长度（行数
    limit: usize,
    /// 是否被标记
    pub marked: bool,
    /// 洞的数量
    hole_num: usize,
    available_line_num: usize,
    eva_target: bool,
}

impl HeaderExt for u8 {
    #[inline]
    fn get_used(&self) -> bool {
        self & 0b1 == 0b1
    }
    #[inline]
    fn get_marked(&self) -> bool {
        self & 0b10 == 0b10
    }
    #[inline]
    fn get_obj_type(&self) -> ObjectType {
        ObjectType::from_int((self >> 2) & 0b11).expect("invalid object type")
    }
    #[inline]
    fn set_used(&mut self, used: bool) {
        if used {
            *self |= 0b1;
        } else {
            *self &= !0b1;
        }
    }
    #[inline]
    fn set_obj_type(&mut self, obj_type: ObjectType) {
        // *self &= !0b110;
        *self |= (obj_type as u8) << 2;
    }
    #[inline]
    fn set_marked(&mut self, marked: bool) {
        debug_assert!(*self & 0b10000000 != 0);
        if marked {
            *self |= 0b10;
        } else {
            *self &= !0b10;
        }
    }
}
impl LineHeaderExt for LineHeader {
    #[inline]
    fn set_forwarded(&mut self, forwarded: bool) {
        let atom_self = self as *mut u8 as *mut AtomicU8;
        unsafe {
            let value = (*atom_self).load(Ordering::SeqCst);
            let forwarded = if forwarded {
                value | 0b1000000
            } else {
                value & !0b1000000
            };
            (*atom_self).store(forwarded, Ordering::Release);
        }
    }
    #[inline]
    fn get_forwarded(&self) -> bool {
        let atom_self = self as *const u8 as *const AtomicU8;
        unsafe { (*atom_self).load(Ordering::Acquire) & 0b1000000 == 0b1000000 }
    }

    fn get_obj_line_size(&self, idx: usize, block: &mut Block) -> usize {
        // 自己必须是head
        debug_assert!(*self & 0b10000000 != 0);
        // 自己必须是used
        debug_assert!(*self & 0b1 != 0);
        // 往后遍历获取自身大小
        let mut line_size = 1;
        while idx + line_size < NUM_LINES_PER_BLOCK
            && block.line_map[idx + line_size] & 0b10000001 == 0b00000001
        {
            line_size += 1;
        }
        line_size
    }

    fn set_is_head(&mut self, is_head: bool) {
        if is_head {
            *self |= 0b10000000;
        } else {
            *self &= !0b10000000;
        }
    }
    #[inline]
    fn get_is_used_follow(&self) -> bool {
        self & 0b10000001 == 0b00000001
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
                cursor: 3, // 跳过前三行，都用来放metadata。浪费一点空间（metadata从0.8%->1.2%）
                limit: (NUM_LINES_PER_BLOCK - 3),
                marked: false,
                hole_num: 1,
                available_line_num: NUM_LINES_PER_BLOCK - 3,
                eva_target: false,
            });

            &mut *ptr
        }
    }

    pub fn get_available_line_num_and_holes(&self) -> (usize, usize) {
        (self.available_line_num, self.hole_num)
    }

    pub fn show(&self) {
        println!("size: {}", self.get_size());
        println!("first_hole_line_idx: {}", self.cursor);
        println!("first_hole_line_len: {}", self.limit);
        println!("marked: {}", self.marked);
        println!("hole_num: {}", self.hole_num);
        println!("available_line_num: {}", self.available_line_num);
        println!("line_map: {:?}", self.line_map);
    }

    /// return the used size of the block
    pub fn get_size(&self) -> usize {
        self.line_map[3..NUM_LINES_PER_BLOCK]
            .iter()
            .filter(|&&x| x & 1 == 1)
            .map(|_| 1)
            .sum::<usize>()
    }

    pub fn iter<F>(&mut self, mut f: F)
    where
        F: FnMut(*mut u8),
    {
        let ptr = self as *mut Self as *mut u8;
        self.line_map[0..NUM_LINES_PER_BLOCK]
            .iter()
            .enumerate()
            .filter(|(_, x)| **x & 1 == 1)
            .for_each(|(i, _)| unsafe { f(ptr.add(i * LINE_SIZE)) })
    }
    pub fn reset_header(&mut self) {
        self.cursor = 3;
        self.limit = NUM_LINES_PER_BLOCK - 3;
        self.line_map = [0; NUM_LINES_PER_BLOCK];
        self.marked = false;
        self.hole_num = 1;
        self.available_line_num = NUM_LINES_PER_BLOCK - 3;
        self.eva_target = false;
    }

    /// # correct_header
    /// 回收的最后阶段，重置block的header
    pub unsafe fn correct_header(&mut self, mark_histogram: *mut VecMap<usize, usize>) -> usize {
        let mut idx = 3;
        let mut len = 0;
        let mut first_hole_line_idx: usize = 3;
        let mut first_hole_line_len: usize = 0;
        let mut holes = 0;
        // 这个marked代表之前是否有被标记的对象头出现
        let mut marked = false;
        let mut marked_num = 0;
        self.available_line_num = 0;

        while idx < NUM_LINES_PER_BLOCK {
            // 未使用或者未标记
            if !self.line_map[idx].get_used()
                || (self.line_map[idx] & 0b10 == 0 //即使标记位为0，也有可能是被标记的对象数据体
                    && (!marked || self.line_map[idx] & 0b10000010 == 0b10000000))
            {
                len += 1;
                self.line_map[idx] &= 0;
                marked = false;
                self.available_line_num += 1;
            } else {
                // 如果是obj的第一个line且被标记了
                if self.line_map[idx] & 0b10000010 == 0b10000010 {
                    marked = true;
                }
                if marked {
                    marked_num += 1;
                }
                // 重置mark bit
                self.line_map[idx] &= !2;
                if len > 0 {
                    // 这里遇到了第一个洞的结尾，设置第一个洞的数据
                    if first_hole_line_len == 0 {
                        first_hole_line_idx = idx - len;
                        first_hole_line_len = len;
                    }
                    holes += 1;
                    len = 0;
                }
            }
            idx += 1;
        }

        if len > 0 {
            if first_hole_line_len == 0 {
                first_hole_line_idx = idx - len;
                first_hole_line_len = len;
            }
            holes += 1;
        }

        self.cursor = first_hole_line_idx;
        self.limit = first_hole_line_len;
        self.marked = false;
        self.hole_num = holes;
        self.eva_target = false;
        // println!("holes: {}, first_idx: {} , first_len: {} {:?}", holes,first_hole_line_idx,first_hole_line_len,self.line_map.iter().map(|&x| x & 1).collect::<Vec<_>>());
        if let Some(count) = (*mark_histogram).get_mut(&self.hole_num) {
            *count += marked_num;
        } else {
            (*mark_histogram).insert(self.hole_num, marked_num);
        }
        marked_num
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
    pub fn find_next_hole(
        &self,
        prev_hole: (usize, usize),
        size_line: usize,
    ) -> Option<(usize, usize)> {
        let mut idx = prev_hole.0 + prev_hole.1;
        let mut len = 0;

        while idx < idx + size_line {
            if idx >= NUM_LINES_PER_BLOCK {
                return None;
            }
            // 如果是空行
            if self.line_map[idx] & 1 == 0 {
                len += 1;
                if len >= size_line {
                    return Some((idx - len + 1, len));
                }
            } else {
                if len >= size_line {
                    return Some((idx - len, len));
                } else if len != 0 {
                    // len = 0;
                    return None;
                }
                len = 0;
            }
            // self.show();
            // panic!("prev_hole: {:?}, idx: {}, len: {}, size_line: {}", prev_hole, idx, len, size_line);
            idx += 1;
        }
        // panic!("xxx");

        if len >= size_line {
            return Some((idx - len, len));
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
    pub fn find_first_hole(&self) -> Option<(usize, usize)> {
        if self.limit == 0 {
            return None;
        }
        (self.cursor, self.limit).into()
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

    pub unsafe fn get_line_header_from_addr(&mut self, addr: *mut u8) -> (&mut LineHeader, usize) {
        let idx = self.get_line_idx_from_addr(addr);
        (self.get_nth_line_header(idx), idx)
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
    unsafe fn get_line_idx_from_addr(&self, addr: *mut u8) -> usize {
        debug_assert!(addr as *const u8 >= self as *const Self as *const u8);
        debug_assert!((addr as *const u8) < (self as *const Self as *const u8).add(BLOCK_SIZE));
        (addr as usize - self as *const Self as usize) / LINE_SIZE
    }

    /// # set_eva_threshold
    ///
    /// set the eva_target flag to true if the block's hole number is greater than the threshold
    pub fn set_eva_threshold(&mut self, threshold: usize) {
        self.eva_target = self.hole_num > threshold;
    }

    pub fn is_eva_candidate(&self) -> bool {
        self.eva_target
    }

    /// # alloc
    ///
    /// start from the cursor, use get_next_hole to find a hole of the given size. Return
    /// the start line index and whether this block can allocate more objects.
    ///
    /// # Safety
    ///
    /// The caller must ensure that the size is valid.
    #[inline]
    pub unsafe fn alloc(&mut self, size: usize, obj_type: ObjectType) -> Option<(usize, bool)> {
        let cursor = self.cursor;
        let line_size = (size - 1) / LINE_SIZE + 1;
        if line_size + self.cursor > NUM_LINES_PER_BLOCK {
            return None;
        }
        let hole = self.find_next_hole((cursor, 0), line_size);
        // debug_assert!(hole.is_some(), "cursor {}, header {:?}, hole: {} av {} first idx {} first_len {}", cursor,self.line_map,self.hole_num,self.available_line_num, self.first_hole_line_idx,self.first_hole_line_len);
        if let Some((start, len)) = hole {
            self.available_line_num -= line_size;
            // 标记为已使用
            for i in start..=start - 1 + line_size {
                let header = self.line_map.get_mut(i).unwrap();
                header.set_used(true);
            }
            // 设置起始line header的obj_type
            let header = self.line_map.get_mut(start).unwrap();
            *header |= (obj_type as u8) << 2 | 0b10000000;
            // header.set_obj_type(obj_type);
            // header.set_is_head(true);
            // 更新first_hole_line_idx和first_hole_line_len
            if start == self.cursor {
                self.cursor += line_size;
                self.limit -= line_size;
            }
            if self.limit == 0 {
                if let Some((idx, len)) = self.find_next_hole((self.cursor, self.limit), 1) {
                    self.cursor = idx;
                    self.limit = len;
                } else {
                    self.hole_num -= 1;
                    return Some((start, false));
                }
            }
            if self.cursor > start + len && len == line_size {
                // 正好匹配，那么减少一个hole
                self.hole_num -= 1;
            }
            return Some((start, true));
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::{allocator::GlobalAllocator, consts::LINE_SIZE, HeaderExt, BLOCK_SIZE};

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
            assert_eq!(block.find_next_hole((3, 1), 1), Some((5, 1)));
            // 标记hole隔一行之后五行为已使用
            for i in 6..=10 {
                let header = block.get_nth_line_header(i as usize);
                header.set_used(true);
            }
            // 获取下一个hole，应该是从第十一行开始，长度是245
            assert_eq!(block.find_next_hole((5, 5), 1), Some((11, 1)));
        };
    }
    #[test]
    fn test_alloc() {
        unsafe {
            let mut ga = GlobalAllocator::new(1024 * 1024 * 1024);
            let block = &mut *ga.get_block();
            // 设置第5行已被使用
            block.get_nth_line_header(5).set_used(true);
            block.limit = 2;
            block.hole_num = 2;
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
            let (start, _) = block
                .alloc(128, crate::block::ObjectType::Atomic)
                .expect("cannot alloc new line");
            assert_eq!(start, 3);
            // assert_eq!(newcursor, Some(block.get_nth_line(4)));
            assert_eq!(block.cursor, 4);
            assert_eq!(block.limit, 1);
            let l = block.get_nth_line_header(3).get_obj_type();
            assert_eq!(l, crate::block::ObjectType::Atomic);
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
            assert_eq!(block.alloc(129, crate::block::ObjectType::Atomic), None);
            assert_eq!(block.cursor, 4);
            assert_eq!(block.limit, 1);

            block.cursor = 6;
            block.limit = 250;
            let (start, newcursor) = block
                .alloc((256 - 6) * LINE_SIZE, crate::block::ObjectType::Complex)
                .expect("cannot alloc new line");
            block.cursor = 4;
            block.limit = 1;
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
            assert_eq!(newcursor, false);
            assert_eq!(block.cursor, 4);
            assert_eq!(block.limit, 1);
            let (start, newcursor) = block
                .alloc(128, crate::block::ObjectType::Atomic)
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
            assert_eq!(newcursor, false);
            // assert_eq!(block.first_hole_line_idx, 255); 这个时候没hole了，此值无意义，len为0
            assert_eq!(block.limit, 0);

            // test big alloc
            let obj = ga.get_big_obj(BLOCK_SIZE);
            ga.big_obj_allocator.state();
            println!("obj: {:?}\n", obj);
            // |      1       |

            ga.return_big_objs(vec![obj]);

            let obj = ga.get_big_obj(BLOCK_SIZE + 1);
            ga.big_obj_allocator.state();
            println!("obj: {:?}\n", obj);
            // |      1*      |          2        |

            let obj = ga.get_big_obj(BLOCK_SIZE - 1);
            ga.big_obj_allocator.state();
            println!("obj: {:?}\n", obj);
            // |      3       |          2        |
        }
    }
}
