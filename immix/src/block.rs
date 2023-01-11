use crate::consts::{BLOCK_SIZE, LINE_SIZE, NUM_LINES_PER_BLOCK};

/// A block is a 32KB memory region.
///
/// A block is divided into 256 lines, each line is 128 bytes.
///
/// **the leading 3 lines are reserved for metadata.**
pub struct Block {
    /// 从右往左，第一个bit是标识是否为空，第二个bit是mark bit
    pub line_map: [u8; NUM_LINES_PER_BLOCK],
    /// 第一个hole的起始行号
    pub first_hole_line_idx: u8,
    /// 第一个hole的长度（行数
    pub first_hole_line_len: u8,
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
            });

            &mut *ptr
        }
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
}

#[cfg(test)]
mod tests {
    use crate::global_allocator::GlobalAllocator;


    #[test]
    fn test_block_hole() {
        unsafe {
            let mut ga = GlobalAllocator::new(1024 * 1024 * 1024);
            let block = &mut *ga.get_block();
            // 第一个hole应该是从第三行开始，长度是253
            assert_eq!(block.find_first_hole(), Some((3, 253)));
            // 标记hole隔一行之后的第一行为已使用
            block.line_map[4] = 1;
            // 获取下一个hole，应该是从第五行开始，长度是251
            assert_eq!(block.find_next_hole((3, 1)), Some((5, 251)));
            // 标记hole隔一行之后五行为已使用
            block.line_map[6] = 1;
            block.line_map[7] = 1;
            block.line_map[8] = 1;
            block.line_map[9] = 1;
            block.line_map[10] = 1;
            // 获取下一个hole，应该是从第十一行开始，长度是245
            assert_eq!(block.find_next_hole((5, 5)), Some((11, 245)));

        };
    }
}
