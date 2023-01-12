//! # Thread-local allocator
//!
//! This module contains the thread-local allocator, which is associated with each thread.
//!
//! Thread-local allocator get empty blocks from global allocator, and allocate objects from these blocks.
//!
//! When a thread-local allocator is dropped, it will return all blocks to global allocator.

use parking_lot::ReentrantMutex;

use crate::block::Block;

use super::GlobalAllocator;

/// # struct ThreadLocalAllocator
///
/// Thread-local allocator, associated with each thread.
///
/// Thread-local allocator get empty blocks from global allocator, and allocate objects from these blocks.
///
/// When a thread-local allocator is dropped, it will return all blocks to global allocator.
///
/// ## Fields
///
/// * `global_allocator` - global allocator
/// * `unavailable_blocks` - unavailable blocks
/// * `current_block` - current block
/// * `recyclable_blocks` - recyclable blocks
/// * `lock` - lock
pub struct ThreadLocalAllocator {
    global_allocator: *mut GlobalAllocator,
    unavailable_blocks: Vec<*mut Block>,
    recyclable_blocks: Vec<*mut Block>,
    lock: ReentrantMutex<()>,
    cursor: *mut u8,
}

impl ThreadLocalAllocator {
    /// # new
    ///
    /// Create a new thread-local allocator.
    ///
    /// ## Parameters
    ///
    /// * `global_allocator` - global allocator
    pub fn new(global_allocator: *mut GlobalAllocator) -> Self {
        Self {
            global_allocator,
            unavailable_blocks: Vec::new(),
            recyclable_blocks: Vec::new(),
            lock: ReentrantMutex::new(()),
            cursor: std::ptr::null_mut(),
        }
    }

    /// # alloc
    ///
    /// 优先从recycle blocks中分配，每用完一个recycle block则将block从
    /// recycle列表中移除，移入unavailable blocks中。当recycle blocks为空时，申请新的
    /// 空block进行分配
    ///
    /// TODO: 中等对象分配实现overflow alloc，大对象分配使用large obj alloc
    ///
    /// ## Parameters
    ///
    /// * `size` - object size
    ///
    /// ## Return
    ///
    /// * `*mut u8` - object pointer
    pub fn alloc(&mut self, size: usize) -> *mut u8 {
        // 空cursor，代表刚启动或者recycle block全用光了
        if self.cursor.is_null() {
            let block = self.get_block();
            self.cursor = unsafe { (*block).get_nth_line(3) };
            unsafe {
                let (s, l, nxt) = (*block).alloc(size, self.cursor).unwrap();
                let re = (*block).get_nth_line(s as usize);
                nxt.or_else(|| {
                    self.cursor = std::ptr::null_mut();
                    None
                })
                .and_then(|n| {
                    self.cursor = n;
                    None::<()>
                });
                return re;
            }
        }

        let f = self.recyclable_blocks.first().unwrap();
        let (s, _, nxt) = unsafe { (**f).alloc(size, self.cursor).unwrap() };
        let re = unsafe { (**f).get_nth_line(s as usize) };
        nxt.or_else(|| {
            // 当前block被用完，将它从recyclable blocks中移除，加入unavailable blocks
            let used_block = self.recyclable_blocks.pop().unwrap();
            self.unavailable_blocks.push(used_block);
            // 如果还有recyclable_blocks 则指向下一个block的第一个可用line
            self.recyclable_blocks
                .first()
                .and_then(|b| {
                    self.cursor = unsafe { (**b).get_nth_line(3) };
                    Some(())
                })
                .or_else(|| {
                    self.cursor = std::ptr::null_mut();
                    None
                });
            None
        })
        .and_then(|n| {
            self.cursor = n;
            None::<()>
        });
        re
    }

    /// # get_block
    ///
    /// Get a block from global allocator.
    ///
    /// If there is a recyclable block, get it from recyclable blocks.
    ///
    /// Otherwise, get a new block from global allocator.
    ///
    /// ## Return
    ///
    /// * `*mut Block` - block pointer
    fn get_block(&mut self) -> *mut Block {
        if let Some(block) = self.recyclable_blocks.pop() {
            block
        } else {
            let block = unsafe { (&mut *self.global_allocator).get_block() };
            self.unavailable_blocks.push(block);
            block
        }
    }

    /// # recycle
    ///
    /// Recycle a block.
    ///
    /// ## Parameters
    ///
    /// * `block` - block pointer
    pub fn recycle(&mut self, block: *mut Block) {
        let _lock = self.lock.lock();

        if self.unavailable_blocks.contains(&block) {
            self.unavailable_blocks.retain(|&b| b != block);
            self.recyclable_blocks.push(block);
        }
    }
}
