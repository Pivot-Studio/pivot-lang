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
    current_block: *mut Block,
    recyclable_blocks: Vec<*mut Block>,
    lock: ReentrantMutex<()>,
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
            current_block: std::ptr::null_mut(),
            recyclable_blocks: Vec::new(),
            lock: ReentrantMutex::new(()),
        }
    }

    /// # alloc
    ///
    /// Allocate an object from current block.
    ///
    /// If current block is full, get a new block from global allocator.
    ///
    /// ## Parameters
    ///
    /// * `size` - object size
    ///
    /// ## Return
    ///
    /// * `*mut u8` - object pointer
    // pub fn alloc(&mut self, size: usize) -> *mut u8 {
    //     let _lock = self.lock.lock();

    //     if self.current_block.is_null() {
    //         self.current_block = self.get_block();
    //     }

    //     let current_block = self.current_block;

    //     let ptr = unsafe { (*current_block).alloc(size) };

    //     if ptr.is_null() {
    //         self.current_block = self.get_block();
    //         let current_block = self.current_block;
    //         unsafe { (*current_block).alloc(size) }
    //     } else {
    //         ptr
    //     }
    // }

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
