//! # Thread-local allocator
//!
//! This module contains the thread-local allocator, which is associated with each thread.
//!
//! Thread-local allocator get empty blocks from global allocator, and allocate objects from these blocks.
//!
//! When a thread-local allocator is dropped, it will return all blocks to global allocator.

use std::collections::VecDeque;

use vector_map::VecMap;

use crate::{
    block::{Block, ObjectType},
    consts::{BLOCK_SIZE, LINE_SIZE},
    EVA_BLOCK_PROPORTION, NUM_LINES_PER_BLOCK,
};

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
    recyclable_blocks: VecDeque<*mut Block>,
    eva_blocks: Vec<*mut Block>,
    cursor: *mut u8,
    collect_mode: bool,
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
            recyclable_blocks: VecDeque::new(),
            cursor: std::ptr::null_mut(),
            eva_blocks: Vec::new(),
            collect_mode: false,
        }
    }

    pub fn set_collect_mode(&mut self, collect_mode: bool) {
        self.collect_mode = collect_mode;
        self.unavailable_blocks
            .extend(self.recyclable_blocks.drain(..));
        self.cursor = std::ptr::null_mut();
    }

    pub fn print_stats(&self) {
        println!("unavailable blocks:");
        for block in &self.unavailable_blocks {
            unsafe {
                (**block).show();
            }
        }
        println!("recyclable blocks:");
        for block in &self.recyclable_blocks {
            unsafe {
                (**block).show();
            }
        }
    }

    pub fn iter<F>(&self, mut f: F)
    where
        F: FnMut(*mut u8),
    {
        for &b in self
            .unavailable_blocks
            .iter()
            .chain(self.recyclable_blocks.iter())
        {
            unsafe { (*b).iter(&mut f) }
        }
    }

    /// # should_eva
    ///
    /// whether the collection should run evacuation algorithm
    pub fn should_eva(&self) -> bool {
        !self.recyclable_blocks.is_empty()
    }

    pub fn get_available_lines(&self) -> usize {
        self.eva_blocks.len() * NUM_LINES_PER_BLOCK
    }

    pub fn set_eva_threshold(&mut self, threshold: usize) {
        self.recyclable_blocks
            .iter()
            .chain(self.unavailable_blocks.iter())
            .for_each(|block| unsafe { (**block).set_eva_threshold(threshold) });
    }

    pub fn reset_line_maps(&mut self) {
        self.recyclable_blocks
            .iter()
            .chain(self.unavailable_blocks.iter())
            .for_each(|block| unsafe { (**block).reset_line_map() });
    }

    /// # get_size
    ///
    /// Get the size of allocated space.
    ///
    /// ## Return
    ///
    /// * `usize` - size
    pub fn get_size(&self) -> usize {
        let mut size = 0;
        // println!("unavailable blocks:");
        for block in &self.unavailable_blocks {
            // unsafe{(**block).show();}
            size += unsafe { (**block).get_size() };
        }
        // println!("recyclable blocks:");
        for block in &self.recyclable_blocks {
            // unsafe{(**block).show();}
            size += unsafe { (**block).get_size() };
        }
        size
    }
    /// # alloc
    ///
    /// 优先从recycle blocks中分配，如果中对象分配失败，使用overflow_alloc，
    /// 每用完一个recycle block则将block从recycle列表中移除，移入unavailable blocks中。
    /// 当recycle blocks为空时，申请新的空block进行分配
    ///
    /// ## Parameters
    ///
    /// * `size` - object size
    /// * `obj_type` - object type
    ///
    /// ## Return
    ///
    /// * `*mut u8` - object pointer
    pub fn alloc(&mut self, size: usize, obj_type: ObjectType) -> *mut u8 {
        // big size object
        if size > ((BLOCK_SIZE / LINE_SIZE - 3) / 4 - 1) * LINE_SIZE {
            return self.big_obj_alloc(size);
        }
        // mid size object & small size object
        // 空cursor，代表刚启动或者recycle block全用光了
        if self.cursor.is_null() {
            let block = self.get_new_block();
            self.cursor = unsafe { (*block).get_nth_line(3) };
            unsafe {
                let (re, nxt) = (*block).alloc(size, self.cursor, obj_type).unwrap();
                nxt.or_else(|| {
                    self.cursor = std::ptr::null_mut();
                    self.unavailable_blocks.push(block);
                    None
                })
                .and_then(|n| {
                    self.cursor = n;
                    self.recyclable_blocks.push_back(block);
                    None::<()>
                });
                return re;
            }
        }
        let mut f = self.recyclable_blocks.front().unwrap();
        unsafe {
            while (**f).is_eva_candidate() {
                let uf = self.recyclable_blocks.pop_front().unwrap();
                self.unavailable_blocks.push(uf);
                let ff = self.recyclable_blocks.front();
                if ff.is_none() {
                    // recycle blocks全用光了
                    self.cursor = std::ptr::null_mut();
                    return self.alloc(size, obj_type);
                } else {
                    f = ff.unwrap()
                }
                self.cursor = (**f).get_nth_line((**f).find_first_hole().unwrap().0 as usize);
            }
        }
        let res = unsafe { (**f).alloc(size, self.cursor, obj_type) };
        // return std::ptr::null_mut();
        if res.is_none() && size + 8 > LINE_SIZE {
            // mid size object alloc failed, try to overflow_alloc
            return self.overflow_alloc(size, obj_type);
        }
        let (re, nxt) = res.unwrap();
        nxt.or_else(|| {
            // 当前block被用完，将它从recyclable blocks中移除，加入unavailable blocks
            let used_block = self.recyclable_blocks.pop_front().unwrap();
            self.unavailable_blocks.push(used_block);
            // 如果还有recyclable_blocks 则指向下一个block的第一个可用line
            self.recyclable_blocks
                .front()
                .and_then(|b| {
                    self.cursor =
                        unsafe { (**b).get_nth_line((**b).find_first_hole().unwrap().0 as usize) };
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

    /// # overflow_alloc
    ///
    /// 从global allocator中获取新block进行分配。
    ///
    /// ## Parameters
    ///
    /// * `size` - object size
    ///
    /// ## Return
    ///
    /// * `*mut u8` - object pointer
    pub fn overflow_alloc(&mut self, size: usize, obj_type: ObjectType) -> *mut u8 {
        // 获取新block
        let new_block = self.get_new_block();
        // alloc
        let (re, nxt) = unsafe {
            (*new_block)
                .alloc(size, (*new_block).get_nth_line(3), obj_type)
                .unwrap()
        };
        nxt.or_else(|| {
            // new_block被用完，将它加入unavailable blocks
            self.unavailable_blocks.push(new_block);
            None
        })
        .and_then(|_| {
            // new_block未被用完，将它加入recyclable blocks
            unsafe {
                debug_assert!((*new_block).find_first_hole().is_some());
            }
            self.recyclable_blocks.push_back(new_block);
            None::<()>
        });
        re
    }
    /// # big_obj_alloc
    ///
    /// 大对象分配
    ///
    /// ## Parameters
    ///
    /// * `size` - object size
    ///
    /// ## Return
    ///
    /// * `*mut u8` - object pointer
    pub fn big_obj_alloc(&mut self, size: usize) -> *mut u8 {
        unsafe { (*self.global_allocator).alloc_big_object(size) }
    }

    /// # get_new_block
    ///
    /// get a new block from global allocator.
    ///
    /// ## Return
    ///
    /// * `*mut Block` - block pointer
    fn get_new_block(&mut self) -> *mut Block {
        if self.collect_mode && self.eva_blocks.len() > 0 {
            return self.eva_blocks.pop().unwrap();
        }
        let block = unsafe { (&mut *self.global_allocator).get_block() };
        block
    }

    /// # in_heap
    pub fn in_heap(&self, ptr: *mut u8) -> bool {
        unsafe { (*self.global_allocator).in_heap(ptr) }
    }

    /// # sweep
    ///
    /// Iterate all blocks, if a block is not marked, free it.
    /// Correct all remain blocks' headers, and classify them
    /// into recyclable blocks and unavailable blocks.
    pub fn sweep(&mut self, mark_histogram: *mut VecMap<usize, usize>) {
        let mut recyclable_blocks = VecDeque::new();
        let mut unavailable_blocks = Vec::new();
        let mut free_blocks = Vec::new();
        let mut cursor = std::ptr::null_mut::<u8>();
        unsafe {
            for block in self
                .recyclable_blocks
                .iter()
                .chain(self.unavailable_blocks.iter())
            {
                let block = *block;
                if (*block).marked {
                    (*block).correct_header(mark_histogram);
                    let (line, hole) = (*block).get_available_line_num_and_holes();
                    if line > 0 {
                        debug_assert!(
                            (*block).find_first_hole().is_some(),
                            "line {}, hole {}",
                            line,
                            hole
                        );
                        recyclable_blocks.push_back(block);
                        if cursor.is_null() {
                            cursor = (*block)
                                .get_nth_line((*block).find_first_hole().unwrap().0 as usize);
                        }
                    } else {
                        unavailable_blocks.push(block);
                    }
                } else {
                    free_blocks.push(block);
                }
            }
        }
        self.recyclable_blocks = recyclable_blocks;
        self.unavailable_blocks = unavailable_blocks;
        self.cursor = cursor;
        let total_block_num =
            self.recyclable_blocks.len() + self.unavailable_blocks.len() + self.eva_blocks.len();
        let head_room_size = (EVA_BLOCK_PROPORTION * total_block_num as f64) as usize;
        if self.eva_blocks.len() > head_room_size {
            // 把多的加到free_blocks
            let over_flow = self.eva_blocks.len() - head_room_size;
            for _ in 0..over_flow {
                free_blocks.push(self.eva_blocks.pop().unwrap());
            }
        } else {
            // 尝试把少的从free_blocks取出来
            let less = head_room_size - self.eva_blocks.len();
            for _ in 0..less {
                if let Some(block) = free_blocks.pop() {
                    unsafe {
                        (*block).reset_header();
                    }
                    self.eva_blocks.push(block);
                } else {
                    break;
                }
            }
        }
        unsafe {
            (&mut *self.global_allocator).return_blocks(free_blocks.into_iter());
        }
    }
}
