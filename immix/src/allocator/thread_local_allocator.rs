//! # Thread-local allocator
//!
//! This module contains the thread-local allocator, which is associated with each thread.
//!
//! Thread-local allocator get empty blocks from global allocator, and allocate objects from these blocks.
//!
//! When a thread-local allocator is dropped, it will return all blocks to global allocator.

use std::collections::VecDeque;

use rustc_hash::FxHashMap;

use crate::{
    bigobj::BigObj,
    block::{Block, ObjectType},
    consts::{BLOCK_SIZE, LINE_SIZE},
    HeaderExt, EVA_BLOCK_PROPORTION, NUM_LINES_PER_BLOCK,
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
    big_objs: Vec<*mut BigObj>,
    eva_blocks: Vec<*mut Block>,
    collect_mode: bool,
}

impl Drop for ThreadLocalAllocator {
    fn drop(&mut self) {
        let global_allocator = unsafe { &mut *self.global_allocator };
        // here we should return all blocks to global allocator
        // however, when a thread exits, it may still hold some non-empty blocks
        // those blocks shall be stored and give to another thread latter
        // eprintln!("drop thread local allocator {} eva blocks {} un blocks {} re blocks", self.eva_blocks.len(), self.unavailable_blocks.len(), self.recyclable_blocks.len());
        global_allocator.on_thread_destroy(
            &self.eva_blocks,
            self.recyclable_blocks.drain(..),
            &self.unavailable_blocks,
        );
    }
}

impl ThreadLocalAllocator {
    // pub fn verify(&self) {
    //     self.recyclable_blocks.iter().chain(self.unavailable_blocks.iter()).for_each(|b| unsafe{
    //         assert!(b.as_ref().unwrap().cursor<300);
    //     })
    // }
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
            eva_blocks: Vec::new(),
            big_objs: Vec::new(),
            collect_mode: false,
        }
    }

    // pub fn has_emergency(&self) -> bool {
    //     unsafe{(*self.global_allocator).out_of_space() && self.recyclable_blocks.len() == 0 }
    // }

    pub fn set_collect_mode(&mut self, collect_mode: bool) {
        self.collect_mode = collect_mode;
    }

    pub fn get_more_works(&mut self) {
        unsafe {
            self.global_allocator
                .as_mut()
                .unwrap()
                .get_returned_blocks(&mut self.recyclable_blocks, &mut self.unavailable_blocks);
        }
    }
    pub fn print_stats(&self) {
        println!("unavailable blocks: {}", self.unavailable_blocks.len());
        for block in &self.unavailable_blocks {
            unsafe {
                (**block).show();
            }
        }
        println!("recyclable blocks: {}", self.recyclable_blocks.len());
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
        #[cfg(debug_assertions)]
        {
            true
        }
        #[cfg(not(debug_assertions))]
        {
            self.recyclable_blocks.len() > 1
        }
    }

    pub fn fill_available_histogram(&self, histogram: &mut FxHashMap<usize, usize>) -> usize {
        let mut total_available = 0;
        self.recyclable_blocks
            .iter()
            .chain(self.unavailable_blocks.iter())
            .for_each(|block| unsafe {
                let (available, holes) = (**block).get_available_line_num_and_holes();
                if let Some(v) = histogram.get_mut(&holes) {
                    *v += available;
                } else {
                    histogram.insert(holes, available);
                }
                total_available += available;
            });
        total_available
    }

    pub fn set_eva_threshold(&mut self, _threshold: usize) {
        self.recyclable_blocks
            .iter()
            .chain(self.unavailable_blocks.iter())
            .for_each(|block| unsafe {
                (**block).set_eva_threshold({
                    #[cfg(not(debug_assertions))]
                    {
                        _threshold
                    }
                    #[cfg(debug_assertions)]
                    {
                        0
                    }
                })
            });
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

    pub fn get_bigobjs_size(&self) -> usize {
        let mut size = 0;
        for bigobj in &self.big_objs {
            size += unsafe { (**bigobj).get_size() };
        }
        size
    }

    pub fn should_gc(&self) -> bool {
        unsafe { self.recyclable_blocks.is_empty() && (*self.global_allocator).should_gc() }
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
            return self.big_obj_alloc(size, obj_type);
        }
        // mid size object & small size object
        // 刚启动或者recycle block全用光了
        if self.recyclable_blocks.is_empty() {
            let block = self.get_new_block();
            if block.is_null() {
                return std::ptr::null_mut();
            }
            unsafe {
                let (s, nxt) = (*block).alloc(size, obj_type).unwrap();
                let re = (*block).get_nth_line(s);
                if !nxt {
                    self.unavailable_blocks.push(block);
                } else {
                    self.recyclable_blocks.push_back(block);
                }
                return re;
            }
        }
        let mut f = self.recyclable_blocks.front().unwrap();
        unsafe {
            while (**f).is_eva_candidate() {
                let uf = self.recyclable_blocks.pop_front().unwrap();
                self.unavailable_blocks.push(uf);
                let ff = self.recyclable_blocks.front();
                if let Some(ff) = ff {
                    f = ff;
                } else {
                    return self.alloc(size, obj_type);
                }
            }
        }
        let res = unsafe { (**f).alloc(size, obj_type) };
        // return std::ptr::null_mut();
        if res.is_none() {
            // if size <= LINE_SIZE {
            //     unsafe{ (**f).show();}
            //     panic!("mid size object alloc failed");
            // }
            debug_assert!(size > LINE_SIZE);
            // mid size object alloc failed, try to overflow_alloc
            return self.overflow_alloc(size, obj_type);
        }
        let (s, nxt) = res.unwrap();
        let re = unsafe { (**f).get_nth_line(s) };
        if !nxt {
            // 当前block被用完，将它从recyclable blocks中移除，加入unavailable blocks
            let used_block = self.recyclable_blocks.pop_front().unwrap();
            self.unavailable_blocks.push(used_block);
        }
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
        if new_block.is_null() {
            return std::ptr::null_mut();
        }
        // alloc
        let (s, nxt) = unsafe { (*new_block).alloc(size, obj_type).unwrap() };
        let re = unsafe { (*new_block).get_nth_line(s) };
        if !nxt {
            // new_block被用完，将它加入unavailable blocks
            self.unavailable_blocks.push(new_block);
        } else {
            // new_block未被用完，将它加入recyclable blocks
            // unsafe {
            //     debug_assert!((*new_block).find_first_hole().is_some());
            // }
            self.recyclable_blocks.push_back(new_block);
        }
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
    pub fn big_obj_alloc(&mut self, size: usize, obj_type: ObjectType) -> *mut u8 {
        let obj = unsafe { (*self.global_allocator).get_big_obj(size) };
        unsafe { (*obj).header.set_obj_type(obj_type) };
        self.big_objs.push(obj);
        unsafe { (obj as *mut u8).add(16) }
    }

    pub fn big_obj_from_ptr(&mut self, ptr: *mut u8) -> Option<*mut BigObj> {
        for obj in self.big_objs.iter() {
            // FIXME: O(n); should use a tree
            let start = unsafe { (*obj as *mut u8).add(16) };
            let end = unsafe { (*obj as *mut u8).add((*(*obj)).size) };
            if start <= ptr && end >= ptr {
                return Some(*obj);
            }
        }
        None
    }

    /// # get_new_block
    ///
    /// get a new block from global allocator.
    ///
    /// ## Return
    ///
    /// * `*mut Block` - block pointer
    fn get_new_block(&mut self) -> *mut Block {
        let b = if self.collect_mode && !self.eva_blocks.is_empty() {
            self.eva_blocks.pop().unwrap()
        } else {
            unsafe { (*self.global_allocator).get_block() }
        };

        unsafe { b.as_mut().map(|b| b.set_eva_threshold(NUM_LINES_PER_BLOCK)) };
        b
    }

    /// # in_heap
    pub fn in_heap(&self, ptr: *mut u8) -> bool {
        unsafe { (*self.global_allocator).in_heap(ptr) }
    }

    /// # in_big_heap
    pub fn in_big_heap(&self, ptr: *mut u8) -> bool {
        unsafe { (*self.global_allocator).in_big_heap(ptr) }
    }

    /// # sweep
    ///
    /// Iterate all blocks, if a block is not marked, free it.
    /// Correct all remain blocks' headers, and classify them
    /// into recyclable blocks and unavailable blocks.
    pub fn sweep(&mut self, mark_histogram: *mut FxHashMap<usize, usize>) -> (usize, usize) {
        let mut recyclable_blocks = VecDeque::new();
        let mut unavailable_blocks = Vec::new();
        let mut free_blocks = Vec::new();
        let mut total_used = 0;
        let mut free_lines = 0;
        unsafe {
            for block in self
                .recyclable_blocks
                .iter()
                .chain(self.unavailable_blocks.iter())
            {
                let block = *block;
                if (*block).marked {
                    let (delta_u, delta_f) = (*block).correct_header(mark_histogram);
                    total_used += delta_u;
                    free_lines += delta_f;
                    let (line, _) = (*block).get_available_line_num_and_holes();
                    if line > 0 {
                        // debug_assert!(
                        //     (*block).find_first_hole().is_some(),
                        //     "line {}, hole {}",
                        //     line,
                        //     hole
                        // );
                        recyclable_blocks.push_back(block);
                    } else {
                        unavailable_blocks.push(block);
                    }
                } else {
                    block.as_mut().unwrap().reset_header();
                    free_blocks.push(block);
                }
            }
        }
        self.recyclable_blocks = recyclable_blocks;
        self.unavailable_blocks = unavailable_blocks;
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
                    self.eva_blocks.push(block);
                } else {
                    break;
                }
            }
        }
        unsafe {
            (*self.global_allocator).return_blocks(free_blocks);
        }
        let mut big_objs = Vec::new();
        for obj in self.big_objs.iter() {
            if unsafe { (*(*obj)).header.get_marked() } {
                big_objs.push(*obj);
            } else {
                unsafe {
                    (*self.global_allocator).return_big_objs([*obj]);
                }
            }
            unsafe {
                (*(*obj)).header &= !0b10;
            }
        }
        self.big_objs = big_objs;
        let used_lines = total_used * LINE_SIZE;
        (used_lines, free_lines * LINE_SIZE)
    }
}
