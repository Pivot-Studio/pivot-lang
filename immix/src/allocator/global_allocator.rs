use std::{cell::Cell, collections::VecDeque};

use parking_lot::ReentrantMutex;
use threadpool::ThreadPool;

use crate::{bigobj::BigObj, block::Block, consts::BLOCK_SIZE, mmap::Mmap, NUM_LINES_PER_BLOCK};

use super::big_obj_allocator::BigObjAllocator;

/// # Global allocator
///
/// Only allocate blocks, shared between threads, need synchronization.
pub struct GlobalAllocator {
    /// mmap region
    mmap: Mmap,
    /// current heap pointer
    current: Cell<*mut u8>,
    /// heap start
    heap_start: *mut u8,
    /// heap end
    heap_end: *mut u8,
    /// 所有被归还的空Block都会被放到这个Vec里面
    free_blocks: Vec<(*mut Block, bool)>,
    /// 线程被销毁时的unavailable_blocks会被暂存到这里，直到下次GC的时候被别的线程
    /// 的GC获取
    unavailable_blocks: Vec<*mut Block>,
    /// 线程被销毁时的urecycle_blocks会被暂存到这里，直到下次GC的时候被别的线程
    /// 的GC获取
    recycle_blocks: Vec<*mut Block>,
    /// lock
    lock: ReentrantMutex<()>,
    /// big object mmap
    pub big_obj_allocator: BigObjAllocator,

    last_get_block_time: std::time::Instant,

    /// 一个周期内alloc的block数减去return的block数
    /// 大于0表示内存使用量处于增加状态，小于0表示内存使用量处于减少状态
    mem_usage_flag: i64,

    /// 周期计数器，每个内存总体增加周期加1，每个内存总体不变/减少周期减1
    round: i64,

    pub pool: ThreadPool,

    /// a bytemap indicates whether a block is in use
    block_bytemap: *mut u8,
}

unsafe impl Sync for GlobalAllocator {}

const ROUND_THRESHOLD: i64 = 10;

const ROUND_MIN_TIME_MILLIS: u128 = 1000;

impl Drop for GlobalAllocator {
    fn drop(&mut self) {
        unsafe { libc::free(self.block_bytemap as _) }
    }
}

impl GlobalAllocator {
    /// Create a new global allocator.
    ///
    /// size is the max heap size
    pub fn new(size: usize) -> Self {
        let mmap = Mmap::new(size * 3 / 4);

        // mmap.commit(mmap.aligned(), BLOCK_SIZE);
        // let n_workers = available_parallelism().unwrap().get();
        let start = mmap.aligned(BLOCK_SIZE);
        let end = mmap.end();

        // bytemap is only for immix space
        let immix_size = end as usize - start as usize;
        // we use one byte per block to avoid using lock
        let bytemap_size = immix_size / BLOCK_SIZE + 1;
        let block_bytemap: *mut u8 = unsafe { libc::malloc(bytemap_size) } as *mut u8;
        Self {
            current: Cell::new(mmap.aligned(BLOCK_SIZE)),
            heap_start: start,
            heap_end: end,
            mmap,
            free_blocks: Vec::new(),
            lock: ReentrantMutex::new(()),
            big_obj_allocator: BigObjAllocator::new(size / 4),
            last_get_block_time: std::time::Instant::now(),
            mem_usage_flag: 0,
            round: 0,
            pool: ThreadPool::default(),
            unavailable_blocks: vec![],
            recycle_blocks: vec![],
            block_bytemap,
        }
    }
    /// 从big object mmap中分配一个大对象，大小为size
    pub fn get_big_obj(&mut self, size: usize) -> *mut BigObj {
        self.big_obj_allocator.get_chunk(size)
    }

    pub fn unmap_all(&mut self) {
        let _lock = self.lock.lock();
        self.free_blocks.iter_mut().for_each(|(block, freed)| {
            if *freed {
                return;
            }
            self.mmap.dontneed(*block as *mut u8, BLOCK_SIZE);
            *freed = true;
        });
    }

    pub fn size(&self) -> usize {
        self.heap_end as usize - self.heap_start as usize
    }

    pub fn return_big_objs<I>(&mut self, objs: I)
    where
        I: IntoIterator<Item = *mut BigObj>,
    {
        let _lock = self.lock.lock();
        for obj in objs {
            self.big_obj_allocator.return_chunk(obj);
        }
    }

    fn set_block_bitmap(&self, block: *mut Block, value: bool) {
        let block_start = block as usize;
        let block_index = (block_start - self.heap_start as usize) / BLOCK_SIZE;
        unsafe {
            self.block_bytemap
                .add(block_index)
                .write(if value { 1 } else { 0 })
        }
    }

    fn get_ptr_bitmap(&self, ptr: *mut u8) -> bool {
        let ptr_start = ptr as usize;
        let block_index = (ptr_start - self.heap_start as usize) / BLOCK_SIZE;
        unsafe { self.block_bytemap.add(block_index).read() == 1 }
    }

    /// 从mmap的heap空间之中获取一个Option<* mut Block>，如果heap空间不够了，就返回None
    ///
    /// 每次分配block会让current增加一个block的大小
    fn alloc_block(&self) -> Option<*mut Block> {
        let current = self.current.get();
        let heap_end = self.heap_end;
        self.current
            .set(unsafe { self.current.get().add(BLOCK_SIZE) });
        if unsafe { current.add(BLOCK_SIZE) } >= heap_end {
            return None;
        }
        if !self.mmap.commit(current, BLOCK_SIZE) {
            self.current
                .set(unsafe { self.current.get().sub(BLOCK_SIZE) });
            return None;
        }

        // if unsafe { current.add(BLOCK_SIZE * 32) } >= heap_end {
        //     return None;
        // }
        // if (self.current as usize - self.heap_start as usize) / BLOCK_SIZE % 32 == 0 {
        //     self.mmap.commit(current, BLOCK_SIZE * 32);
        // }

        let block = Block::new(current);

        Some(block)
    }

    pub fn should_gc(&self) -> bool {
        unsafe {
            let p = self.current.get().add(BLOCK_SIZE * 10);
            p >= self.heap_end && self.free_blocks.is_empty()
        }
    }

    // pub fn out_of_space(&self) ->bool {
    //     (self.heap_end as usize - self.current as usize) < BLOCK_SIZE && self.free_blocks.len() == 0
    // }

    /// # get_returned_blocks
    ///
    /// 获取所有因为线程被销毁而被归还的非空block
    pub fn get_returned_blocks(
        &mut self,
        recycle: &mut VecDeque<*mut Block>,
        unv: &mut Vec<*mut Block>,
    ) {
        let _lock = self.lock.lock();
        recycle.extend(self.recycle_blocks.drain(..).map(|b| unsafe {
            b.as_mut().unwrap().set_eva_threshold(NUM_LINES_PER_BLOCK);
            b
        }));
        unv.extend(self.unavailable_blocks.drain(..).map(|b| unsafe {
            b.as_mut().unwrap().set_eva_threshold(NUM_LINES_PER_BLOCK);
            b
        }));
    }

    /// # get_block
    ///
    /// 从free_blocks中获取一个可用的block，如果没有可用的block，就从mmap的heap空间之中获取一个新block
    pub fn get_block(&mut self) -> *mut Block {
        // let b = self.current as *mut Block;
        // unsafe{
        //     // core::ptr::write_bytes(b as *mut u8, 0, 3*LINE_SIZE);
        //     (*b).reset_header();
        // }
        // return b;
        let _lock = self.lock.lock();
        self.mem_usage_flag += 1;
        let block = if let Some((block, freed)) = self.free_blocks.pop() {
            if freed && !self.mmap.commit(block as *mut u8, BLOCK_SIZE) {
                std::ptr::null_mut()
            } else {
                block
            }
        } else {
            self.alloc_block().unwrap_or(std::ptr::null_mut())
        };
        if block.is_null() {
            return block;
        }
        unsafe {
            #[cfg(feature = "zero_init")]
            core::ptr::write_bytes(block as *mut u8, 0, BLOCK_SIZE);
            (*block).reset_header();
        }
        let now = std::time::Instant::now();
        // 距离上次alloc时间超过1秒，检查是否需要把free_blocks中的block都dont need
        if now.duration_since(self.last_get_block_time).as_millis() > ROUND_MIN_TIME_MILLIS {
            self.last_get_block_time = now;
            if self.mem_usage_flag > 0 {
                self.round += 1;
            } else if self.mem_usage_flag <= 0 {
                self.round -= 1;
            }
            self.mem_usage_flag = 0;
            if self.round <= -ROUND_THRESHOLD {
                // 符合条件，进行dontneed
                self.round = 0;
                // println!("trigger dont need");
                self.free_blocks
                    .iter_mut()
                    .filter(|(_, free)| !*free)
                    .for_each(|(block, freed)| {
                        if !*freed {
                            self.mmap.dontneed(*block as *mut u8, BLOCK_SIZE);
                            *freed = true;
                        }
                    });
            } else if self.round >= ROUND_THRESHOLD {
                self.round = 0;
            }
        }
        self.set_block_bitmap(block, true);
        block
    }

    /// # return_blocks
    ///
    /// blocks是into iterator
    ///
    /// 将blocks放回free_blocks中，所有放回的空间都可能会被dontneed，访问它虽然有时不会报错，
    /// 但是可能会造成Invalid Memory Access
    pub fn return_blocks<I>(&mut self, blocks: I)
    where
        I: IntoIterator<Item = *mut Block>,
    {
        let _lock = self.lock.lock();
        for block in blocks {
            self.mem_usage_flag -= 1;
            self.set_block_bitmap(block, false);
            self.free_blocks.push((block, false));
            // self.mmap
            //     .dontneed(block as *mut Block as *mut u8, BLOCK_SIZE);
        }
    }

    pub fn on_thread_destroy<I>(&mut self, eva: &[*mut Block], recycle: I, unv: &[*mut Block])
    where
        I: IntoIterator<Item = *mut Block>,
    {
        let _lock = self.lock.lock();
        self.unavailable_blocks.extend_from_slice(unv);
        self.recycle_blocks.extend(recycle);
        for block in eva {
            self.mem_usage_flag -= 1;
            self.set_block_bitmap(*block, false);
            self.free_blocks.push((*block, false));
        }
    }

    // pub fn gc_end(&mut self) {
    //     let _lock = self.lock.lock();
    //     self.free_blocks.iter().for_each(|block| {
    //         self.mmap
    //             .dontneed(*block as *mut Block as *mut u8, BLOCK_SIZE);
    //     });
    // }

    /// # in heap
    /// 判断一个指针是否在heap之中
    pub fn in_heap(&self, ptr: *mut u8) -> bool {
        ptr >= self.heap_start && ptr < self.current.get() && self.get_ptr_bitmap(ptr)
    }

    /// # in_big_heap
    /// 判断一个指针是否是一个大对象
    pub fn in_big_heap(&self, ptr: *mut u8) -> bool {
        self.big_obj_allocator.in_heap(ptr)
    }
}
