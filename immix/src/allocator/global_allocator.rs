use std::{cell::RefCell, cmp::{min_by, max_by}};

use parking_lot::ReentrantMutex;

use crate::{bigobj::BigObj, block::Block, consts::BLOCK_SIZE, mmap::Mmap};

use super::big_obj_allocator::BigObjAllocator;

/// # Global allocator
///
/// Only allocate blocks, shared between threads, need synchronization.
pub struct GlobalAllocator {
    /// k=mmap.start, v=mmap
    mmaps: RefCell<Vec<Mmap>>,
    /// current mmap index
    mmap_index: usize,
    /// current heap pointer
    current: *mut u8,
    /// current mmap size
    heap_size: usize,
    min_mmap_start: *mut u8,
    max_mmap_end: *mut u8,
    /// 所有被归还的Block都会被放到这个Vec里面
    free_blocks: Vec<(*mut Block, bool, usize)>,
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
}

unsafe impl Sync for GlobalAllocator {}

const ROUND_THRESHOLD: i64 = 3;

const ROUND_MIN_TIME_MILLIS: u128 = 1000;

impl GlobalAllocator {
    /// Create a new global allocator.
    ///
    /// size is the max heap size
    pub fn new(size: usize) -> Self {
        let mmap = Mmap::new(size);
        // println!("mmap start: {:p}, end: {:p}", mmap.start(), mmap.end());
        let start = mmap.aligned(BLOCK_SIZE);
        let end = mmap.end();
        // mmap.commit(mmap.aligned(), BLOCK_SIZE);

        Self {
            current: start,
            mmaps: RefCell::new(vec![mmap]),
            mmap_index: 0,
            heap_size: size,
            min_mmap_start: start,
            max_mmap_end: end,
            free_blocks: Vec::new(),
            lock: ReentrantMutex::new(()),
            big_obj_allocator: BigObjAllocator::new(size),
            last_get_block_time: std::time::Instant::now(),
            mem_usage_flag: 0,
            round: 0,
        }
    }
    /// 从big object mmap中分配一个大对象，大小为size
    pub fn get_big_obj(&mut self, size: usize) -> *mut BigObj {
        self.big_obj_allocator.get_chunk(size)
    }

    pub fn unmap_all(&mut self) {
        let _lock = self.lock.lock();
        self.free_blocks.iter_mut().for_each(|(block, freed, mmap_index)| {
            if *freed {
                return;
            }
            // 根据block的地址获取mmap的index
            let mmap = &self.mmaps.borrow()[*mmap_index];
            mmap.dontneed(*block as *mut u8, BLOCK_SIZE);
            *freed = true;
        });
    }
    pub fn heap_size(&self) -> usize {
        self.heap_size
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
    /// 扩展堆接口
    pub fn expand_heap(&mut self, size: usize) {
        let _lock = self.lock.lock();
        let mmap = Mmap::new(size);
        let start = mmap.aligned(BLOCK_SIZE);
        let end = mmap.end();
        self.mmaps.borrow_mut().push(mmap);
        self.heap_size += size;
        self.min_mmap_start = min_by(self.min_mmap_start, start, |a, b| a.cmp(b));
        self.max_mmap_end = max_by(self.max_mmap_end, end, |a, b| a.cmp(b));
        self.current = start;
        self.mmap_index += 1;
    }

    /// 从mmap的heap空间之中获取一个Option<* mut Block>，如果heap空间不够了，返回None
    ///
    /// 每次分配block会让current增加一个block的大小
    fn alloc_block(&self) -> Option<*mut Block> {
        let current = self.current;
        let mmap = &self.mmaps.borrow()[self.mmap_index];
        let heap_end = mmap.end();

        if current > unsafe { heap_end.sub(BLOCK_SIZE) } {
            return None;
        }
        mmap.commit(current, BLOCK_SIZE);
        // if unsafe { current.add(BLOCK_SIZE * 32) } >= heap_end {
        //     return None;
        // }
        // if (self.current as usize - self.heap_start as usize) / BLOCK_SIZE % 32 == 0 {
        //     self.mmap.commit(current, BLOCK_SIZE * 32);
        // }

        let block = Block::new(current, self.mmap_index);

        Some(block)
    }

    // pub fn out_of_space(&self) ->bool {
    //     (self.heap_end as usize - self.current as usize) < BLOCK_SIZE && self.free_blocks.len() == 0
    // }

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
        let block = if let Some((block, freed, mmap_index)) = self.free_blocks.pop() {
            if freed {
                let mmap = &self.mmaps.borrow()[mmap_index];
                mmap.commit(block as *mut u8, BLOCK_SIZE);
            }
            block
        } else {
            let b = self.alloc_block().unwrap_or(std::ptr::null_mut());
            self.current = unsafe { self.current.add(BLOCK_SIZE) };
            b
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
        // 距离上次alloc时间超过1秒，把free_blocks中的block都dont need
        if now.duration_since(self.last_get_block_time).as_millis() > ROUND_MIN_TIME_MILLIS {
            self.last_get_block_time = now;
            self.mem_usage_flag = 0;
            if self.mem_usage_flag > 0 {
                self.round += 1;
            } else if self.mem_usage_flag <= 0 {
                self.round -= 1;
            }
            if self.round <= -ROUND_THRESHOLD {
                // 总体有三个周期处于内存不变或减少状态，进行dontneed
                self.round = 0;
                // println!("trigger dont need");
                self.free_blocks
                    .iter_mut()
                    .filter(|(_, free, _)| !*free)
                    .for_each(|(block, freed, mmap_index)| {
                        if !*freed {
                            let mmap = &self.mmaps.borrow()[*mmap_index];
                            mmap.dontneed(*block as *mut u8, BLOCK_SIZE);
                            *freed = true;
                        }
                    });
            } else if self.round >= ROUND_THRESHOLD {
                self.round = 0;
            }
        }
        block
    }

    /// # return_blocks
    ///
    /// blocks是into iterator
    ///
    /// 将blocks放回free_blocks中，所有放回的空间会被标记为DONT_NEED
    pub fn return_blocks<I>(&mut self, blocks: I)
    where
        I: IntoIterator<Item = *mut Block>,
    {
        let _lock = self.lock.lock();
        for block in blocks {
            self.mem_usage_flag -= 1;
            let mmap_index = unsafe { (*block).mmap_index };
            self.free_blocks.push((block, false, mmap_index));
            // self.mmap
            //     .dontneed(block as *mut Block as *mut u8, BLOCK_SIZE);
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
        let ptr = ptr as usize;
        let min = self.min_mmap_start as usize;
        let max = self.max_mmap_end as usize;
        ptr >= min && ptr <= max
    }

    /// # in_big_heap
    /// 判断一个指针是否是一个大对象
    pub fn in_big_heap(&self, ptr: *mut u8) -> bool {
        self.big_obj_allocator.in_heap(ptr)
    }
}
