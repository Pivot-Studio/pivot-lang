use parking_lot::ReentrantMutex;

use crate::{bigobj::BigObj, block::Block, consts::BLOCK_SIZE, mmap::Mmap};

use super::big_obj_allocator::BigObjAllocator;

/// # Global allocator
///
/// Only allocate blocks, shared between threads, need synchronization.
pub struct GlobalAllocator {
    /// mmap region
    mmap: Mmap,
    /// current heap pointer
    current: *mut u8,
    /// heap start
    heap_start: *mut u8,
    /// heap end
    heap_end: *mut u8,
    /// 所有被归还的Block都会被放到这个Vec里面
    free_blocks: Vec<(*mut Block, bool)>,
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

        // mmap.commit(mmap.aligned(), BLOCK_SIZE);

        Self {
            current: mmap.aligned(BLOCK_SIZE),
            heap_start: mmap.aligned(BLOCK_SIZE),
            heap_end: mmap.end(),
            mmap,
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
        self.free_blocks.iter_mut().for_each(|(block, freed)| {
            if *freed {
                return;
            }
            self.mmap.dontneed(*block as *mut u8, BLOCK_SIZE);
            *freed = true;
        });
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
    /// 从mmap的heap空间之中获取一个Option<* mut Block>，如果heap空间不够了，就返回None
    ///
    /// 每次分配block会让current增加一个block的大小
    fn alloc_block(&self) -> Option<*mut Block> {
        let current = self.current;
        let heap_end = self.heap_end;

        if current >= heap_end {
            return None;
        }
        self.mmap.commit(current, BLOCK_SIZE);
        // if unsafe { current.add(BLOCK_SIZE * 32) } >= heap_end {
        //     return None;
        // }
        // if (self.current as usize - self.heap_start as usize) / BLOCK_SIZE % 32 == 0 {
        //     self.mmap.commit(current, BLOCK_SIZE * 32);
        // }

        let block = Block::new(current);

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
        let block = if let Some((block, freed)) = self.free_blocks.pop() {
            if freed {
                self.mmap.commit(block as *mut u8, BLOCK_SIZE);
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
            self.free_blocks.push((block, false));
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
        ptr >= self.heap_start && ptr < self.heap_end
    }

    /// # in_big_heap
    /// 判断一个指针是否是一个大对象
    pub fn in_big_heap(&self, ptr: *mut u8) -> bool {
        self.big_obj_allocator.in_heap(ptr)
    }
}
