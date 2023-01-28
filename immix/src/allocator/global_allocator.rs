use std::collections::VecDeque;

use parking_lot::ReentrantMutex;

use crate::{block::Block, consts::BLOCK_SIZE, mmap::Mmap};

/// # Big object mmap
///
/// Big object mmap, used to allocate big objects.
pub struct BigObjectMmap {
    /// mmap region
    mmap: Mmap,
    /// current heap pointer
    current: *mut u8,
    /// heap start
    heap_start: *mut u8,
    /// heap end
    heap_end: *mut u8,
    /// lock
    lock: ReentrantMutex<()>,
}

impl BigObjectMmap {
    pub fn new(size: usize) -> Self {
        let mmap = Mmap::new(size);
        Self {
            current: mmap.aligned(),
            heap_start: mmap.aligned(),
            heap_end: mmap.end(),
            mmap,
            lock: ReentrantMutex::new(()),
        }
    }

    pub fn state(&self) {
        println!("current: {:p}", self.current);
        println!("heap_start: {:p}", self.heap_start);
        println!("heap_end: {:p}", self.heap_end);
    }

    /// # Alloc big object
    ///
    /// Alloc big object from big object mmap.
    ///
    /// Return a pointer to the allocated memory.
    pub fn alloc(&mut self, size: usize) -> *mut u8 {
        let _lock = self.lock.lock();
        let current = self.current;
        let heap_end = self.heap_end;

        if unsafe { current.add(size) } >= heap_end {
            panic!("big object mmap out of memory");
        }

        self.mmap.commit(current, size);

        self.current = unsafe { current.add(size) };

        current
    }
    /// # Realloc big object
    ///
    /// Realloc big object from big object mmap.
    pub fn realloc(&mut self, ptr: *mut u8, old_size: usize, new_size: usize) -> *mut u8 {
        let _lock = self.lock.lock();
        let current = self.current;
        let heap_end = self.heap_end;

        if unsafe { current.add(new_size) } >= heap_end {
            panic!("big object mmap out of memory");
        }

        self.mmap.commit(current, new_size);

        self.current = unsafe { current.add(new_size) };

        unsafe {
            std::ptr::copy_nonoverlapping(ptr, current, old_size);
        }

        self.mmap.dontneed(ptr, old_size);

        current
    }

    /// # Free big object
    ///
    /// Free big object from big object mmap.
    pub fn free(&mut self, ptr: *mut u8, size: usize) {
        let _lock = self.lock.lock();
        self.mmap.dontneed(ptr, size);
    }
}

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
    free_blocks: VecDeque<(*mut Block, bool)>,
    /// lock
    lock: ReentrantMutex<()>,
    /// big object mmap
    big_object_mmap: BigObjectMmap,

    last_get_block_time: std::time::Instant,

    /// 一个周期内alloc的block数减去return的block数
    /// 大于0表示内存使用量处于增加状态，小于0表示内存使用量处于减少状态
    mem_usage_flag: i64,

    /// 周期计数器，每个内存总体增加周期加1，每个内存总体不变/减少周期减1
    round: i64,
}

unsafe impl Sync for GlobalAllocator {}

const ROUND_THRESHOLD: i64 = 3;

const ROUND_MIN_TIME_MILLIS: u128 = 300;

impl GlobalAllocator {
    /// Create a new global allocator.
    ///
    /// size is the max heap size
    pub fn new(size: usize) -> Self {
        let mmap = Mmap::new(size);
        Self {
            current: mmap.aligned(),
            heap_start: mmap.aligned(),
            heap_end: mmap.end(),
            mmap,
            free_blocks: VecDeque::new(),
            lock: ReentrantMutex::new(()),
            big_object_mmap: BigObjectMmap::new(size),
            last_get_block_time: std::time::Instant::now(),
            mem_usage_flag: 0,
            round: 0,
        }
    }
    /// 从big object mmap中分配一个大对象，大小为size
    pub fn alloc_big_object(&mut self, size: usize) -> *mut u8 {
        self.big_object_mmap.alloc(size)
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

        let block = Block::new(current);

        Some(block)
    }

    /// # get_block
    ///
    /// 从free_blocks中获取一个可用的block，如果没有可用的block，就从mmap的heap空间之中获取一个新block
    pub fn get_block(&mut self) -> *mut Block {
        self.get_blocks(1)[0]
    }

    /// # get_blocks
    ///
    /// 从free_blocks中获取n个可用的block，如果没有可用的block，就从mmap的heap空间之中获取n个新block
    pub fn get_blocks(&mut self, n: usize) -> Vec<*mut Block> {
        let _lock = self.lock.lock();
        self.mem_usage_flag += n as i64;
        let mut blocks = Vec::with_capacity(n);
        for _ in 0..n {
            let block = if let Some((block, freed)) = self.free_blocks.pop_front() {
                if freed {
                    self.mmap.commit(block as *mut u8, BLOCK_SIZE);
                }
                block
            } else {
                let b = self
                    .alloc_block()
                    .expect("global allocator is out of memory!");
                self.current = unsafe { self.current.add(BLOCK_SIZE) };
                b
            };
            unsafe {
                core::ptr::write_bytes(block as *mut u8, 0, BLOCK_SIZE);
                (*block).reset_header();
            }
            blocks.push(block);
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
        blocks
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
            self.free_blocks.push_back((block, false));
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
}
