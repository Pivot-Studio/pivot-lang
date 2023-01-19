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
    free_blocks: Vec<*mut Block>,
    /// lock
    lock: ReentrantMutex<()>,
    /// big object mmap
    big_object_mmap: BigObjectMmap,
}

unsafe impl Sync for GlobalAllocator {}

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
            free_blocks: Vec::new(),
            lock: ReentrantMutex::new(()),
            big_object_mmap: BigObjectMmap::new(size),
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

        let block = Block::new(current);

        Some(block)
    }

    /// # get_block
    ///
    /// 从free_blocks中获取一个可用的block，如果没有可用的block，就从mmap的heap空间之中获取一个新block
    pub fn get_block(&mut self) -> *mut Block {
        let _lock = self.lock.lock();
        let block = if let Some(block) = self.free_blocks.pop() {
            block
        } else {
            let b = self
                .alloc_block()
                .expect("global allocator is out of memory!");
            self.current = unsafe { self.current.add(BLOCK_SIZE) };
            self.mmap.commit(b as *mut Block as *mut u8, BLOCK_SIZE);
            b
        };
        unsafe {
            core::ptr::write_bytes(block as *mut u8, 0, BLOCK_SIZE);
            (*block).reset_header();
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
            self.free_blocks.push(block);
            self.mmap
                .dontneed(block as *mut Block as *mut u8, BLOCK_SIZE);
        }
    }

    /// # in heap
    /// 判断一个指针是否在heap之中
    pub fn in_heap(&self, ptr: *mut u8) -> bool {
        ptr >= self.heap_start && ptr < self.heap_end
    }
}
