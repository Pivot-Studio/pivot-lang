use parking_lot::ReentrantMutex;

use crate::{block::Block, consts::BLOCK_SIZE, mmap::Mmap};

/// # Global allocator
///
/// Only allocate blocks, shared between threads, need synchronization.
pub struct GlobalAllocator {
    /// mmap region
    mmap: Mmap,
    /// current heap pointer
    current: *mut u8,
    /// heap end
    heap_end: *mut u8,
    /// 所有被归还的Block都会被放到这个Vec里面
    free_blocks: Vec<*mut Block>,
    /// lock
    lock: ReentrantMutex<()>,
}

impl GlobalAllocator {
    /// Create a new global allocator.
    ///
    /// size is the max heap size
    pub fn new(size: usize) -> Self {
        let mmap = Mmap::new(size);
        Self {
            current: mmap.aligned(),
            heap_end: mmap.end(),
            mmap,
            free_blocks: Vec::new(),
            lock: ReentrantMutex::new(()),
        }
    }
    /// 从mmap的heap空间之中获取一个Option<* mut Block>，如果heap空间不够了，就返回None
    ///
    /// 每次分配block会让current增加一个block的大小
    fn alloc_block(&mut self) -> Option<*mut Block> {
        let current = self.current;
        let heap_end = self.heap_end;

        if current >= heap_end {
            return None;
        }

        self.mmap.commit(current, BLOCK_SIZE);

        let block = Block::new(current);

        let current = self.current;
        let next = unsafe { current.add(BLOCK_SIZE) };
        self.current = next;

        Some(block)
    }

    /// # get_block
    ///
    /// 从free_blocks中获取一个可用的block，如果没有可用的block，就从mmap的heap空间之中获取一个新block
    pub fn get_block(&mut self) -> *mut Block {
        _ = self.lock.lock();
        let block = if let Some(block) = self.free_blocks.pop() {
            block
        } else {
            let b = self.alloc_block().unwrap();
            self.mmap.commit(b as *mut Block as *mut u8, BLOCK_SIZE);
            b
        };
        block
    }

    /// # return_blocks
    ///
    /// blocks是into iterator
    ///
    /// 将blocks放回free_blocks中，所有放回的空间会被标记为DONT_NEED
    pub fn return_blocks<I>(&mut self, blocks: I)
    where
        I: IntoIterator<Item = &'static mut Block>,
    {
        _ = self.lock.lock();
        for block in blocks {
            self.free_blocks.push(block);
            self.mmap
                .dontneed(block as *mut Block as *mut u8, BLOCK_SIZE);
        }
    }
}
