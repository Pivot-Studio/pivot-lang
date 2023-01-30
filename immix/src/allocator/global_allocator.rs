use parking_lot::ReentrantMutex;

use crate::{block::Block, consts::BLOCK_SIZE, mmap::Mmap, bigobj::BigObj};

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
    free_blocks: Vec<*mut Block>,
    /// lock
    lock: ReentrantMutex<()>,
    /// big object mmap
    pub big_obj_allocator: BigObjAllocator,
}

unsafe impl Sync for GlobalAllocator {}

impl GlobalAllocator {
    /// Create a new global allocator.
    ///
    /// size is the max heap size
    pub fn new(size: usize) -> Self {
        let mmap = Mmap::new(size);
        Self {
            current: mmap.aligned(BLOCK_SIZE),
            heap_start: mmap.aligned(BLOCK_SIZE),
            heap_end: mmap.end(),
            mmap,
            free_blocks: Vec::new(),
            lock: ReentrantMutex::new(()),
            big_obj_allocator: BigObjAllocator::new(size),
        }
    }
    /// 从big object mmap中分配一个大对象，大小为size
    pub fn get_big_obj(&mut self, size: usize) -> *mut BigObj {
        self.big_obj_allocator.get_chunk(size)
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
        let mut blocks = Vec::with_capacity(n);
        for _ in 0..n {
            let block = if let Some(block) = self.free_blocks.pop() {
                self.mmap.commit(block as *mut u8, BLOCK_SIZE);
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

    /// # in_big_heap
    /// 判断一个指针是否是一个大对象
    pub fn in_big_heap(&self, ptr: *mut u8) -> bool {
        self.big_obj_allocator.in_heap(ptr)
    }
}
