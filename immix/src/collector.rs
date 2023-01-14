use std::collections::VecDeque;

use rustc_hash::FxHashMap;

use crate::{allocator::ThreadLocalAllocator, block::Block, consts::LINE_SIZE};

/// # Collector
/// The collector is responsible for collecting garbage. It is the entry point for
/// the garbage collection process. It is also responsible for allocating new
/// blocks and for allocating new objects.
///
/// One thread has a collector associated with it. The collector is thread-local.
///
/// ## Fields
/// * `thread_local_allocator` - thread-local allocator
/// * `roots` - gc roots
/// * `queue` - gc queue
pub struct Collector {
    thread_local_allocator: ThreadLocalAllocator,
    roots: FxHashMap<usize, usize>,
    queue: VecDeque<*mut u8>,
}

impl Collector {
    /// # new
    /// Create a new collector.
    ///
    /// ## Parameters
    /// * `thread_local_allocator` - thread-local allocator
    pub fn new(thread_local_allocator: ThreadLocalAllocator) -> Self {
        Self {
            thread_local_allocator,
            roots: FxHashMap::default(),
            queue: VecDeque::new(),
        }
    }

    /// # get_size
    ///
    /// Get the size of allocated space.
    ///
    /// ## Return
    ///
    /// * `usize` - size
    pub fn get_size(&self) -> usize {
        self.thread_local_allocator.get_size()
    }
    /// # alloc
    ///
    /// Allocate a new object.
    ///
    /// ## Parameters
    /// * `size` - object size
    ///
    /// ## Return
    /// * `ptr` - object pointer
    pub fn alloc(&mut self, size: usize) -> *mut u8 {
        let ptr = self.thread_local_allocator.alloc(size);
        ptr
    }

    /// # add_root
    /// Add a root to the collector.
    ///
    /// ## Parameters
    /// * `root` - root
    /// * `size` - root size
    pub fn add_root(&mut self, root: *mut u8, size: usize) {
        self.roots.insert(root as usize, size);
    }

    /// # remove_root
    /// Remove a root from the collector.
    ///
    /// ## Parameters
    /// * `root` - root
    pub fn remove_root(&mut self, root: *mut u8) {
        self.roots.remove(&(root as usize));
    }

    /// # mark
    /// From gc roots, mark all reachable objects.
    ///
    /// this mark function is not precise, it will iterate through
    /// address space, and try to treat every address as a pointer
    pub fn mark(&mut self) {
        for (root, size) in self.roots.iter() {
            let root = *root as *mut *mut u8;
            // size is of bytes, so we need to divide it by 8
            unsafe {
                for i in 0..size / 8 {
                    let ptr = root.offset(i as isize);
                    let obj = *ptr;
                    if self.thread_local_allocator.in_heap(obj) {
                        self.queue.push_back(obj);
                    }
                }
            }
        }

        // iterate through queue and mark all reachable objects
        // get object size from object header, which is stored in the block header
        // the block can be constructed from object pointer, using Block::from_obj_ptr
        while let Some(obj) = self.queue.pop_front() {
            unsafe {
                let block = Block::from_obj_ptr(obj);
                block.marked = true;
                let (idx, size) = block.get_obj_line_size_from_ptr(obj);
                // idx and size are in lines, to get actual byte size, we need to multiply it by LINE_SIZE
                for i in idx..idx + size {
                    block.set_line_mark(i);
                }
                // iterate through object and push all heap pointers into queue
                for i in 0..size * LINE_SIZE / 8 {
                    let ptr = obj.offset(i as isize) as *mut *mut u8;
                    let obj = *ptr;
                    if self.thread_local_allocator.in_heap(obj) {
                        self.queue.push_back(obj);
                    }
                }
            }
        }
    }

    /// # sweep
    pub fn sweep(&mut self) {
        self.thread_local_allocator.sweep();
    }

    /// # collect
    /// Collect garbage.
    pub fn collect(&mut self) {
        self.mark();
        self.sweep();
    }
}

#[cfg(test)]
mod tests {
    use crate::allocator::GlobalAllocator;

    use super::*;
    unsafe fn set_point_to(ptr1: *mut u8, ptr2: *mut u8, offset: i64) {
        (ptr1 as *mut *mut u8).offset(offset as isize).write(ptr2);
    }
    #[test]
    fn test_basic_gc() {
        unsafe {
            let mut ga = GlobalAllocator::new(1024 * 1024 * 1024);
            let tla = ThreadLocalAllocator::new(&mut ga);
            let mut gc = Collector::new(tla);
            // gc.about();
            // allocate 4 pointers
            let mut ptr1 = gc.alloc(64);
            let ptr2 = gc.alloc(64);
            let _ = gc.alloc(64);
            let _ = gc.alloc(64);
            // get rust stack pointer point to ptr1
            let rustptr = (&mut ptr1) as *mut *mut u8 as *mut u8;
            // println!("1");
            assert_eq!(gc.get_size(), 4);
            gc.add_root(rustptr, 8);
            // set ptr1 point to ptr2
            set_point_to(ptr1, ptr2, 0);
            gc.collect();
            // println!("2");
            assert_eq!(gc.get_size(), 2);
            // set ptr1 empty
            *(ptr1 as *mut i64) = 0;
            gc.collect();
            // println!("3");
            assert_eq!(gc.get_size(), 1);
            // remove gc root
            gc.remove_root(rustptr);
            gc.collect();
            // println!("4");
            assert_eq!(gc.get_size(), 0);
            print!("{} {}", ptr1 as usize, ptr2 as usize)
            // assert_eq!(gc.get_size(), 0);
        }
    }
}
