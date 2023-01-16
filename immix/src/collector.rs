use std::{collections::VecDeque, marker::PhantomData};

use rustc_hash::FxHashMap;

use crate::{
    allocator::ThreadLocalAllocator,
    block::{Block, LineHeaderExt, ObjectType},
};

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
    roots: FxHashMap<usize, ObjectType>,
    queue: *mut VecDeque<*mut u8>,
}

pub type VisitFunc = unsafe fn(&Collector, *mut u8);

pub type VtableFunc = fn(*mut u8, &Collector, VisitFunc, VisitFunc, VisitFunc);

impl Collector {
    /// # new
    /// Create a new collector.
    ///
    /// ## Parameters
    /// * `thread_local_allocator` - thread-local allocator
    pub fn new(
        thread_local_allocator: ThreadLocalAllocator,
        queue: &mut VecDeque<*mut u8>,
    ) -> Self {
        Self {
            thread_local_allocator,
            roots: FxHashMap::default(),
            queue: queue as *mut VecDeque<*mut u8>,
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
    /// * `obj_type` - object type
    ///
    /// ## Return
    /// * `ptr` - object pointer
    pub fn alloc(&mut self, size: usize, obj_type: ObjectType) -> *mut u8 {
        let ptr = self.thread_local_allocator.alloc(size, obj_type);
        ptr
    }

    /// # add_root
    /// Add a root to the collector.
    ///
    /// ## Parameters
    /// * `root` - root
    /// * `size` - root size
    pub fn add_root(&mut self, root: *mut u8, obj_type: ObjectType) {
        self.roots.insert(root as usize, obj_type);
    }

    /// # remove_root
    /// Remove a root from the collector.
    ///
    /// ## Parameters
    /// * `root` - root
    pub fn remove_root(&mut self, root: *mut u8) {
        self.roots.remove(&(root as usize));
    }

    /// precise mark a pointer
    unsafe fn mark_ptr(&self, ptr: *mut u8) {
        // mark it if it is in heap
        if self.thread_local_allocator.in_heap(ptr) {
            let block = Block::from_obj_ptr(ptr);
            block.marked = true;
            let line_header = block.get_line_header_from_addr(ptr);
            if line_header.get_marked() {
                return;
            }
            line_header.set_marked(true);
            (*self.queue).push_back(ptr);
            let obj_type = line_header.get_obj_type();
            match obj_type {
                ObjectType::Atomic => {}
                ObjectType::Complex => {
                    self.mark_complex(ptr);
                }
                ObjectType::Trait => {
                    self.mark_trait(ptr);
                }
                ObjectType::Pointer => {
                    let ptr = *(ptr as *mut *mut u8);
                    self.mark_ptr(ptr);
                }
            }
        }
    }

    /// precise mark a complex object
    ///
    /// it self does not mark the object, but mark the object's fields by calling
    /// mark_ptr
    unsafe fn mark_complex(&self, ptr: *mut u8) {
        let vtable = *(ptr as *mut VtableFunc);
        vtable(
            ptr,
            self,
            Self::mark_ptr,
            Self::mark_complex,
            Self::mark_trait,
        );
    }
    /// precise mark a trait object
    unsafe fn mark_trait(&self, ptr: *mut u8) {
        let loaded = *(ptr as *mut *mut *mut u8);
        let ptr = *loaded.offset(1);
        self.mark_ptr(ptr);
    }

    /// # mark
    /// From gc roots, mark all reachable objects.
    ///
    /// this mark function is __precise__
    pub fn mark(&mut self) {
        for (root, obj_type) in self.roots.iter() {
            let root = *root as *mut u8;
            unsafe {
                match obj_type {
                    ObjectType::Atomic => {
                        let obj = *(root as *mut *mut u8);
                        if self.thread_local_allocator.in_heap(obj) {
                            (*self.queue).push_back(obj);
                        }
                    }
                    ObjectType::Complex => {
                        self.mark_complex(root);
                    }
                    ObjectType::Trait => {
                        self.mark_trait(root);
                    }
                    ObjectType::Pointer => {
                        let ptr = *(root as *mut *mut u8);
                        self.mark_ptr(ptr);
                    }
                }
            }
        }

        // iterate through queue and mark all reachable objects
        // get object size from object header, which is stored in the block header
        // the block can be constructed from object pointer, using Block::from_obj_ptr
        unsafe {
            while let Some(obj) = (*self.queue).pop_front() {
                self.mark_ptr(obj);
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
    use std::mem::size_of;

    use crate::allocator::GlobalAllocator;

    use super::*;
    unsafe fn set_point_to(ptr1: *mut u8, ptr2: *mut u8, offset: i64) {
        (ptr1 as *mut *mut u8).offset(offset as isize).write(ptr2);
    }

    struct GCTestObj {
        _vtable: VtableFunc,
        b: *mut GCTestObj,
        c: u64,
        d: *mut u64,
    }
    fn gctest_vtable(
        ptr: *mut u8,
        gc: &Collector,
        mark_ptr: VisitFunc,
        _mark_complex: VisitFunc,
        _mark_trait: VisitFunc,
    ) {
        let obj = ptr as *mut GCTestObj;
        unsafe {
            mark_ptr(gc, (*obj).b as *mut u8);
            mark_ptr(gc, (*obj).d as *mut u8);
        }
    }
    #[test]
    fn test_basic_gc() {
        unsafe {
            let mut ga = GlobalAllocator::new(1024 * 1024 * 1024);
            let tla = ThreadLocalAllocator::new(&mut ga);
            let mut queue = VecDeque::new();
            let mut gc = Collector::new(tla, &mut queue);
            let mut a = gc.alloc(size_of::<GCTestObj>(), ObjectType::Complex) as *mut GCTestObj;
            let b = gc.alloc(size_of::<GCTestObj>(), ObjectType::Complex) as *mut GCTestObj;
            (*a).b = b;
            (*a).c = 1;
            (*a)._vtable = gctest_vtable;
            (*b)._vtable = gctest_vtable;
            (*b).c = 2;
            let rustptr = (&mut a) as *mut *mut GCTestObj as *mut u8;
            gc.add_root(rustptr, ObjectType::Atomic);
            let size1 = gc.get_size();
            gc.collect();
            let size2 = gc.get_size();
            assert_eq!(size1, size2);
            let d = gc.alloc(size_of::<u64>(), ObjectType::Atomic) as *mut u64;
            (*b).d = d;
            (*d) = 3;
            gc.collect();
            let size3 = gc.get_size();
            assert!(size3 > size2);
            (*a).d = d;
            gc.collect();
            let size4 = gc.get_size();
            assert_eq!(size3, size4);
            (*a).b = a;
            gc.collect();
            let size5 = gc.get_size();
            assert!(size5 < size4);
            (*a).d = core::ptr::null_mut();
            gc.collect();
            let size6 = gc.get_size();
            assert!(size5 > size6);
        }
    }
}
