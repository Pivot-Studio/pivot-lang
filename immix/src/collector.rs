use std::{collections::VecDeque, sync::atomic::Ordering, thread::yield_now};

use libc::malloc;
use parking_lot::lock_api::{RawRwLock, RawRwLockRecursive};
use rustc_hash::FxHashMap;

use crate::{
    allocator::{GlobalAllocator, ThreadLocalAllocator},
    block::{Block, LineHeaderExt, ObjectType},
    spin_until, GC_COLLECTOR_COUNT, GC_ID, GC_MARKING, GC_MARK_WAITING, GC_RUNNING, GC_RW_LOCK,
    GC_SWEEPING, GC_SWEEPPING_NUM,
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
    thread_local_allocator: *mut ThreadLocalAllocator,
    roots: FxHashMap<usize, ObjectType>,
    queue: *mut VecDeque<*mut u8>,
    id: usize,
}

pub type VisitFunc = unsafe fn(&Collector, *mut u8);

pub type VtableFunc = fn(*mut u8, &Collector, VisitFunc, VisitFunc, VisitFunc);

impl Drop for Collector {
    fn drop(&mut self) {
        unsafe {
            libc::free(self.thread_local_allocator as *mut libc::c_void);
            libc::free(self.queue as *mut libc::c_void);
            GC_COLLECTOR_COUNT.fetch_sub(1, Ordering::SeqCst);
        }
    }
}

impl Collector {
    /// # new
    /// Create a new collector.
    ///
    /// ## Parameters
    /// * `heap_size` - heap size
    pub fn new(ga: &mut GlobalAllocator) -> Self {
        GC_COLLECTOR_COUNT.fetch_add(1, Ordering::SeqCst);
        let id = GC_ID.fetch_add(1, Ordering::Relaxed);
        unsafe {
            let tla = ThreadLocalAllocator::new(ga);
            let mem =
                malloc(core::mem::size_of::<ThreadLocalAllocator>()).cast::<ThreadLocalAllocator>();
            mem.write(tla);
            let queue = VecDeque::new();
            let memqueue =
                malloc(core::mem::size_of::<VecDeque<*mut u8>>()).cast::<VecDeque<*mut u8>>();
            memqueue.write(queue);
            Self {
                thread_local_allocator: mem,
                roots: FxHashMap::default(),
                queue: memqueue,
                id,
            }
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
        unsafe { self.thread_local_allocator.as_mut().unwrap().get_size() }
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
        unsafe {
            let ptr = self
                .thread_local_allocator
                .as_mut()
                .unwrap()
                .alloc(size, obj_type);
            ptr
        }
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
        if self.thread_local_allocator.as_mut().unwrap().in_heap(ptr) {
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

    pub fn print_stats(&self) {
        unsafe {
            self.thread_local_allocator.as_ref().unwrap().print_stats();
        }
    }
    pub fn get_id(&self) -> usize {
        self.id
    }

    /// # mark
    /// From gc roots, mark all reachable objects.
    ///
    /// this mark function is __precise__
    pub fn mark(&mut self) {
        GC_RUNNING.store(true, Ordering::Release);
        let gcs = GC_COLLECTOR_COUNT.load(Ordering::Acquire);
        let v = GC_MARK_WAITING.fetch_add(1, Ordering::Acquire);
        if v + 1 != gcs {
            let mut i = 0;
            while !GC_MARKING.load(Ordering::Acquire) {
                // 防止 gc count 改变（一个线程在gc时消失了
                let gcs = GC_COLLECTOR_COUNT.load(Ordering::Acquire);
                if gcs == v + 1 {
                    GC_MARKING.store(true, Ordering::Release);
                    break;
                }
                core::hint::spin_loop();
                i += 1;
                if i % 100 == 0 {
                    yield_now();
                }
            }
        } else {
            GC_MARKING.store(true, Ordering::Release);
        }

        for (root, obj_type) in self.roots.iter() {
            let root = *root as *mut u8;
            unsafe {
                match obj_type {
                    ObjectType::Atomic => {
                        let obj = *(root as *mut *mut u8);
                        if self.thread_local_allocator.as_mut().unwrap().in_heap(obj) {
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

        let v = GC_MARK_WAITING.fetch_sub(1, Ordering::AcqRel);

        if v - 1 == 0 {
            GC_MARKING.store(false, Ordering::Release);
        } else {
            spin_until!(GC_MARKING.load(Ordering::Acquire) == false);
        }
    }

    /// # sweep
    ///
    /// since we did synchronization in mark, we don't need to do synchronization again in sweep
    pub fn sweep(&mut self) {
        GC_SWEEPPING_NUM.fetch_add(1, Ordering::AcqRel);
        GC_SWEEPING.store(true, Ordering::Release);
        unsafe {
            self.thread_local_allocator.as_mut().unwrap().sweep();
        }
        let v = GC_SWEEPPING_NUM.fetch_sub(1, Ordering::AcqRel);
        if v - 1 == 0 {
            GC_SWEEPING.store(false, Ordering::Release);
            GC_RUNNING.store(false, Ordering::Release);
        } else {
            spin_until!(!GC_SWEEPING.load(Ordering::Acquire));
        }
    }

    /// # collect
    /// Collect garbage.
    pub fn collect(&mut self) -> std::time::Duration {
        let lock = unsafe { GC_RW_LOCK.raw() };
        spin_until!(lock.try_lock_shared_recursive());
        let time = std::time::Instant::now();
        self.mark();
        self.sweep();
        let time = time.elapsed();
        unsafe {
            lock.unlock_shared();
        }
        time
    }
}

#[cfg(test)]
mod tests {
    use std::mem::size_of;

    use rand::random;

    use crate::SPACE;

    use super::*;

    struct GCTestObj {
        _vtable: VtableFunc,
        b: *mut GCTestObj,
        c: u64,
        d: *mut u64,
        e: *mut GCTestObj,
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
            mark_ptr(gc, (*obj).e as *mut u8);
        }
    }
    #[test]
    fn test_basic_multiple_thread_gc() {
        let mut handles = vec![];
        for _ in 0..10 {
            let t = std::thread::spawn(|| {
                SPACE.with(|gc| unsafe {
                    let mut gc = gc.borrow_mut();
                    println!("thread1 gcid = {}", gc.get_id());
                    let mut a =
                        gc.alloc(size_of::<GCTestObj>(), ObjectType::Complex) as *mut GCTestObj;
                    let b = gc.alloc(size_of::<GCTestObj>(), ObjectType::Complex) as *mut GCTestObj;
                    (*a).b = b;
                    (*a).c = 1;
                    (*a)._vtable = gctest_vtable;
                    (*b)._vtable = gctest_vtable;
                    (*b).c = 2;
                    let rustptr = (&mut a) as *mut *mut GCTestObj as *mut u8;
                    gc.add_root(rustptr, ObjectType::Atomic);
                    let size1 = gc.get_size();
                    let time = std::time::Instant::now();
                    gc.collect();
                    println!("gc{} gc time = {:?}", gc.get_id(), time.elapsed());
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
                    gc.remove_root(rustptr);
                    gc.collect();
                    let size7 = gc.get_size();
                    assert_eq!(0, size7);
                    let mut a =
                        gc.alloc(size_of::<GCTestObj>(), ObjectType::Complex) as *mut GCTestObj;
                    let b = gc.alloc(size_of::<GCTestObj>(), ObjectType::Complex) as *mut GCTestObj;
                    (*a).b = b;
                    (*a).c = 1;
                    (*a)._vtable = gctest_vtable;
                    (*b)._vtable = gctest_vtable;
                    (*b).c = 2;
                    let rustptr = (&mut a) as *mut *mut GCTestObj as *mut u8;
                    gc.add_root(rustptr, ObjectType::Atomic);
                    let size1 = gc.get_size();
                    assert_eq!(size1, 2)
                });
            });
            handles.push(t);
        }
        for h in handles {
            h.join().unwrap();
        }
    }

    unsafe fn alloc_test_obj(gc: &mut Collector) -> *mut GCTestObj {
        let a = gc.alloc(size_of::<GCTestObj>(), ObjectType::Complex) as *mut GCTestObj;
        (*a)._vtable = gctest_vtable;
        a
    }

    #[test]
    fn test_complecated_single_thread_gc() {
        SPACE.with(|gc| unsafe {
            let mut gc = gc.borrow_mut();
            println!("thread1 gcid = {}", gc.get_id());
            let mut first_obj = alloc_test_obj(&mut gc);
            let rustptr = (&mut first_obj) as *mut *mut GCTestObj as *mut u8;
            let mut live_obj = 1;
            let mut unused_objs = vec![&mut (*first_obj).b, &mut (*first_obj).e];
            for _ in 0..1000 {
                let obj = alloc_test_obj(&mut gc);
                if random() {
                    live_obj += 1;
                    let father_ptr = unused_objs.pop().unwrap();
                    *father_ptr = obj;
                    unused_objs.push(&mut (*obj).b);
                    unused_objs.push(&mut (*obj).e);
                }
            }
            gc.add_root(rustptr, ObjectType::Atomic);
            let size1 = gc.get_size();
            assert_eq!(size1, 1001);
            let time = std::time::Instant::now();
            gc.collect();
            println!("gc{} gc time = {:?}", gc.get_id(), time.elapsed());
            let size2 = gc.get_size();
            assert_eq!(live_obj, size2);
            gc.remove_root(rustptr);
            gc.collect();
            let size3 = gc.get_size();
            assert_eq!(size3, 0);
        });
    }

    #[test]
    fn test_complecated_multiple_thread_gc() {
        let mut handles = vec![];
        for _ in 0..10 {
            let t = std::thread::spawn(|| test_complecated_single_thread_gc());
            handles.push(t);
        }
        for h in handles {
            h.join().unwrap();
        }
    }
}
