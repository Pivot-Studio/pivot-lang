use std::{
    sync::atomic::{AtomicPtr, Ordering},
    thread::yield_now,
};

use libc::malloc;
use parking_lot::lock_api::{RawRwLock, RawRwLockRecursive};
use rustc_hash::FxHashMap;
use vector_map::VecMap;

use crate::{
    allocator::{GlobalAllocator, ThreadLocalAllocator},
    block::{Block, LineHeaderExt, ObjectType},
    spin_until, GC_COLLECTOR_COUNT, GC_ID, GC_MARKING, GC_MARK_WAITING, GC_RUNNING, GC_RW_LOCK,
    GC_SWEEPING, GC_SWEEPPING_NUM, LINE_SIZE, NUM_LINES_PER_BLOCK,
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
    // queue: *mut VecDeque<*mut u8>,
    id: usize,
    mark_histogram: *mut VecMap<usize, usize>,
}

pub type VisitFunc = unsafe fn(&Collector, *mut u8);

pub type VtableFunc = fn(*mut u8, &Collector, VisitFunc, VisitFunc, VisitFunc);

impl Drop for Collector {
    fn drop(&mut self) {
        unsafe {
            libc::free(self.thread_local_allocator as *mut libc::c_void);
            // libc::free(self.queue as *mut libc::c_void);
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
            // let queue = VecDeque::new();
            // let memqueue =
            //     malloc(core::mem::size_of::<VecDeque<*mut u8>>()).cast::<VecDeque<*mut u8>>();
            // memqueue.write(queue);
            let memvecmap =
                malloc(core::mem::size_of::<VecMap<usize, usize>>()).cast::<VecMap<usize, usize>>();
            memvecmap.write(VecMap::with_capacity(NUM_LINES_PER_BLOCK));
            Self {
                thread_local_allocator: mem,
                roots: FxHashMap::default(),
                // queue: memqueue,
                id,
                mark_histogram: memvecmap,
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
    pub fn alloc(&self, size: usize, obj_type: ObjectType) -> *mut u8 {
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

    /// used to correct forward pointer
    ///
    /// 原本
    ///
    /// ptr -> heap_ptr -> value
    ///
    /// evacuate之后
    ///
    /// ptr -> heap_ptr -> new_ptr(forwarded) -> value
    ///
    /// 现在纠正为
    ///
    /// ptr -> new_ptr(forwarded) -> value
    unsafe fn correct_ptr(&self, ptr: *mut u8) {
        let ptr = ptr as *mut AtomicPtr<*mut u8>;
        let new_ptr = *(*ptr).load(Ordering::SeqCst);
        let ptr = ptr as *mut *mut u8;
        debug_assert!(!new_ptr.is_null());
        *ptr = new_ptr;
    }

    /// precise mark a pointer
    unsafe fn mark_ptr(&self, ptr: *mut u8) {
        let father = ptr;
        let mut ptr = *(ptr as *mut *mut u8);

        // mark it if it is in heap
        if self.thread_local_allocator.as_mut().unwrap().in_heap(ptr) {
            let block = Block::from_obj_ptr(ptr);
            let is_candidate = block.is_eva_candidate();
            if !is_candidate {
                block.marked = true;
            }
            let (line_header, idx) = block.get_line_header_from_addr(ptr);
            if line_header.get_marked() {
                return;
            }
            // 若此block是待驱逐对象
            if is_candidate {
                let atomic_ptr = ptr as *mut AtomicPtr<u8>;
                let old_loaded = (*atomic_ptr).load(Ordering::SeqCst);
                if line_header.get_forwarded() {
                    self.correct_ptr(father);
                    return;
                }
                let line_size = line_header.get_obj_line_size(idx, Block::from_obj_ptr(ptr));
                let new_ptr = self.alloc(line_size * LINE_SIZE, line_header.get_obj_type());
                let new_block = Block::from_obj_ptr(new_ptr);
                let (new_line_header, _) = new_block.get_line_header_from_addr(new_ptr);
                // 将数据复制到新的地址
                core::ptr::copy_nonoverlapping(ptr, new_ptr, line_size * LINE_SIZE);
                // core::ptr::copy_nonoverlapping(line_header as * const u8, new_line_header as * mut u8, line_size);

                if let Ok(_) = (*atomic_ptr).compare_exchange_weak(
                    old_loaded,
                    new_ptr,
                    Ordering::SeqCst,
                    Ordering::SeqCst,
                ) {
                    // 成功驱逐
                    // println!("{}: eva {} to {}", self.id, ptr as usize, new_ptr as usize);
                    new_line_header.set_marked(true);
                    line_header.set_forwarded(true);
                    new_block.marked = true;
                    ptr = new_ptr;
                    self.correct_ptr(father);
                } else {
                    // 期间别的线程驱逐了它
                    spin_until!(line_header.get_forwarded());
                    self.correct_ptr(father);
                    return;
                }
            }
            line_header.set_marked(true);
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
        if !self.thread_local_allocator.as_mut().unwrap().in_heap(ptr) {
            return;
        }
        let vtable = *(ptr as *mut VtableFunc);
        // let ptr = vtable as *mut u8;
        let v = vtable as i64;
        // println!("vtable: {:?}, ptr : {:p}", v, ptr);
        // 我不知道为什么，vtable为0的情况，这里如果写v == 0，进不去这个if。应该是rust的一个bug
        if v < 1 && v > -1 {
            return;
        }
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
        if !self.thread_local_allocator.as_mut().unwrap().in_heap(ptr) {
            return;
        }
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
            let root = (*root) as *mut u8;
            unsafe {
                match obj_type {
                    ObjectType::Atomic => {}
                    ObjectType::Complex => {
                        self.mark_complex(root);
                    }
                    ObjectType::Trait => {
                        self.mark_trait(root);
                    }
                    ObjectType::Pointer => {
                        self.mark_ptr(root);
                    }
                }
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
            self.thread_local_allocator
                .as_mut()
                .unwrap()
                .sweep(self.mark_histogram);
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
        // evacuation pre process
        // 这个过程要在完成safepoint同步之前完成，因为在驱逐的情况下
        // 他要设置每个block是否是驱逐目标
        // 如果设置的时候别的线程的mark已经开始，那么将无法保证能够纠正所有被驱逐的指针
        unsafe {
            if self.thread_local_allocator.as_mut().unwrap().should_eva() {
                let mut eva_threshold = 0;
                let mut available_histogram: VecMap<usize, usize> =
                    VecMap::with_capacity(NUM_LINES_PER_BLOCK);
                let mut available_lines = self
                    .thread_local_allocator
                    .as_mut()
                    .unwrap()
                    .fill_available_histogram(&mut available_histogram);
                let mut required_lines = 0;
                let mark_histogram = &mut *self.mark_histogram;
                // println!(
                //     "available_histogram: {:?} mark_histogram: {:?} total_available: {}",
                //     available_histogram, mark_histogram, available_lines
                // );
                for threshold in (0..(NUM_LINES_PER_BLOCK / 2)).rev() {
                    required_lines += *mark_histogram.get(&threshold).unwrap_or(&0);
                    available_lines = available_lines
                        .saturating_sub(*available_histogram.get(&threshold).unwrap_or(&0));
                    if available_lines <= required_lines {
                        eva_threshold = threshold;
                        break;
                    }
                }
                // println!("eva_threshold: {}", eva_threshold);
                self.thread_local_allocator
                    .as_mut()
                    .unwrap()
                    .set_eva_threshold(eva_threshold);
                // self.eva_threshold = eva_threshold;
                // self.eva = true
            }
            (*self.mark_histogram).clear();
        }
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

    const LINE_SIZE_OBJ: usize = 2;

    struct GCTestObj {
        _vtable: VtableFunc,
        b: *mut GCTestObj,
        c: u64,
        _arr: [u8; 128],
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
            mark_ptr(gc, (&mut (*obj).b) as *mut *mut GCTestObj as *mut u8);
            mark_ptr(gc, (&mut (*obj).d) as *mut *mut u64 as *mut u8);
            mark_ptr(gc, (&mut (*obj).e) as *mut *mut GCTestObj as *mut u8);
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
                    gc.add_root(rustptr, ObjectType::Pointer);
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
                    gc.add_root(rustptr, ObjectType::Pointer);
                    let size1 = gc.get_size();
                    assert_eq!(size1, 2 * LINE_SIZE_OBJ)
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
            println!(
                "gcid = {} rustptr point to {:p}",
                gc.get_id(),
                *(rustptr as *mut *mut GCTestObj)
            );
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
            gc.add_root(rustptr, ObjectType::Pointer);
            let size1 = gc.get_size();
            assert_eq!(size1, 1001 * LINE_SIZE_OBJ);
            let time = std::time::Instant::now();
            gc.collect();
            println!("gc{} gc time = {:?}", gc.get_id(), time.elapsed());
            let size2 = gc.get_size();
            assert_eq!(live_obj * LINE_SIZE_OBJ, size2);
            gc.collect();
            let size3 = gc.get_size();
            assert_eq!(size2, size3);
            let first_obj = *(rustptr as *mut *mut GCTestObj);
            let objs = walk_obj(first_obj);
            assert_eq!(objs, live_obj);
            gc.remove_root(rustptr);
            gc.collect();
            let size3 = gc.get_size();
            assert_eq!(size3, 0);
        });
    }

    fn walk_obj(obj: *mut GCTestObj) -> usize {
        unsafe {
            let b = Block::from_obj_ptr(obj as *mut u8);
            let (h, _) = b.get_line_header_from_addr(obj as *mut u8);
            assert!(h.get_used());
        }
        let mut count = 0;
        unsafe {
            if !(*obj).b.is_null() {
                count += walk_obj((*obj).b);
            }
            if !(*obj).e.is_null() {
                count += walk_obj((*obj).e);
            }
        }
        count + 1
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
