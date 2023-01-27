use std::{
    collections::VecDeque,
    ptr::drop_in_place,
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
    spin_until, ENABLE_EVA, GC_COLLECTOR_COUNT, GC_ID, GC_MARKING, GC_MARK_WAITING, GC_RUNNING,
    GC_RW_LOCK, GC_SWEEPING, GC_SWEEPPING_NUM, LINE_SIZE, NUM_LINES_PER_BLOCK,
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
    queue: *mut VecDeque<(*mut u8, ObjectType)>,
    id: usize,
    mark_histogram: *mut VecMap<usize, usize>,
}

pub type VisitFunc = unsafe fn(&Collector, *mut u8);

pub type VtableFunc = fn(*mut u8, &Collector, VisitFunc, VisitFunc, VisitFunc);

impl Drop for Collector {
    fn drop(&mut self) {
        unsafe {
            drop_in_place(self.thread_local_allocator);
            libc::free(self.thread_local_allocator as *mut libc::c_void);
            libc::free(self.mark_histogram as *mut libc::c_void);
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
            let memvecmap =
                malloc(core::mem::size_of::<VecMap<usize, usize>>()).cast::<VecMap<usize, usize>>();
            memvecmap.write(VecMap::with_capacity(NUM_LINES_PER_BLOCK));
            let queue = VecDeque::new();
            let memqueue = malloc(core::mem::size_of::<VecDeque<(*mut u8, ObjectType)>>())
                .cast::<VecDeque<(*mut u8, ObjectType)>>();
            memqueue.write(queue);
            Self {
                thread_local_allocator: mem,
                roots: FxHashMap::default(),
                id,
                mark_histogram: memvecmap,
                queue: memqueue,
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
        // println!("correct ptr {:p} to {:p}", ptr, new_ptr);
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
            } else {
                let (line_header, _) = block.get_line_header_from_addr(ptr);
                if line_header.get_forwarded() {
                    self.correct_ptr(father);
                    return;
                }
            }
            let (line_header, idx) = block.get_line_header_from_addr(ptr);
            if line_header.get_marked() {
                return;
            }
            // 若此block是待驱逐对象
            if is_candidate {
                let atomic_ptr = ptr as *mut AtomicPtr<u8>;
                let old_loaded = (*atomic_ptr).load(Ordering::SeqCst);
                let obj_line_size = line_header.get_obj_line_size(idx, Block::from_obj_ptr(ptr));
                let new_ptr = self.alloc(obj_line_size * LINE_SIZE, line_header.get_obj_type());
                let new_block = Block::from_obj_ptr(new_ptr);
                let (new_line_header, _) = new_block.get_line_header_from_addr(new_ptr);
                // 将数据复制到新的地址
                core::ptr::copy_nonoverlapping(ptr, new_ptr, obj_line_size * LINE_SIZE);
                // core::ptr::copy_nonoverlapping(line_header as * const u8, new_line_header as * mut u8, line_size);

                // 线程安全性说明
                // 如果cas操作成功，代表没有别的线程与我们同时尝试驱逐这个模块
                // 如果cas操作失败，代表别的线程已经驱逐了这个模块
                // 此时虽然我们已经分配了驱逐空间并且将内容写入，但是我们还没有设置line_header的
                // 标记位，所以这块地址很快会在sweep阶段被识别并且回收利用。
                // 虽然这造成了一定程度的内存碎片化和潜在的重复操作，但是
                // 这种情况理论上是很少见的，所以这么做问题不大
                if let Ok(_) = (*atomic_ptr).compare_exchange_weak(
                    old_loaded,
                    new_ptr,
                    Ordering::SeqCst,
                    Ordering::SeqCst,
                ) {
                    // 成功驱逐
                    // println!("gc {}: eva {:p} to {:p}", self.id, ptr, new_ptr);
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
                _ => (*self.queue).push_back((ptr, obj_type)),
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
        // let a = *(ptr as *mut *mut u8);
        // println!("a: {:p}", a);
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

    pub fn iter<F>(&self, f: F)
    where
        F: FnMut(*mut u8),
    {
        unsafe {
            self.thread_local_allocator.as_ref().unwrap().iter(f);
        }
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
                    _ => (*self.queue).push_back((root, *obj_type)),
                }
            }
        }
        // iterate through queue and mark all reachable objects
        unsafe {
            while let Some((obj, obj_type)) = (*self.queue).pop_front() {
                match obj_type {
                    ObjectType::Atomic => {}
                    ObjectType::Complex => {
                        self.mark_complex(obj);
                    }
                    ObjectType::Trait => {
                        self.mark_trait(obj);
                    }
                    ObjectType::Pointer => {
                        self.mark_ptr(obj);
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
    pub fn collect(&mut self) -> (std::time::Duration, std::time::Duration) {
        // evacuation pre process
        // 这个过程要在完成safepoint同步之前完成，因为在驱逐的情况下
        // 他要设置每个block是否是驱逐目标
        // 如果设置的时候别的线程的mark已经开始，那么将无法保证能够纠正所有被驱逐的指针
        unsafe {
            if ENABLE_EVA && self.thread_local_allocator.as_mut().unwrap().should_eva() {
                // 如果需要驱逐，首先计算驱逐阀域
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
                for threshold in (0..(NUM_LINES_PER_BLOCK / 2)).rev() {
                    // 从洞多到洞少遍历，计算剩余空间，直到空间不足
                    // 此时洞的数量就是驱逐阀域
                    required_lines += *mark_histogram.get(&threshold).unwrap_or(&0);
                    available_lines = available_lines
                        .saturating_sub(*available_histogram.get(&threshold).unwrap_or(&0));
                    if available_lines <= required_lines {
                        eva_threshold = threshold;
                        break;
                    }
                }
                // 根据驱逐阀域标记每个block是否是驱逐目标
                self.thread_local_allocator
                    .as_mut()
                    .unwrap()
                    .set_eva_threshold(eva_threshold);
            }
            (*self.mark_histogram).clear();
        }
        let lock = unsafe { GC_RW_LOCK.raw() };
        spin_until!(lock.try_lock_shared_recursive());
        let time = std::time::Instant::now();
        unsafe {
            self.thread_local_allocator
                .as_mut()
                .unwrap()
                .set_collect_mode(true);
        }
        self.mark();
        let mark_time = time.elapsed();
        self.sweep();
        let sweep_time = time.elapsed() - mark_time;
        unsafe {
            self.thread_local_allocator
                .as_mut()
                .unwrap()
                .set_collect_mode(false);
            lock.unlock_shared();
        }
        (mark_time, sweep_time)
    }
}

#[cfg(test)]
mod tests {
    use std::{mem::size_of, thread::sleep, time::Duration};

    use lazy_static::lazy_static;
    use parking_lot::Mutex;
    use rand::random;
    use rustc_hash::FxHashSet;

    use crate::SPACE;

    use super::*;

    const LINE_SIZE_OBJ: usize = 1;

    #[repr(C)]
    struct GCTestObj {
        _vtable: VtableFunc,
        b: *mut GCTestObj,
        c: u64,
        // _arr: [u8; 128],
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
        let _lock = THE_RESOURCE.lock();
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

    lazy_static! {
        static ref TEST_OBJ_QUEUE: Mutex<Vec<TestObjWrap>> = Mutex::new(vec![]);
    }
    #[derive(Debug, PartialEq, Eq, Hash)]
    struct TestObjWrap(*mut *mut GCTestObj);

    unsafe impl Send for TestObjWrap {}
    unsafe impl Sync for TestObjWrap {}

    struct SingleThreadResult {
        size1: usize,
        expect_size1: usize,
        size2: usize,
        expect_size2: usize,
        size3: usize,
        expect_size3: usize,
        expect_objs: usize,
        set: FxHashSet<TestObjWrap>,
        set2: FxHashSet<TestObjWrap>,
    }

    fn single_thread(obj_num: usize) -> SingleThreadResult {
        SPACE.with(|gc| unsafe {
            // 睡眠一秒保证所有线程创建
            let mut gc = gc.borrow_mut();
            println!("thread1 gcid = {}", gc.get_id());
            sleep(Duration::from_secs(1));
            let mut first_obj = alloc_test_obj(&mut gc);
            println!("first_obj = {:p}", first_obj);
            let rustptr = (&mut first_obj) as *mut *mut GCTestObj as *mut u8;
            println!(
                "gcid = {} rustptr point to {:p}",
                gc.get_id(),
                *(rustptr as *mut *mut GCTestObj)
            );
            let mut live_obj = 1;
            TEST_OBJ_QUEUE
                .lock()
                .push(TestObjWrap(&mut (*first_obj).b as *mut *mut GCTestObj));
            TEST_OBJ_QUEUE
                .lock()
                .push(TestObjWrap(&mut (*first_obj).e as *mut *mut GCTestObj));
            // let mut unused_objs = vec![&mut (*first_obj).b, &mut (*first_obj).e];
            let mut has_loop = false;
            for _ in 0..obj_num {
                let obj = alloc_test_obj(&mut gc);
                if random() {
                    live_obj += 1;
                    let father_ptr = TEST_OBJ_QUEUE.lock().pop().unwrap().0;
                    *father_ptr = obj;
                    if random() && random() && !has_loop {
                        // 循环引用
                        (*obj).b = first_obj;
                        println!("live_obj = {} has loop", live_obj);
                        has_loop = true;
                    } else {
                        TEST_OBJ_QUEUE
                            .lock()
                            .push(TestObjWrap(&mut (*obj).b as *mut *mut GCTestObj));
                    }
                    TEST_OBJ_QUEUE
                        .lock()
                        .push(TestObjWrap(&mut (*obj).e as *mut *mut GCTestObj));
                }
            }
            gc.add_root(rustptr, ObjectType::Pointer);
            let size1 = gc.get_size();

            let mut set2 = FxHashSet::default();
            let mut ii = 0;
            gc.iter(|ptr| {
                ii += 1;
                // if set2.contains(&TestObjWrap(ptr as *mut *mut GCTestObj)) {
                //     panic!("repeat ptr = {:p}", ptr);
                // }
                set2.insert(TestObjWrap(
                    Block::from_obj_ptr(ptr) as *mut Block as *mut *mut GCTestObj
                ));
            });
            println!("gc{} itered ", gc.get_id());

            // assert_eq!(size1, 1001 * LINE_SIZE_OBJ);
            let time = std::time::Instant::now();
            gc.collect();
            println!("gc{} gc time = {:?}", gc.get_id(), time.elapsed());
            let size2 = gc.get_size();

            // assert_eq!(live_obj * LINE_SIZE_OBJ, size2);
            gc.collect();
            let size3 = gc.get_size();
            // assert_eq!(ii, size3);
            // assert_eq!(set2.len(), size3);
            let first_obj = *(rustptr as *mut *mut GCTestObj);
            println!("new first_obj = {:p}", first_obj);
            let mut set = FxHashSet::default();
            let objs = walk_obj(first_obj, &mut set);
            // assert_eq!(objs, live_obj);
            assert_eq!(objs, set.len());
            gc.remove_root(rustptr);
            gc.collect();

            let size4 = gc.get_size();
            assert_eq!(size4, 0);
            SingleThreadResult {
                size1,
                expect_size1: (obj_num + 1) * LINE_SIZE_OBJ,
                size2,
                expect_size2: live_obj * LINE_SIZE_OBJ,
                size3,
                expect_size3: size2,
                set,
                expect_objs: live_obj,
                set2,
            }
        })
    }

    lazy_static! {
        /// gc测试不能并发啊进行，需要加锁
        static ref THE_RESOURCE: Mutex<()> = Mutex::new(());
    }

    #[test]
    fn test_complecated_single_thread_gc() {
        // 这个测试和test_complecated_multiple_thread_gc不能同时跑，用了全局变量会相互干扰
        let _lock = THE_RESOURCE.lock();
        TEST_OBJ_QUEUE.lock().clear();
        let re = single_thread(1000);
        assert_eq!(re.size1, re.expect_size1);
        assert_eq!(re.size2, re.expect_size2);
        assert_eq!(re.size3, re.size2);
        assert_eq!(re.size3, re.expect_size3);
        assert_eq!(re.set.len(), re.expect_objs);
    }

    fn walk_obj(obj: *mut GCTestObj, set: &mut FxHashSet<TestObjWrap>) -> usize {
        unsafe {
            let b = Block::from_obj_ptr(obj as *mut u8);
            let (h, _) = b.get_line_header_from_addr(obj as *mut u8);
            assert!(h.get_used());
        }
        if set.contains(&TestObjWrap(obj as *mut *mut GCTestObj)) {
            return 0;
        }
        let mut count = 0;
        set.insert(TestObjWrap(obj as *mut *mut GCTestObj));
        unsafe {
            if !(*obj).b.is_null() {
                count += walk_obj((*obj).b, set);
            }
            if !(*obj).e.is_null() {
                count += walk_obj((*obj).e, set);
            }
        }
        count + 1
    }
    #[test]
    fn test_complecated_multiple_thread_gc() {
        let _lock = THE_RESOURCE.lock();
        TEST_OBJ_QUEUE.lock().clear();
        let mut handles = vec![];
        for _ in 0..10 {
            let t = std::thread::Builder::new()
                .spawn(|| single_thread(1000))
                .unwrap();
            handles.push(t);
        }
        let mut total_size1 = 0;
        let mut total_target_size1 = 0;
        let mut total_size2 = 0;
        let mut total_target_size2 = 0;
        let mut total_size3 = 0;
        let mut total_target_size3 = 0;
        let mut total_target_objs = 0;
        let mut set = FxHashSet::default();
        let mut set2 = FxHashSet::default();
        for h in handles {
            let mut re = h.join().unwrap();
            total_size1 += re.size1;
            total_target_size1 += re.expect_size1;
            total_size2 += re.size2;
            total_target_size2 += re.expect_size2;
            total_size3 += re.size3;
            total_target_size3 += re.expect_size3;
            set.extend(re.set);
            for s in re.set2.drain().into_iter() {
                if set2.contains(&s) {
                    println!("repeat ptr = {:p}", s.0);
                }
                set2.insert(s);
            }
            total_target_objs += re.expect_objs;
        }
        assert_eq!(total_size1, total_target_size1);
        assert_eq!(total_size2, total_target_size2);
        assert_eq!(total_size3, total_size2);
        assert_eq!(total_size3, total_target_size3);
        for k in set2.iter() {
            if !set.contains(k) {
                println!("set2 not in set {:p}", k.0);
            }
        }
        println!("set2 len {}", set2.len());
        println!("size2 {} size3 {}", total_size2, total_size3);
        assert_eq!(set.len(), total_target_objs);
    }
}
