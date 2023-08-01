use std::{
    cell::RefCell,
    ptr::drop_in_place,
    sync::atomic::{AtomicPtr, Ordering},
};

use libc::malloc;
use vector_map::VecMap;

#[cfg(feature = "llvm_stackmap")]
use crate::STACK_MAP;
use crate::{
    allocator::{GlobalAllocator, ThreadLocalAllocator},
    block::{Block, LineHeaderExt, ObjectType},
    gc_is_auto_collect_enabled, spin_until, HeaderExt, ENABLE_EVA, GC_COLLECTOR_COUNT, GC_ID,
    GC_MARKING, GC_MARK_COND, GC_RUNNING, GC_STW_COUNT, GC_SWEEPING, GC_SWEEPPING_NUM, LINE_SIZE,
    NUM_LINES_PER_BLOCK, THRESHOLD_PROPORTION, USE_SHADOW_STACK,
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
    #[cfg(feature = "shadow_stack")]
    roots: rustc_hash::FxHashMap<*mut u8, ObjectType>,
    queue: *mut Vec<(*mut u8, ObjectType)>,
    id: usize,
    mark_histogram: *mut VecMap<usize, usize>,
    status: RefCell<CollectorStatus>,
}

struct CollectorStatus {
    /// in bytes
    collect_threshold: usize,
    /// in bytes
    bytes_allocated_since_last_gc: usize,
    last_gc_time: std::time::SystemTime,
    collecting: bool,
}

pub type VisitFunc = unsafe extern "C" fn(&Collector, *mut u8);

pub type VtableFunc = extern "C" fn(*mut u8, &Collector, VisitFunc, VisitFunc, VisitFunc);

impl Drop for Collector {
    fn drop(&mut self) {
        // println!("Collector {} is dropped", self.id);
        unsafe {
            if (*self.thread_local_allocator).is_live() {
                let mut v = GC_COLLECTOR_COUNT.lock();
                v.0 -= 1;
                GC_MARK_COND.notify_all();
                drop(v);
                drop_in_place(self.thread_local_allocator);
            }
            libc::free(self.thread_local_allocator as *mut libc::c_void);
            libc::free(self.mark_histogram as *mut libc::c_void);
            libc::free(self.queue as *mut libc::c_void);
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
        let mut v = GC_COLLECTOR_COUNT.lock();
        v.0 += 1;
        GC_MARK_COND.notify_all();
        drop(v);
        let id = GC_ID.fetch_add(1, Ordering::Relaxed);
        unsafe {
            let tla = ThreadLocalAllocator::new(ga);
            let mem =
                malloc(core::mem::size_of::<ThreadLocalAllocator>()).cast::<ThreadLocalAllocator>();
            mem.write(tla);
            let memvecmap =
                malloc(core::mem::size_of::<VecMap<usize, usize>>()).cast::<VecMap<usize, usize>>();
            memvecmap.write(VecMap::with_capacity(NUM_LINES_PER_BLOCK));
            let queue = Vec::new();
            let memqueue =
                malloc(core::mem::size_of::<Vec<(*mut u8, ObjectType)>>())
                    .cast::<Vec<(*mut u8, ObjectType)>>();
            memqueue.write(queue);
            Self {
                thread_local_allocator: mem,
                #[cfg(feature = "shadow_stack")]
                roots: rustc_hash::FxHashMap::default(),
                id,
                mark_histogram: memvecmap,
                queue: memqueue,
                status: RefCell::new(CollectorStatus {
                    collect_threshold: 1024,
                    bytes_allocated_since_last_gc: 0,
                    last_gc_time: std::time::SystemTime::now(),
                    collecting: false,
                }),
            }
        }
    }

    pub fn unregister_current_thread(&self) {
        let mut v = GC_COLLECTOR_COUNT.lock();
        v.0 -= 1;
        GC_MARK_COND.notify_all();
        drop(v);
        unsafe {
            drop_in_place(self.thread_local_allocator);
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
        if size == 0 {
            return std::ptr::null_mut();
        }
        if gc_is_auto_collect_enabled() {
            if GC_RUNNING.load(Ordering::Acquire) {
                self.collect();
            }
            let status = self.status.borrow();
            if status.collect_threshold < status.bytes_allocated_since_last_gc {
                drop(status);
                self.collect();
            } else {
                drop(status);
            }
            let mut status = self.status.borrow_mut();
            status.bytes_allocated_since_last_gc += ((size - 1) / LINE_SIZE + 1) * LINE_SIZE;
        }
        unsafe {
            let ptr = self
                .thread_local_allocator
                .as_mut()
                .unwrap()
                .alloc(size, obj_type);
            if ptr.is_null() {
                self.collect();
                return self.alloc(size, obj_type);
            }
            ptr
        }
    }
    pub fn alloc_no_collect(&self, size: usize, obj_type: ObjectType) -> *mut u8 {
        unsafe {
            let ptr = self
                .thread_local_allocator
                .as_mut()
                .unwrap()
                .alloc(size, obj_type);
            debug_assert!(!ptr.is_null());
            ptr
        }
    }

    /// # add_root
    /// Add a root to the collector.
    ///
    /// ## Parameters
    /// * `root` - root
    /// * `size` - root size
    #[cfg(feature = "shadow_stack")]
    pub fn add_root(&mut self, root: *mut u8, obj_type: ObjectType) {
        self.roots.insert(root, obj_type);
    }

    /// # remove_root
    /// Remove a root from the collector.
    ///
    /// ## Parameters
    /// * `root` - root
    #[cfg(feature = "shadow_stack")]
    pub fn remove_root(&mut self, root: *mut u8) {
        self.roots.remove(&(root));
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
        log::trace!("gc {} correct ptr {:p} to {:p}", self.id, ptr, new_ptr);
        *ptr = new_ptr;
    }

    #[cfg(feature = "llvm_gc_plugin")]
    extern "C" fn mark_ptr_callback(ptr: *mut u8, _tp: *mut u8) {
        unsafe {
            crate::SPACE.with(|gc| {
                gc.borrow().mark_ptr(ptr);
            });
        }
    }

    /// precise mark a pointer
    unsafe extern "C" fn mark_ptr(&self, ptr: *mut u8) {
        let father = ptr;
        // check pointer is valid (divided by 8)
        if (ptr as usize) % 8 != 0 {
            return;
        }

        let mut ptr = *(ptr as *mut *mut u8);
        // println!("mark ptr {:p} -> {:p}", father, ptr);
        // mark it if it is in heap
        if self.thread_local_allocator.as_mut().unwrap().in_heap(ptr) {
            // println!("mark ptr succ {:p} -> {:p}", father, ptr);
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
                if (*atomic_ptr)
                    .compare_exchange(old_loaded, new_ptr, Ordering::SeqCst, Ordering::SeqCst)
                    .is_ok()
                {
                    // 成功驱逐
                    log::trace!("gc {}: eva {:p} to {:p}", self.id, ptr, new_ptr);
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
                _ => (*self.queue).push((ptr, obj_type)),
            }
            return;
        }
        // mark it if it is a big object
        if self
            .thread_local_allocator
            .as_mut()
            .unwrap()
            .in_big_heap(ptr)
        {
            let big_obj = self
                .thread_local_allocator
                .as_mut()
                .unwrap()
                .big_obj_from_ptr(ptr)
                .unwrap();
            if (*big_obj).header.get_marked() {
                return;
            }
            (*big_obj).header.set_marked(true);
            let obj_type = (*big_obj).header.get_obj_type();
            match obj_type {
                ObjectType::Atomic => {}
                _ => (*self.queue).push((ptr, obj_type)),
            }
        }
    }

    /// precise mark a complex object
    ///
    /// it self does not mark the object, but mark the object's fields by calling
    /// mark_ptr
    unsafe extern "C" fn mark_complex(&self, ptr: *mut u8) {
        let vptr = *(ptr as *mut *mut u8);
        if vptr.is_null() {
            return;
        }
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
    unsafe extern "C" fn mark_trait(&self, ptr: *mut u8) {
        // if !self.thread_local_allocator.as_mut().unwrap().in_heap(ptr)
        //    &&!self.thread_local_allocator.as_mut().unwrap().in_big_heap(ptr) {
        //     return;
        // }
        let loaded = ptr as *mut *mut u8;
        let ptr = loaded.offset(1);
        // the trait is not init
        if ptr.is_null() {
            return;
        }
        self.mark_ptr(ptr as *mut u8);
    }

    pub fn print_stats(&self) {
        println!("gc {} states:", self.id);
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
    pub fn mark(&self) {
        GC_RUNNING.store(true, Ordering::Release);

        let mut v = GC_COLLECTOR_COUNT.lock();
        let (count, mut waiting) = *v;
        waiting += 1;
        *v = (count, waiting);
        // println!("gc mark {}: waiting: {}, count: {}", self.id, waiting, count);
        if waiting != count {
            GC_MARK_COND.wait_while(&mut v, |(c, _)| {
                // 线程数量变化了？
                if waiting == *c {
                    GC_MARKING.store(true, Ordering::Release);
                    GC_STW_COUNT.fetch_add(1, Ordering::Relaxed);
                    GC_MARK_COND.notify_all();
                    return false;
                }
                !GC_MARKING.load(Ordering::Acquire)
            });
            drop(v);
        } else {
            GC_MARKING.store(true, Ordering::Release);
            GC_STW_COUNT.fetch_add(1, Ordering::Relaxed);
            GC_MARK_COND.notify_all();
            drop(v);
        }

        #[cfg(feature = "shadow_stack")]
        {
            for (root, obj_type) in self.roots.iter() {
                unsafe {
                    match obj_type {
                        ObjectType::Atomic => {}
                        ObjectType::Pointer => (*self.queue).push((*root, *obj_type)),
                        _ => {
                            if !self.thread_local_allocator.as_mut().unwrap().in_heap(*root) {
                                continue;
                            }
                            (*self.queue).push((*root, *obj_type))
                        }
                    }
                }
            }
        }
        #[cfg(feature = "llvm_stackmap")]
        {
            if USE_SHADOW_STACK.load(Ordering::Relaxed) {
                #[cfg(feature = "llvm_gc_plugin")]
                unsafe {
                    crate::shadow_stack::visitGCRoots(Self::mark_ptr_callback)
                };
            } else {
                // println!("{:?}", &STACK_MAP.map.borrow());
                let mut depth = 0;
                backtrace::trace(|frame| {
                    let addr = frame.ip() as *mut u8;
                    let const_addr = addr as *const u8;
                    let map = STACK_MAP.map.borrow();
                    let f = map.get(&const_addr);
                    // backtrace::resolve_frame(frame,
                    //     |s|
                    //     {
                    //         println!("{}: {:?} ip: {:p}, address: {:p}, sp: {:?}", depth, s.name(), frame.ip(), const_addr, frame.sp());
                    //     }
                    // );
                    if let Some(f) = f {
                        // println!("found fn in stackmap, f: {:?} sp: {:p}", f,frame.sp());
                        f.iter_roots().for_each(|(offset, _obj_type)| unsafe {
                            // println!("offset: {}", offset);
                            let sp = frame.sp() as *mut u8;
                            let root = sp.offset(offset as isize);
                            self.mark_ptr(root);
                        });
                    }
                    depth += 1;
                    true
                });
                STACK_MAP
                    .global_roots
                    .borrow()
                    .iter()
                    .for_each(|root| unsafe {
                        self.mark_ptr((*root) as usize as *mut u8);
                    });
            }
        }

        // iterate through queue and mark all reachable objects
        unsafe {
            // (*self.queue).extend(self.roots.iter().map(|(a,b)|(*a,*b)));
            while let Some((obj, obj_type)) = (*self.queue).pop() {
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

        let mut v = GC_COLLECTOR_COUNT.lock();
        let (count, mut waiting) = *v;
        waiting -= 1;
        *v = (count, waiting);
        // println!("gc {}: waiting: {}, count: {}", self.id, waiting, count);
        if waiting != 0 {
            GC_MARK_COND.wait_while(&mut v, |_| GC_MARKING.load(Ordering::Acquire));
        } else {
            GC_MARKING.store(false, Ordering::Release);
            GC_MARK_COND.notify_all();
            drop(v);
        }
    }

    /// # sweep
    ///
    /// since we did synchronization in mark, we don't need to do synchronization again in sweep
    pub fn sweep(&self) -> usize {
        GC_RUNNING.store(false, Ordering::Release);
        GC_SWEEPPING_NUM.fetch_add(1, Ordering::AcqRel);
        GC_SWEEPING.store(true, Ordering::Release);
        let used = unsafe {
            self.thread_local_allocator
                .as_mut()
                .unwrap()
                .sweep(self.mark_histogram)
        };
        self.status.borrow_mut().collect_threshold = (used as f64 * THRESHOLD_PROPORTION) as usize;
        let v = GC_SWEEPPING_NUM.fetch_sub(1, Ordering::AcqRel);
        if v - 1 == 0 {
            GC_SWEEPING.store(false, Ordering::Release);
        }
        used
    }

    /// # collect
    /// Collect garbage.
    pub fn collect(&self) -> (std::time::Duration, std::time::Duration) {
        let start_time = std::time::Instant::now();
        log::info!("gc {} collecting...", self.id);
        // self.print_stats();
        let mut status = self.status.borrow_mut();
        // println!("gc {} collecting... {}", self.id,status.bytes_allocated_since_last_gc);
        if status.collecting {
            return Default::default();
        }
        status.collecting = true;
        status.bytes_allocated_since_last_gc = 0;
        status.last_gc_time = std::time::SystemTime::now();
        drop(status);
        // evacuation pre process
        // 这个过程要在完成safepoint同步之前完成，因为在驱逐的情况下
        // 他要设置每个block是否是驱逐目标
        // 如果设置的时候别的线程的mark已经开始，那么将无法保证能够纠正所有被驱逐的指针
        unsafe {
            if ENABLE_EVA.load(Ordering::Relaxed)
                && self.thread_local_allocator.as_mut().unwrap().should_eva()
            {
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
                log::info!("gc {} eva threshold:{}", self.id, eva_threshold);
                // 根据驱逐阀域标记每个block是否是驱逐目标
                self.thread_local_allocator
                    .as_mut()
                    .unwrap()
                    .set_eva_threshold(eva_threshold);
            }
            (*self.mark_histogram).clear();
        }
        let time = std::time::Instant::now();
        unsafe {
            self.thread_local_allocator
                .as_mut()
                .unwrap()
                .set_collect_mode(true);
        }
        self.mark();
        let mark_time = time.elapsed();
        let _used = self.sweep();
        let sweep_time = time.elapsed() - mark_time;
        unsafe {
            self.thread_local_allocator
                .as_mut()
                .unwrap()
                .set_collect_mode(false);
        }
        log::info!(
            "gc {} collect done, mark: {:?}, sweep: {:?}, used heap size: {} byte, total: {:?}",
            self.id,
            mark_time,
            sweep_time,
            _used,
            start_time.elapsed()
        );
        let mut status = self.status.borrow_mut();
        status.collecting = false;
        (mark_time, sweep_time)
        // (Default::default(), Default::default())
    }

    /// # get_bigobjs_size
    pub fn get_bigobjs_size(&self) -> usize {
        unsafe {
            self.thread_local_allocator
                .as_ref()
                .unwrap()
                .get_bigobjs_size()
        }
    }
}

#[cfg(test)]
#[cfg(feature = "shadow_stack")]
mod tests {
    use std::{mem::size_of, ptr::null_mut, thread::sleep, time::Duration};

    use lazy_static::lazy_static;
    use parking_lot::Mutex;
    use rand::random;
    use rustc_hash::FxHashSet;

    use crate::{gc_disable_auto_collect, no_gc_thread, round_n_up, BLOCK_SIZE, SPACE};

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
    extern "C" fn gctest_vtable(
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

    #[repr(C)]
    struct GCTestBigObj {
        _vtable: VtableFunc,
        b: *mut GCTestBigObj,
        _arr: [u8; BLOCK_SIZE],
        d: *mut GCTestBigObj,
    }
    const BIGOBJ_ALLOC_SIZE: usize = round_n_up!(size_of::<GCTestBigObj>() + 16, 128);
    extern "C" fn gctest_vtable_big(
        ptr: *mut u8,
        gc: &Collector,
        mark_ptr: VisitFunc,
        _mark_complex: VisitFunc,
        _mark_trait: VisitFunc,
    ) {
        let obj = ptr as *mut GCTestBigObj;
        unsafe {
            mark_ptr(gc, (&mut (*obj).b) as *mut *mut GCTestBigObj as *mut u8);
            mark_ptr(gc, (&mut (*obj).d) as *mut *mut GCTestBigObj as *mut u8);
        }
    }
    #[test]
    fn test_basic_multiple_thread_gc() {
        let _lock = THE_RESOURCE.lock();
        let mut handles = vec![];
        gc_disable_auto_collect();
        no_gc_thread();
        for _ in 0..10 {
            let t = std::thread::spawn(|| {
                SPACE.with(|gc| unsafe {
                    let mut gc = gc.borrow_mut();
                    println!("thread1 gcid = {}", gc.get_id());
                    let mut a =
                        gc.alloc(size_of::<GCTestObj>(), ObjectType::Complex) as *mut GCTestObj;
                    let rustptr = (&mut a) as *mut *mut GCTestObj as *mut u8;
                    (*a).b = null_mut();
                    (*a).c = 1;
                    (*a).d = null_mut();
                    (*a).e = null_mut();
                    (*a)._vtable = gctest_vtable;
                    gc.add_root(rustptr, ObjectType::Pointer);
                    let b = gc.alloc(size_of::<GCTestObj>(), ObjectType::Complex) as *mut GCTestObj;
                    gc.print_stats();
                    (*a).b = b;
                    (*b)._vtable = gctest_vtable;
                    (*b).c = 2;
                    (*b).d = null_mut();
                    (*b).e = null_mut();
                    (*b).b = null_mut();
                    let size1 = gc.get_size();
                    let time = std::time::Instant::now();
                    gc.collect();
                    println!("gc{} gc time = {:?}", gc.get_id(), time.elapsed());
                    let size2 = gc.get_size();
                    assert_eq!(size2, LINE_SIZE_OBJ * 2, "gc {}", gc.get_id());
                    assert_eq!(size1, size2, "gc {}", gc.get_id());
                    let d = gc.alloc(size_of::<u64>(), ObjectType::Atomic) as *mut u64;
                    (*b).d = d;
                    (*d) = 3;
                    gc.collect();
                    let size3 = gc.get_size();
                    assert_eq!(size3, LINE_SIZE_OBJ * 3);
                    assert!(size3 > size2, "gc {}", gc.get_id());
                    (*a).d = d;
                    gc.collect();
                    let size4 = gc.get_size();
                    assert_eq!(size3, size4, "gc {}", gc.get_id());
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
                    (*a).b = null_mut();
                    (*a).c = 1;
                    (*a).d = null_mut();
                    (*a).e = null_mut();
                    (*a)._vtable = gctest_vtable;
                    let rustptr = (&mut a) as *mut *mut GCTestObj as *mut u8;
                    gc.add_root(rustptr, ObjectType::Pointer);
                    let b = gc.alloc(size_of::<GCTestObj>(), ObjectType::Complex) as *mut GCTestObj;
                    (*a).b = b;
                    (*a).c = 1;
                    (*a)._vtable = gctest_vtable;
                    (*b)._vtable = gctest_vtable;
                    (*b).c = 2;
                    (*b).d = null_mut();
                    (*b).e = null_mut();
                    (*b).b = null_mut();
                    let size1 = gc.get_size();
                    assert_eq!(size1, 2 * LINE_SIZE_OBJ);
                    // drop(gc);
                    // no_gc_thread();
                });
            });
            handles.push(t);
        }
        for h in handles {
            h.join().unwrap();
        }
    }

    #[test]
    fn test_big_obj_gc() {
        gc_disable_auto_collect();
        let _lock = THE_RESOURCE.lock();
        SPACE.with(|gc| unsafe {
            let mut gc = gc.borrow_mut();
            println!("thread1 gcid = {}", gc.get_id());
            let mut a =
                gc.alloc(size_of::<GCTestBigObj>(), ObjectType::Complex) as *mut GCTestBigObj;
            let b = gc.alloc(size_of::<GCTestBigObj>(), ObjectType::Complex) as *mut GCTestBigObj;
            (*a).b = b;
            (*a).d = null_mut();
            (*a)._vtable = gctest_vtable_big;
            (*b)._vtable = gctest_vtable_big;
            let rustptr = (&mut a) as *mut *mut GCTestBigObj as *mut u8;
            gc.add_root(rustptr, ObjectType::Pointer);
            let size1 = gc.get_bigobjs_size();
            assert_eq!(size1, 2 * BIGOBJ_ALLOC_SIZE);
            let time = std::time::Instant::now();
            gc.collect(); // 不回收，剩余 a b
            println!("gc{} gc time = {:?}", gc.get_id(), time.elapsed());
            let size2 = gc.get_bigobjs_size();
            assert_eq!(size1, size2);
            (*a).b = a;
            gc.collect(); // 回收，剩余 a
            let size3 = gc.get_bigobjs_size();
            assert!(size3 < size2);
            gc.remove_root(rustptr);

            gc.collect(); // 回收，不剩余
            let size4 = gc.get_bigobjs_size();
            assert_eq!(0, size4);
            let mut a =
                gc.alloc(size_of::<GCTestBigObj>(), ObjectType::Complex) as *mut GCTestBigObj;
            let b = gc.alloc(size_of::<GCTestBigObj>(), ObjectType::Complex) as *mut GCTestBigObj;
            let c = gc.alloc(size_of::<GCTestBigObj>(), ObjectType::Complex) as *mut GCTestBigObj;
            (*a).b = b;
            (*a).d = c;
            (*a)._vtable = gctest_vtable_big;
            (*b)._vtable = gctest_vtable_big;
            (*c)._vtable = gctest_vtable_big;
            let rustptr = (&mut a) as *mut *mut GCTestBigObj as *mut u8;
            gc.add_root(rustptr, ObjectType::Pointer);
            //  32896       32896       32896
            // |  b  | <-- |  a  | --> |  c  |
            //               ^ rustptr
            let size1 = gc.get_bigobjs_size();
            assert_eq!(size1, 3 * BIGOBJ_ALLOC_SIZE);
            (*a).b = null_mut();
            (*a).d = null_mut();
            gc.collect(); // 回收，剩余 a
            gc.remove_root(rustptr);
            gc.collect(); // 回收，不剩余, b a merge，a c merge。
        });
    }

    unsafe fn alloc_test_obj(gc: &mut Collector) -> *mut GCTestObj {
        let a = gc.alloc(size_of::<GCTestObj>(), ObjectType::Complex) as *mut GCTestObj;
        a.write(GCTestObj {
            _vtable: gctest_vtable,
            b: null_mut(),
            c: 0,
            d: null_mut(),
            e: null_mut(),
        });
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
        gc_disable_auto_collect();
        // let _lock = THE_RESOURCE.lock();
        SPACE.with(|gc| unsafe {
            // 睡眠一秒保证所有线程创建
            let mut gc = gc.borrow_mut();
            println!("thread1 gcid = {}", gc.get_id());
            sleep(Duration::from_secs(1));
            let mut first_obj = alloc_test_obj(&mut gc);
            println!("first_obj = {:p}", first_obj);
            let rustptr = (&mut first_obj) as *mut *mut GCTestObj as *mut u8;
            gc.add_root(rustptr, ObjectType::Pointer);
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
            // evacuation might be triggered, so it mat not be zero
            // let size4 = gc.get_size();
            // assert_eq!(size4, 0);
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
        /// gc测试不能并发进行，需要加锁
        static ref THE_RESOURCE: Mutex<()> = Mutex::new(());
    }

    #[test]
    fn test_complecated_single_thread_gc() {
        gc_disable_auto_collect();
        // 这个测试和test_complecated_multiple_thread_gc不能同时跑，用了全局变量会相互干扰
        let _lock = THE_RESOURCE.lock();
        TEST_OBJ_QUEUE.lock().clear();
        let re = single_thread(1000);
        no_gc_thread();
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
        gc_disable_auto_collect();
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
            for s in re.set2.drain() {
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
