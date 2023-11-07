use std::{
    cell::RefCell,
    ptr::drop_in_place,
    sync::atomic::{AtomicBool, AtomicPtr, Ordering},
};

use libc::malloc;
use vector_map::VecMap;

#[cfg(feature = "llvm_stackmap")]
use crate::STACK_MAP;
use crate::{
    allocator::{GlobalAllocator, ThreadLocalAllocator},
    block::{Block, LineHeaderExt, ObjectType},
    gc_is_auto_collect_enabled, spin_until, HeaderExt, ENABLE_EVA, GC_COLLECTOR_COUNT, GC_ID,
    GC_MARKING, GC_MARK_COND, GC_RUNNING, GC_STW_COUNT, GC_SWEEPING, GC_SWEEPPING_NUM,
    GLOBAL_ALLOCATOR, LINE_SIZE, NUM_LINES_PER_BLOCK, THRESHOLD_PROPORTION, USE_SHADOW_STACK,
};

/// # Collector
/// The collector is responsible for collecting garbage. It is the entry point for
/// the garbage collection process. It is also responsible for allocating new
/// blocks and objects.
///
/// Each thread has a collector associated with it. The collector is thread-local.
///
/// ## Fields
/// * `thread_local_allocator` - thread-local allocator
/// * `roots` - gc roots
/// * `queue` - gc queue
/// * `id` - collector id
/// * `mark_histogram` - mark histogram
/// * `status` - collector status
pub struct Collector {
    thread_local_allocator: *mut ThreadLocalAllocator,
    // #[cfg(feature = "shadow_stack")]
    roots: rustc_hash::FxHashMap<*mut u8, ObjectType>,
    queue: *mut Vec<(*mut u8, ObjectType)>,
    id: usize,
    mark_histogram: *mut VecMap<usize, usize>,
    status: RefCell<CollectorStatus>,
    frames_list: AtomicPtr<Vec<(*mut libc::c_void, *mut libc::c_void)>>,
    shadow_thread_running: AtomicBool,
}

struct CollectorStatus {
    /// in bytes
    collect_threshold: usize,
    /// in bytes
    bytes_allocated_since_last_gc: usize,
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
                // #[cfg(feature = "shadow_stack")]
                roots: rustc_hash::FxHashMap::default(),
                id,
                mark_histogram: memvecmap,
                queue: memqueue,
                status: RefCell::new(CollectorStatus {
                    collect_threshold: 1024,
                    bytes_allocated_since_last_gc: 0,
                    collecting: false,
                }),
                frames_list: AtomicPtr::default(),
                shadow_thread_running: AtomicBool::new(false),
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
        if !self.frames_list.load(Ordering::SeqCst).is_null() {
            panic!("gc stucked, can not alloc")
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
                return self
                    .thread_local_allocator
                    .as_mut()
                    .unwrap()
                    .alloc(size, obj_type);
            }
            ptr
        }
    }

    /// # add_root
    /// Add a root to the collector.
    ///
    /// ## Parameters
    /// * `root` - root
    /// * `size` - root size
    // #[cfg(feature = "shadow_stack")]
    pub fn add_root(&mut self, root: *mut u8, obj_type: ObjectType) {
        self.roots.insert(root, obj_type);
    }

    /// # remove_root
    /// Remove a root from the collector.
    ///
    /// ## Parameters
    /// * `root` - root
    // #[cfg(feature = "shadow_stack")]
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
    unsafe fn correct_ptr(&self, ptr: *mut u8, offset: isize) {
        let ptr = ptr as *mut AtomicPtr<*mut u8>;
        let new_ptr = *(*ptr).load(Ordering::SeqCst);
        let ptr = ptr as *mut *mut u8;
        debug_assert!(!new_ptr.is_null());
        log::trace!("gc {} correct ptr {:p} to {:p}", self.id, ptr, new_ptr);
        *ptr = new_ptr.offset(offset);
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

        let ptr = *(ptr as *mut *mut u8);
        // println!("mark ptr {:p} -> {:p}", father, ptr);
        // mark it if it is in heap
        if self.thread_local_allocator.as_mut().unwrap().in_heap(ptr) {
            // println!("mark ptr succ {:p} -> {:p}", father, ptr);
            let block = Block::from_obj_ptr(ptr);
            let is_candidate = block.is_eva_candidate();
            let mut head = block.get_head_ptr(ptr);
            let offset_from_head = ptr.offset_from(head);
            if !is_candidate {
                block.marked = true;
            } else {
                let (line_header, _) = block.get_line_header_from_addr(head);
                if line_header.get_forwarded() {
                    self.correct_ptr(father, offset_from_head);
                    return;
                }
            }
            let (line_header, idx) = block.get_line_header_from_addr(head);
            if line_header.get_marked() {
                return;
            }
            // 若此block是待驱逐对象
            if is_candidate {
                let atomic_ptr = head as *mut AtomicPtr<u8>;
                let old_loaded = (*atomic_ptr).load(Ordering::SeqCst);
                let obj_line_size = line_header.get_obj_line_size(idx, Block::from_obj_ptr(head));
                let new_ptr = self.alloc(obj_line_size * LINE_SIZE, line_header.get_obj_type());
                if !new_ptr.is_null() {
                    let new_block = Block::from_obj_ptr(new_ptr);
                    let (new_line_header, _) = new_block.get_line_header_from_addr(new_ptr);
                    // 将数据复制到新的地址
                    core::ptr::copy_nonoverlapping(head, new_ptr, obj_line_size * LINE_SIZE);
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
                        log::trace!("gc {}: eva {:p} to {:p}", self.id, head, new_ptr);
                        new_line_header.set_marked(true);
                        line_header.set_forwarded(true);
                        new_block.marked = true;
                        head = new_ptr;
                        self.correct_ptr(father, offset_from_head);
                    } else {
                        // 期间别的线程驱逐了它
                        spin_until!(line_header.get_forwarded());
                        self.correct_ptr(father, offset_from_head);
                        return;
                    }
                }
            }
            line_header.set_marked(true);
            let obj_type = line_header.get_obj_type();
            match obj_type {
                ObjectType::Atomic => {}
                _ => (*self.queue).push((head, obj_type)),
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
        STUCK_GCED.store(false, Ordering::SeqCst);

        // #[cfg(feature = "shadow_stack")]
        {
            for (root, obj_type) in self.roots.iter() {
                unsafe {
                    match obj_type {
                        ObjectType::Atomic => {}
                        ObjectType::Pointer => (*self.queue).push((*root, *obj_type)),
                        _ => (*self.queue).push((*root, *obj_type)),
                    }
                }
            }
        }
        log::trace!("gc {}: marking...", self.id);
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
                let fl = self.frames_list.load(Ordering::SeqCst);
                let frames = if !fl.is_null() {
                    log::trace!("gc {}: tracing stucked frames", self.id);
                    unsafe { fl.as_mut().unwrap().clone() }
                } else {
                    let mut frames: Vec<(*mut libc::c_void, *mut libc::c_void)> = vec![];
                    backtrace::trace(|frame| {
                        frames.push((frame.ip(), frame.sp()));
                        true
                    });
                    frames
                };

                frames.iter().for_each(|(ip, sp)| unsafe {
                    let addr = *ip as *mut u8;
                    let const_addr = addr as *const u8;
                    let map = STACK_MAP.map.as_ref().unwrap();
                    let f = map.get(&const_addr);
                    // backtrace::resolve_frame(frame,
                    //     |s|
                    //     {
                    //         println!("{}: {:?} ip: {:p}, address: {:p}, sp: {:?}", depth, s.name(), frame.ip(), const_addr, frame.sp());
                    //     }
                    // );
                    if let Some(f) = f {
                        // println!("found fn in stackmap, f: {:?} sp: {:p}", f,frame.sp());
                        f.iter_roots().for_each(|(offset, _obj_type)| {
                            // println!("offset: {}", offset);
                            let sp = *sp as *mut u8;
                            let root = sp.offset(offset as isize);
                            self.mark_ptr(root);
                        });
                    }
                    depth += 1;
                });
                self.mark_globals();
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
            GC_RUNNING.store(false, Ordering::Release);
            drop(v);
        }
    }

    #[cfg(feature = "llvm_stackmap")]
    fn mark_globals(&self) {
        unsafe {
            STACK_MAP
                .global_roots
                .as_mut()
                .unwrap()
                .iter()
                .for_each(|root| {
                    self.mark_ptr((*root) as usize as *mut u8);
                });
        }
    }

    /// # sweep
    ///
    /// since we did synchronization in mark, we don't need to do synchronization again in sweep
    pub fn sweep(&self) -> usize {
        GC_SWEEPPING_NUM.fetch_add(1, Ordering::AcqRel);
        GC_SWEEPING.store(true, Ordering::Release);
        log::trace!("gc {}: sweeping...", self.id);
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

    pub fn safepoint(&self) {
        if GC_RUNNING.load(Ordering::Acquire) {
            self.collect();
        }
    }

    /// # collect
    /// Collect garbage.
    pub fn collect(&self) {
        // let start_time = std::time::Instant::now();
        // log::info!("gc {} collecting...", self.id);
        // if STUCK_GCED.compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst).is_ok() {
        //     for (c1,f) in FRAMES_LIST.0.lock().borrow().iter() {
        //         unsafe {
        //             let c = c1.as_mut().unwrap();
        //             c.frames_list = Some(f.clone());
        //             GLOBAL_ALLOCATOR.0.as_ref().unwrap().pool.execute(|| {
        //                 c.collect();
        //                 c.frames_list = None;
        //             });
        //         }
        //     }
        // }
        log::info!(
            "gc {} collecting... stucked: {}",
            self.id,
            !self.frames_list.load(Ordering::SeqCst).is_null()
        );
        // self.print_stats();
        let mut status = self.status.borrow_mut();
        // println!("gc {} collecting... {}", self.id,status.bytes_allocated_since_last_gc);
        if status.collecting {
            return Default::default();
        }
        status.collecting = true;
        status.bytes_allocated_since_last_gc = 0;
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
        // let time = std::time::Instant::now();
        unsafe {
            self.thread_local_allocator
                .as_mut()
                .unwrap()
                .set_collect_mode(true);
        }
        self.mark();
        // let mark_time = time.elapsed();
        let _used = self.sweep();
        // let sweep_time = time.elapsed() - mark_time;
        unsafe {
            self.thread_local_allocator
                .as_mut()
                .unwrap()
                .set_collect_mode(false);
        }
        log::info!(
            "gc {} collect done, used heap size: {} byte",
            self.id,
            _used,
            // start_time.elapsed()
        );
        let mut status = self.status.borrow_mut();
        status.collecting = false;
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

    pub fn stuck(&mut self) {
        log::trace!("gc {}: stucking...", self.id);
        let mut frames: Box<Vec<(*mut libc::c_void, *mut libc::c_void)>> = Box::default();
        backtrace::trace(|frame| {
            frames.push((frame.ip(), frame.sp()));
            true
        });
        unsafe {
            let ptr = Box::leak(frames) as *mut _;
            self.frames_list.store(ptr, Ordering::SeqCst);
            let c: *mut Collector = self as *mut _;
            let c = c.as_mut().unwrap();
            c.shadow_thread_running.store(true, Ordering::SeqCst);
            GLOBAL_ALLOCATOR.0.as_ref().unwrap().pool.execute(move || {
                log::info!("gc {}: stucked, waiting for unstuck...", c.id);
                loop {
                    let mut i: i32 = 0;
                    core::hint::spin_loop();
                    let (re, _) = i.overflowing_add(1);
                    i = re;
                    if i % 100 == 0 {
                        std::thread::yield_now();
                    }
                    // sleep(std::time::Duration::from_millis(100));
                    if c.frames_list.load(Ordering::SeqCst).is_null() {
                        log::trace!("gc {}: unstucking break...", c.id);
                        c.shadow_thread_running.store(false, Ordering::SeqCst);
                        break;
                    } else {
                        c.safepoint();
                    }
                }
            });
        }

        // FRAMES_LIST.0.lock().borrow_mut().insert( self as _,frames);
    }

    pub fn unstuck(&mut self) {
        log::trace!("gc {}: unstucking...", self.id);
        let old = self
            .frames_list
            .swap(std::ptr::null_mut(), Ordering::SeqCst);
        if !old.is_null() {
            unsafe {
                drop_in_place(old as *mut Vec<(*mut libc::c_void, *mut libc::c_void)>);
            }
        }
        // wait until the shadow thread exit
        spin_until!(!self.shadow_thread_running.load(Ordering::SeqCst));
    }
}

#[cfg(test)]
#[cfg(feature = "shadow_stack")]
mod tests;

static STUCK_GCED: AtomicBool = AtomicBool::new(false);

unsafe impl Sync for Collector {}
unsafe impl Send for Collector {}
