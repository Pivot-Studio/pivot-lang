use std::{
    cell::RefCell,
    ptr::drop_in_place,
    sync::{
        atomic::{AtomicBool, AtomicPtr, Ordering},
        Arc,
    },
};

use libc::malloc;
use parking_lot::{Condvar, Mutex};
use rustc_hash::FxHashMap;

#[cfg(feature = "llvm_stackmap")]
use crate::STACK_MAP;
use crate::{
    allocator::{GlobalAllocator, ThreadLocalAllocator},
    block::{Block, LineHeaderExt, ObjectType},
    gc_is_auto_collect_enabled, spin_until, HeaderExt, ENABLE_EVA, GC_COLLECTOR_COUNT, GC_ID,
    GC_MARKING, GC_MARK_COND, GC_RUNNING, GC_STW_COUNT, GC_SWEEPING, GC_SWEEPPING_NUM,
    GLOBAL_ALLOCATOR, LINE_SIZE, NUM_LINES_PER_BLOCK, THRESHOLD_PROPORTION, USE_SHADOW_STACK,
};

fn get_ip_from_sp(sp: *mut u8) -> *mut u8 {
    let sp = sp as *mut *mut u8;
    // check align
    unsafe { 
        let p = sp.offset(-1);
        if p as usize % 8 == 0 {
            *p
        }else {
            std::ptr::null_mut()
        }
     }
}

fn walk_gc_frames(sp: *mut u8, mut walker: impl FnMut(*mut u8, i32, ObjectType)) {
    let mut sp = sp;
    loop {
        let ip = get_ip_from_sp(sp);
        let frame = unsafe { STACK_MAP.map.as_ref().unwrap().get(&ip.cast_const()) };
        if let Some(frame) = frame {
            for o in frame.iter_roots() {
                walker(sp, o, ObjectType::Pointer);
            }

            sp = unsafe {
                #[cfg(target_arch = "aarch64")]
                {
                    sp.offset(frame.frame_size as _)
                }
                #[cfg(target_arch = "x86_64")]
                {
                    sp.offset(frame.frame_size as isize + 8)
                }
            };
        } else {
            break;
        }
    }
}

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
    #[cfg(feature = "shadow_stack")]
    roots: rustc_hash::FxHashMap<*mut u8, ObjectType>,
    queue: *mut Vec<(*mut u8, ObjectType)>,
    id: usize,
    mark_histogram: *mut FxHashMap<usize, usize>,
    status: RefCell<CollectorStatus>,
    frames_list: AtomicPtr<Vec<(*mut libc::c_void, *mut libc::c_void)>>,
    shadow_thread_running: AtomicBool,
    live_set: RefCell<FxHashMap<u64, *mut u8>>,
    coro_stacks: FxHashMap<*mut u8, Box<Vec<(*mut libc::c_void, *mut libc::c_void)>>>,
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
            let memvecmap = malloc(core::mem::size_of::<FxHashMap<usize, usize>>())
                .cast::<FxHashMap<usize, usize>>();
            memvecmap.write(FxHashMap::with_capacity_and_hasher(
                NUM_LINES_PER_BLOCK,
                Default::default(),
            ));
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
                    collecting: false,
                }),
                frames_list: AtomicPtr::default(),
                shadow_thread_running: AtomicBool::new(false),
                live_set: RefCell::new(FxHashMap::default()),
                coro_stacks: FxHashMap::default(),
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
    /// This function is considered as a
    /// GC safepoint, which means it may trigger a GC.
    ///
    /// ## Parameters
    /// * `size` - object size
    /// * `obj_type` - object type
    ///
    /// ## Return
    /// * `ptr` - object pointer
    pub fn alloc(&self, size: usize, obj_type: ObjectType) -> *mut u8 {
        self.alloc_fast_unwind(size, obj_type, std::ptr::null_mut())
    }

    /// # alloc_fast_unwind
    ///
    /// Allocate a new object, if it trigger a GC, it will use stack pointer to walk gc frames.
    ///
    /// For more information, see [mark_fast_unwind](Collector::mark_fast_unwind)
    pub fn alloc_fast_unwind(&self, size: usize, obj_type: ObjectType, sp: *mut u8) -> *mut u8 {
        if !self.frames_list.load(Ordering::SeqCst).is_null() {
            panic!("gc stucked, can not alloc")
        }
        if size == 0 {
            panic!("alloc size can not be zero");
        }
        if gc_is_auto_collect_enabled() {
            self.safepoint_fast_unwind(sp);
            let mut status = self.status.borrow_mut();
            status.bytes_allocated_since_last_gc += ((size - 1) / LINE_SIZE + 1) * LINE_SIZE;
        }
        let ptr = self.alloc_no_collect(size, obj_type);
        if ptr.is_null() {
            self.collect_fast_unwind(sp);
            return unsafe {
                self.thread_local_allocator
                    .as_mut()
                    .unwrap()
                    .alloc(size, obj_type)
            };
        }
        ptr
    }

    /// # safepoint_fast_unwind
    ///
    /// Safepoint, if it trigger a GC, it will use stack pointer to walk gc frames.
    ///
    /// For more information, see [mark_fast_unwind](Collector::mark_fast_unwind)
    pub fn safepoint_fast_unwind(&self, sp: *mut u8) {
        if GC_RUNNING.load(Ordering::Acquire)
            || (unsafe { self.thread_local_allocator.as_mut().unwrap().should_gc() })
        {
            self.collect_fast_unwind(sp);
        }
        let status = self.status.borrow();
        if status.collect_threshold < status.bytes_allocated_since_last_gc {
            drop(status);
            self.collect_fast_unwind(sp);
        } else {
            drop(status);
        }
    }

    /// # alloc_no_collect
    ///
    /// Allocate a new object without possibility triggering a GC.
    pub fn alloc_no_collect(&self, size: usize, obj_type: ObjectType) -> *mut u8 {
        if size == 0 {
            return std::ptr::null_mut();
        }
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

    /// # correct_ptr
    ///
    /// used to correct forward pointer in `evacuation`
    ///
    /// ## 一般情况
    ///
    /// 原本
    ///
    /// ```no_run
    /// ptr -> heap_ptr -> value
    /// ```
    ///
    /// evacuate之后
    ///
    /// ```no_run
    /// ptr -> heap_ptr -> new_ptr(forwarded) -> value
    /// ```
    ///
    /// 现在纠正为
    ///
    /// ```no_run
    /// ptr -> new_ptr(forwarded) -> value
    /// ```
    ///
    /// ## 特殊情况
    ///
    /// 有时候gc接触的指针不是原本的堆指针，他有个offset（derived pointer）
    /// 这种情况下我们需要找到原本的堆指针，然后加上一个offset，这样才能纠正
    ///
    /// ## Parameters
    ///
    /// * `ptr` - pointer which points to the evacuated heap pointer
    /// * `offset` - offset from derived pointer to heap pointer
    ///
    /// ## Return
    ///
    /// * `ptr` - corrected pointer
    unsafe fn correct_ptr(&self, ptr: *mut u8, offset: isize) -> *mut u8 {
        let father = ptr as *mut *mut u8;
        let ptr = AtomicPtr::new((*father).offset(-offset) as *mut *mut u8);
        let ptr = ptr.load(Ordering::SeqCst);
        let new_ptr = *ptr;
        debug_assert!(!new_ptr.is_null(), "{:p}", new_ptr);
        let np = new_ptr.offset(offset);
        log::trace!(
            "gc {} correct ptr in {:p} from  {:p} to {:p}",
            self.id,
            father,
            *father,
            np
        );
        *father = np;
        np
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
            // eprintln!("invalid pointer: {:p}", ptr);
            return;
        }

        let ptr = *(ptr as *mut *mut u8);
        if ptr.is_null() {
            return;
        }
        // eprintln!("mark ptr {:p} -> {:p}", father, ptr);
        // mark it if it is in heap
        if self.thread_local_allocator.as_mut().unwrap().in_heap(ptr) {
            // println!("mark ptr succ {:p} -> {:p}", father, ptr);
            let block_p: *mut Block = Block::from_obj_ptr(ptr) as *mut _;

            let block = &mut *block_p;
            let is_candidate = block.is_eva_candidate();
            let mut head = block.get_head_ptr(ptr);
            if head.is_null() {
                return;
            }
            let offset_from_head = ptr.offset_from(head);
            let (line_header, idx) = block.get_line_header_from_addr(head);
            if !is_candidate {
                let block = &mut *block_p;
                block.marked = true;

                if line_header.get_marked() {
                    return;
                }
            } else {
                // evacuation logic
                let (forward, h) = line_header.get_forward_start();
                let old_h = h;
                // if forward is true or cas is failed, then it's forwarded by other thread.
                let shall_i_forward = !forward && line_header.forward_cas(old_h);
                if !shall_i_forward {
                    spin_until!(line_header.get_forwarded());
                    self.correct_ptr(father, offset_from_head);
                    return;
                }

                // otherwise, we shall forward it
                let atomic_ptr = head as *mut AtomicPtr<u8>;

                let obj_line_size = line_header.get_obj_line_size(idx, Block::from_obj_ptr(head));
                let new_ptr =
                    self.alloc_no_collect(obj_line_size * LINE_SIZE, line_header.get_obj_type());
                if !new_ptr.is_null() {
                    let new_block = Block::from_obj_ptr(new_ptr);
                    let (new_line_header, _) = new_block.get_line_header_from_addr(new_ptr);
                    // 将数据复制到新的地址
                    core::ptr::copy_nonoverlapping(head, new_ptr, obj_line_size * LINE_SIZE);
                    // core::ptr::copy_nonoverlapping(line_header as * const u8, new_line_header as * mut u8, line_size);

                    // 将新指针写入老数据区开头
                    (*atomic_ptr).store(new_ptr, Ordering::SeqCst);
                    // eprintln!("gc {}: eva {:p} to {:p}", self.id, head, new_ptr);
                    log::trace!("gc {}: eva {:p} to {:p}", self.id, head, new_ptr);
                    new_line_header.set_marked(true);
                    new_block.marked = true;
                    head = new_ptr;
                    self.correct_ptr(father, offset_from_head);
                    // 成功驱逐
                    line_header.set_forwarded();
                } else {
                    // 驱逐失败
                    panic!("gc: OOM during evacuation");
                }
            }

            line_header.set_marked(true);
            let obj_type = line_header.get_obj_type();
            match obj_type {
                ObjectType::Atomic => {}
                // ObjectType::Trait => self.mark_trait(head),
                // ObjectType::Complex => self.mark_complex(head),
                // ObjectType::Pointer => self.mark_ptr(head),
                _ => (*self.queue).push((head, obj_type)),
            }
        }
        // mark it if it is a big object
        else if self
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
        if ptr.is_null() {
            return;
        }
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
        // the trait is not init
        if ptr.is_null() {
            return;
        }
        let loaded = ptr as *mut *mut u8;
        let ptr = loaded.offset(1);
        self.mark_ptr(ptr as *mut u8);
    }
    pub fn keep_live(&self, gc_ptr: *mut u8) -> u64 {
        let len = self.live_set.borrow().len();
        self.live_set.borrow_mut().insert(len as _, gc_ptr);
        len as _
    }
    pub fn rm_live(&self, handle: u64) {
        self.live_set.borrow_mut().remove(&handle);
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
    ///
    /// mark using backtrace-rs
    ///
    /// for more information, see [mark_fast_unwind](Collector::mark_fast_unwind)
    pub fn mark(&self) {
        self.mark_fast_unwind(std::ptr::null_mut());
    }

    fn mark_stack_offset(&self, sp: *mut u8, offset: i32, obj_type: ObjectType) {
        unsafe {
            let root = sp.offset(offset as isize);
            if root.is_null() {
                return;
            }
            match obj_type {
                ObjectType::Atomic => panic!("stack root shall never be atomic"),
                ObjectType::Trait => self.mark_trait(*(root as *mut *mut u8)),
                ObjectType::Complex => self.mark_complex(*(root as *mut *mut u8)),
                ObjectType::Pointer => self.mark_ptr(root),
            }
        }
    }

    /// # mark_fast_unwind
    ///
    ///
    /// From gc roots, mark all reachable objects.
    ///
    /// this mark function is __precise__
    ///
    /// ## Parameters
    ///
    /// * `sp` - stack pointer
    ///
    /// if the stack pointer is null, then it will use backtrace-rs to walk
    /// the stack.
    ///
    /// Stack walk using sp is lock-free and more precise(it only walks gc frames).
    ///
    /// ## Safety
    ///
    /// The fast version relies heavily on llvm generated stackmap, calling convention and
    /// made some assumptions on the stack layout, which may not work on some
    /// platforms. A known issue is it breaks on [linux x86_64][1] with
    /// late machine code optimization enabled(in llvm 16).
    ///
    /// [1]:https://github.com/llvm/llvm-project/issues/75162
    ///
    /// The backtrace-rs version works on most platforms in aot mode, but it
    /// failed on [macos aarch64 in jit mode][2], as there's a issue for llvm
    /// to emit __eh_frame section.
    ///
    /// [2]:https://github.com/llvm/llvm-project/issues/49036
    pub fn mark_fast_unwind(&self, sp: *mut u8) {
        let mutex = STUCK_MUTEX.lock();
        GC_RUNNING.store(true, Ordering::Release);
        STUCK_COND.notify_all();
        drop(mutex);

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
            unsafe {
                self.thread_local_allocator
                    .as_mut()
                    .unwrap()
                    .get_more_works();
            }
            drop(v);
        }

        #[cfg(feature = "shadow_stack")]
        {
            for (root, obj_type) in self.roots.iter() {
                unsafe {
                    match obj_type {
                        ObjectType::Atomic => {}
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
                let fl = self.frames_list.load(Ordering::SeqCst);
                if !fl.is_null() {
                    log::trace!("gc {}: tracing stucked frames", self.id);
                    self.mark_gc_frames(unsafe { fl.as_mut().unwrap().clone() });
                } else if sp.is_null() {
                    // use backtrace.rs or stored frames
                    let mut frames: Vec<(*mut libc::c_void, *mut libc::c_void)> = vec![];
                    backtrace::trace(|frame| {
                        frames.push((frame.ip(), frame.sp()));
                        true
                    });

                    self.mark_gc_frames(frames);
                } else {
                    walk_gc_frames(sp, |sp, offset, obj_type| {
                        self.mark_stack_offset(sp, offset, obj_type);
                    });
                }
                self.mark_globals();
                self.mark_coro_stacks();
            }
        }

        // iterate through queue and mark all reachable objects
        unsafe {
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
        let mut lives = vec![];
        for (i, live) in self.live_set.borrow().iter() {
            unsafe {
                let p = live as *const _ as _;
                self.mark_ptr(p);
                let p = p as *mut *mut u8;
                lives.push((*i, *p));
            }
        }
        self.live_set.borrow_mut().clear();
        for (i, live) in lives {
            self.live_set.borrow_mut().insert(i, live);
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
            GLOBAL_MARK_FLAG.store(false, Ordering::Release);
            GC_MARK_COND.notify_all();
            GC_RUNNING.store(false, Ordering::Release);
            drop(v);
        }
    }

    /// # mark_gc_frames
    ///
    /// mark gc frames
    ///
    /// ## Parameters
    ///
    /// * `frames` - gc frames, each frame is a tuple of (ip, sp)
    fn mark_gc_frames(&self, frames: Vec<(*mut libc::c_void, *mut libc::c_void)>) {
        frames.iter().for_each(|(ip, sp)| unsafe {
            let addr = *ip as *mut u8;
            let const_addr = addr as *const u8;
            let map = STACK_MAP.map.as_ref().unwrap();
            let f = map.get(&const_addr);
            if let Some(f) = f {
                f.iter_roots().for_each(|offset| {
                    self.mark_stack_offset(*sp as _, offset, ObjectType::Pointer)
                });
            }
        });
    }

    #[cfg(feature = "llvm_stackmap")]
    fn mark_globals(&self) {
        if GLOBAL_MARK_FLAG
            .compare_exchange(false, true, Ordering::SeqCst, Ordering::SeqCst)
            .is_ok()
        {
            unsafe {
                STACK_MAP
                    .global_roots
                    .as_mut()
                    .unwrap()
                    .iter()
                    .for_each(|root| {
                        self.mark_ptr(root.cast_mut());
                    });
            }
        }
    }

    /// # sweep
    ///
    /// since we did synchronization in mark, we don't need to do synchronization again in sweep
    pub fn sweep(&self) -> (usize, usize) {
        GC_SWEEPPING_NUM.fetch_add(1, Ordering::AcqRel);
        GC_SWEEPING.store(true, Ordering::Release);
        log::trace!("gc {}: sweeping...", self.id);
        let used = unsafe {
            self.thread_local_allocator
                .as_mut()
                .unwrap()
                .sweep(self.mark_histogram)
        };
        self.status.borrow_mut().collect_threshold =
            (used.0 as f64 * THRESHOLD_PROPORTION) as usize;
        let v = GC_SWEEPPING_NUM.fetch_sub(1, Ordering::AcqRel);
        if v - 1 == 0 {
            GC_SWEEPING.store(false, Ordering::Release);
        }
        used
    }

    /// # safepoint
    ///
    /// A safepoint is a point in the program where the garbage collector can
    /// safely run.
    ///
    /// Safepoint, if it trigger a GC, it will use backtrace-rs to walk gc frames.
    pub fn safepoint(&self) {
        self.safepoint_fast_unwind(std::ptr::null_mut());
    }

    /// # collect
    ///
    /// Collect garbage.
    ///
    /// This default implementation is walk stack using backtrace-rs,
    /// which is kind of slow but perfectly works on most of platforms. Backtrace-rs is
    /// using a global mutex during stack walking, which may cause
    /// performance issue.
    ///
    /// If you want to gain more performance, you can use its fast version
    /// `collect_fast_unwind`, which requires you to pass the stack pointer,
    /// and perform stack walking using frame size recorded in stackmap, which
    /// is lock-free and more precise(it only walks gc frames).
    pub fn collect(&self) {
        self.collect_fast_unwind(std::ptr::null_mut());
    }

    /// # collect_fast_unwind
    ///
    /// Collect garbage, use stack pointer to walk gc frames.
    ///
    /// ## Parameters
    ///
    /// * `sp` - stack pointer
    ///
    /// if `sp` is null, then use backtrace-rs to walk stack
    ///
    /// for more information, see [mark_fast_unwind](Collector::mark_fast_unwind)
    pub fn collect_fast_unwind(&self, sp: *mut u8) {
        // unsafe {
        //     self.thread_local_allocator.as_ref().unwrap().verify();
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
            if ENABLE_EVA.load(Ordering::SeqCst)
                && self.thread_local_allocator.as_mut().unwrap().should_eva()
            {
                // 如果需要驱逐，首先计算驱逐阀域
                let mut eva_threshold = 0;
                let mut available_histogram: FxHashMap<usize, usize> =
                    FxHashMap::with_capacity_and_hasher(NUM_LINES_PER_BLOCK, Default::default());
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
        self.mark_fast_unwind(sp);
        // let mark_time = time.elapsed();
        let (_used, free) = self.sweep();
        // let sweep_time = time.elapsed() - mark_time;
        unsafe {
            self.thread_local_allocator
                .as_mut()
                .unwrap()
                .set_collect_mode(false);
        }
        log::info!(
            "gc {} collect done, used heap size: {} bytes, freed {} bytes in this gc",
            self.id,
            _used,
            free
        );
        let mut status = self.status.borrow_mut();
        // unsafe {
        //     self.thread_local_allocator.as_ref().unwrap().verify();
        // }
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

    /// # stuck
    ///
    /// tell the collector that the current thread is stucked.
    pub fn stuck(&mut self) {
        self.stuck_fast_unwind(std::ptr::null_mut());
    }

    /// # stuck_fast_unwind
    ///
    /// tell the collector that the current thread is stucked.
    ///
    /// ## Parameters
    ///
    /// - `sp` - stack pointer
    ///
    /// for more information, see [mark_fast_unwind](Collector::mark_fast_unwind)
    pub fn stuck_fast_unwind(&mut self, sp: *mut u8) {
        log::trace!("gc {}: stucking...", self.id);
        let frames = get_frames(sp);
        unsafe {
            let ptr = Box::leak(frames) as *mut _;
            self.frames_list.store(ptr, Ordering::SeqCst);
            let c: *mut Collector = self as *mut _;
            let c = c.as_mut().unwrap();
            c.shadow_thread_running.store(true, Ordering::SeqCst);
            GLOBAL_ALLOCATOR.0.as_ref().unwrap().pool.execute(move || {
                log::info!("gc {}: stucked, waiting for unstuck...", c.id);
                loop {
                    let mut mutex = STUCK_MUTEX.lock();
                    if c.frames_list.load(Ordering::SeqCst).is_null() {
                        log::trace!("gc {}: unstucking break...", c.id);
                        c.shadow_thread_running.store(false, Ordering::SeqCst);
                        drop(mutex);
                        break;
                    } else if GC_RUNNING.load(Ordering::Acquire) {
                        drop(mutex);
                        c.collect();
                    } else {
                        STUCK_COND.wait(&mut mutex);
                        drop(mutex);
                        c.safepoint();
                    }
                }
            });
        }
        STUCK_COND.notify_all();
        // FRAMES_LIST.0.lock().borrow_mut().insert( self as _,frames);
    }

    fn mark_coro_stacks(&self) {
        for (_, frames) in self.coro_stacks.iter() {
            for (ip, sp) in frames.iter() {
                let addr = *ip as *mut u8;
                let const_addr = addr as *const u8;
                let map = unsafe{STACK_MAP.map.as_ref()}.unwrap();
                let f = map.get(&const_addr);
                if let Some(f) = f {
                    // eprintln!("marking coro stack {:p} {:p}, key: {:p}", *ip, *sp, *k);
                    f.iter_roots().for_each(|offset| {
                        self.mark_stack_offset(*sp as _, offset, ObjectType::Pointer)
                    });
                }
            }
        }
    }

    pub fn add_coro_stack(& mut self,sp: * mut u8, stack:* mut u8) {
        let frames = get_frames(sp);
        // eprintln!("add coro stack {:?}", frames);
        self.coro_stacks.insert(stack,frames);
    }

    pub fn remove_coro_stack(& mut self,stack:* mut u8) {
        self.coro_stacks.remove(&stack);
    }

    pub fn unstuck(&mut self) {
        log::trace!("gc {}: unstucking...", self.id);
        let mutex = STUCK_MUTEX.lock();
        let old = self
            .frames_list
            .swap(std::ptr::null_mut(), Ordering::SeqCst);
        if !old.is_null() {
            STUCK_COND.notify_all();
            drop(mutex);
            unsafe {
                drop(Box::from_raw(old));
            }
        } else {
            STUCK_COND.notify_all();
            drop(mutex);
        }
        // wait until the shadow thread exit
        spin_until!(!self.shadow_thread_running.load(Ordering::SeqCst));
    }
}

fn get_frames(sp: *mut u8) -> Box<Vec<(*mut libc::c_void, *mut libc::c_void)>> {
    let mut frames: Box<Vec<(*mut libc::c_void, *mut libc::c_void)>> = Box::default();
    if sp.is_null() {
        backtrace::trace(|frame| {
            frames.push((frame.ip(), frame.sp()));
            true
        });
    } else {
        walk_gc_frames(sp, |sp, _, _| {
            frames.push((get_ip_from_sp(sp) as _, sp as _))
        });
    }
    frames
}

#[cfg(test)]
#[cfg(feature = "shadow_stack")]
mod tests;

static GLOBAL_MARK_FLAG: AtomicBool = AtomicBool::new(false);

// static STUCK_GCED: AtomicBool = AtomicBool::new(false);

lazy_static::lazy_static! {
    static ref STUCK_COND: Arc<Condvar> = Arc::new(Condvar::new());
    static ref STUCK_MUTEX:Mutex<()> = Mutex::new(());
}

unsafe impl Sync for Collector {}
unsafe impl Send for Collector {}
