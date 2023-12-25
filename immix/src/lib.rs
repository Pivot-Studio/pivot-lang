#![allow(clippy::not_unsafe_ptr_arg_deref)]
#![allow(clippy::missing_safety_doc)]
use std::{
    cell::RefCell,
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc,
    },
};

pub use int_enum::IntEnum;
use lazy_static::lazy_static;
use libc::malloc;

mod allocator;
mod block;
mod collector;
mod consts;
#[cfg(feature = "llvm_stackmap")]
mod llvm_stackmap;
mod macros;
mod mmap;
mod shadow_stack;
#[cfg(feature = "llvm_stackmap")]
pub use llvm_stackmap::*;
mod bigobj;

pub use allocator::*;
pub use block::*;
pub use collector::*;
pub use consts::*;

use parking_lot::{Condvar, Mutex};
#[cfg(feature = "llvm_stackmap")]
use rustc_hash::FxHashMap;

thread_local! {
    pub static SPACE: RefCell<Collector> = unsafe {
        // gc运行中的时候不能增加线程
        let gc = Collector::new(GLOBAL_ALLOCATOR.0.as_mut().unwrap());
        RefCell::new(gc)
    };
}
#[cfg(feature = "llvm_stackmap")]
lazy_static! {
    static ref STACK_MAP: StackMapWrapper = {
        let map = Box::into_raw(Box::default());
        let global_roots = Box::into_raw(Box::default());
        StackMapWrapper { map, global_roots }
    };
}

pub fn register_global(p: *mut u8) {
    unsafe {
        STACK_MAP.global_roots.as_mut().unwrap().push(p);
    }
}

#[cfg(feature = "llvm_stackmap")]
pub struct StackMapWrapper {
    pub map: *mut FxHashMap<*const u8, Function>,
    pub global_roots: *mut Vec<*mut u8>,
}
#[cfg(feature = "llvm_stackmap")]
unsafe impl Sync for StackMapWrapper {}
#[cfg(feature = "llvm_stackmap")]
unsafe impl Send for StackMapWrapper {}
const DEFAULT_HEAP_SIZE: usize = 1024 * 1024 * 1024 * 16;

lazy_static! {
    pub static ref GLOBAL_ALLOCATOR: GAWrapper = unsafe {
        let mut heap_size = DEFAULT_HEAP_SIZE;
        if let Ok(usage) = sys_info::mem_info() {
            heap_size = usage.total as usize * 1024;
        } else {
            log::warn!(
                "Failed to get virtual memory size, use default heap size {} byte",
                heap_size
            );
        }
        if let Some(size) = option_env!("PL_IMMIX_HEAP_SIZE") {
            heap_size = size.parse().unwrap();
        }
        heap_size = round_n_up!(heap_size, BLOCK_SIZE);
        log::info!("heap size: {}", heap_size);
        let ga = GlobalAllocator::new(heap_size);
        let mem = malloc(core::mem::size_of::<GlobalAllocator>()).cast::<GlobalAllocator>();
        mem.write(ga);
        GAWrapper(mem)
    };
}

/// This function is used to allocate a new object on the heap.
/// The size of the object is given in bytes by the size argument.
/// The object type is specified by the obj_type argument, which is a u8.
/// This function is used to allocate all objects (except
/// for the object header) on the heap,
/// and it is used by both the GC and the user.
/// This function is used by the user in the following manner:
/// ```ignore
/// let obj = gc_malloc(size, obj_type);
/// ```
/// where obj is a pointer to the newly allocated object.
///
/// ## Behaviour
///
/// If auto gc is enabled, this function may trigger a gc if some conditions are met.
///
/// If the heap is full, this function will trigger an emergency gc and try again.
/// If the heap is still full after the emergency gc, this function will return null.
pub fn gc_malloc(size: usize, obj_type: u8) -> *mut u8 {
    SPACE.with(|gc| {
        // println!("start malloc");
        let gc = gc.borrow();
        // println!("malloc");
        gc.alloc(size, ObjectType::from_int(obj_type).unwrap())
    })
}

/// # gc_malloc_fast_unwind
///
/// Same behavior as gc_malloc, but this function will use stack
/// pointer to perform fast unwind
///
/// If the stack pointer is not provided, this function will use
/// backtrace-rs to walk the stack.
///
/// For more information, see [mark_fast_unwind](crate::collector::Collector::mark_fast_unwind)
pub fn gc_malloc_fast_unwind(size: usize, obj_type: u8, sp: *mut u8) -> *mut u8 {
    SPACE.with(|gc| {
        // println!("start malloc");
        let gc = gc.borrow();
        // println!("malloc");
        gc.alloc_fast_unwind(size, ObjectType::from_int(obj_type).unwrap(), sp)
    })
}

/// # gc_malloc_no_collect
///
/// Same behavior as gc_malloc, but this function will never trigger
/// a gc.
pub fn gc_malloc_no_collect(size: usize, obj_type: u8) -> *mut u8 {
    SPACE.with(|gc| {
        // println!("start malloc");
        let gc = gc.borrow();
        // println!("malloc");
        gc.alloc_no_collect(size, ObjectType::from_int(obj_type).unwrap())
    })
}

/// This function is used to force a garbage collection.
///
/// During the collection mark phase, this function will
/// use backtrace-rs to walk the stack.
///
/// For more information, see [mark_fast_unwind](crate::collector::Collector::mark_fast_unwind)
pub fn gc_collect() {
    SPACE.with(|gc| {
        // println!("start collect");
        let gc = gc.borrow();
        gc.collect();
        // println!("collect")
    })
}

/// # gc_collect_fast_unwind
///
/// Same behavior as gc_collect, but this function will use stack
/// pointer to perform fast unwind.
///
/// For more information, see [mark_fast_unwind](crate::collector::Collector::mark_fast_unwind)
pub fn gc_collect_fast_unwind(sp: *mut u8) {
    SPACE.with(|gc| {
        // println!("start collect");
        let gc = gc.borrow();
        gc.collect_fast_unwind(sp);
        // println!("collect")
    })
}

#[cfg(feature = "shadow_stack")]
pub fn gc_add_root(root: *mut u8, obj_type: u8) {
    SPACE.with(|gc| {
        // println!("start add_root");
        let mut gc = gc.borrow_mut();
        gc.add_root(root, ObjectType::from_int(obj_type).unwrap());
        // println!("add_root")
    })
}

pub fn gc_keep_live(gc_ptr: *mut u8) -> u64 {
    SPACE.with(|gc| {
        // println!("start add_root");
        let gc = gc.borrow();
        gc.keep_live(gc_ptr)
        // println!("add_root")
    })
}

pub fn gc_rm_live(handle: u64) {
    SPACE.with(|gc| {
        // println!("start add_root");
        let gc = gc.borrow();
        gc.rm_live(handle);
        // println!("add_root")
    })
}

#[cfg(feature = "shadow_stack")]
pub fn gc_remove_root(root: *mut u8) {
    SPACE.with(|gc| {
        // println!("start remove_root");
        let mut gc = gc.borrow_mut();
        gc.remove_root(root);
        // println!("remove_root")
    })
}

pub fn gc_disable_auto_collect() {
    GC_AUTOCOLLECT_ENABLE.store(false, Ordering::SeqCst);
}

pub fn gc_enable_auto_collect() {
    GC_AUTOCOLLECT_ENABLE.store(true, Ordering::SeqCst);
}

pub fn gc_is_auto_collect_enabled() -> bool {
    GC_AUTOCOLLECT_ENABLE.load(Ordering::SeqCst)
}

pub fn no_gc_thread() {
    SPACE.with(|gc| {
        gc.borrow_mut().unregister_current_thread();
    })
}

pub fn safepoint() {
    SPACE.with(|gc| {
        gc.borrow().safepoint();
    })
}

pub fn safepoint_fast_unwind(sp: *mut u8) {
    SPACE.with(|gc| {
        gc.borrow().safepoint_fast_unwind(sp);
    })
}

#[cfg(feature = "llvm_stackmap")]
pub fn gc_init(ptr: *mut u8) {
    // print_stack_map(ptr);
    // println!("stackmap: {:?}", &STACK_MAP.map.borrow());
    build_root_maps(ptr, unsafe { STACK_MAP.map.as_mut().unwrap() }, unsafe {
        STACK_MAP.global_roots.as_mut().unwrap()
    });
    log::info!("read stack map done");
}

/// notify gc current thread is going to stuck e.g.
/// lock a mutex or doing sync io or sleep etc.
///
/// during thread stucking, gc will start a nanny thread to
/// do gc works that original thread should do.
///
/// ## Note
///
/// During thread stucking, the stucking thread should not
/// request any memory from gc, or it will cause a panic.
pub fn thread_stuck_start() {
    // v.0 -= 1;
    SPACE.with(|gc| {
        // println!("start add_root");
        let mut gc = gc.borrow_mut();
        gc.stuck()
        // println!("add_root")
    });
}

pub fn thread_stuck_start_fast(sp: *mut u8) {
    // v.0 -= 1;
    SPACE.with(|gc| {
        // println!("start add_root");
        let mut gc = gc.borrow_mut();
        gc.stuck_fast_unwind(sp)
        // println!("add_root")
    });
}

/// notify gc current thread is not stuck anymore
///
/// if a gc is triggered during thread stucking, this function
/// will block until the gc is finished
pub fn thread_stuck_end() {
    log::trace!("unstucking...");
    spin_until!(!GC_RUNNING.load(Ordering::SeqCst));
    // v.0 += 1;
    SPACE.with(|gc| {
        // println!("start add_root");
        let mut gc = gc.borrow_mut();
        gc.unstuck()
        // println!("add_root")
    });
}

pub fn add_coro_stack(sp: *mut u8, stack: *mut u8) {
    SPACE.with(|gc| {
        // println!("start add_root");
        let mut gc = gc.borrow_mut();
        gc.add_coro_stack(sp, stack)
        // println!("add_root")
    });
}

pub fn remove_coro_stack(stack: *mut u8) {
    SPACE.with(|gc| {
        // println!("start add_root");
        let mut gc = gc.borrow_mut();
        gc.remove_coro_stack(stack)
        // println!("add_root")
    });
}

/// # set evacuation
///
/// 设置是否开启自动驱逐
///
/// 驱逐(evacuation)是immix一种去碎片化机制
///
/// 一般来说驱逐能带来更好的性能，但是请注意，驱逐会
/// 改变mutator指针的指向，正常来说这种改动是自愈的，
/// 在gc过程结束后mutator应当不会受到影响，但是如果
/// mutator中**特殊**存储了指向堆内存的指针，且该数据不被gc
/// 观测，那么驱逐可能会导致它指向错误的地址。
pub fn set_evacuation(eva: bool) {
    ENABLE_EVA.store(eva, Ordering::SeqCst);
}

pub struct GAWrapper(*mut GlobalAllocator);

impl GAWrapper {
    pub fn unmap_all(&self) {
        unsafe {
            self.0.as_mut().unwrap().unmap_all();
        }
    }
}

/// collector count
///
/// should be the same as the number of threads
static GC_COLLECTOR_COUNT: Mutex<(usize, usize)> = Mutex::new((0, 0));

// static GC_MARK_WAITING: AtomicUsize = AtomicUsize::new(0);

static GC_MARKING: AtomicBool = AtomicBool::new(false);

// static GC_MARK_COND: Arc< Condvar> = Arc::new( Condvar::new());

lazy_static! {
    static ref GC_MARK_COND: Arc<Condvar> = Arc::new(Condvar::new());
}

static GC_SWEEPPING_NUM: AtomicUsize = AtomicUsize::new(0);

static GC_SWEEPING: AtomicBool = AtomicBool::new(false);

static GC_RUNNING: AtomicBool = AtomicBool::new(false);

static GC_ID: AtomicUsize = AtomicUsize::new(0);

static GC_STW_COUNT: AtomicUsize = AtomicUsize::new(0);

pub(crate) static USE_SHADOW_STACK: AtomicBool = AtomicBool::new(false);

pub static ENABLE_EVA: AtomicBool = AtomicBool::new(true);

#[cfg(feature = "auto_gc")]
static GC_AUTOCOLLECT_ENABLE: AtomicBool = AtomicBool::new(true);
#[cfg(not(feature = "auto_gc"))]
static GC_AUTOCOLLECT_ENABLE: AtomicBool = AtomicBool::new(false);

unsafe impl Sync for GAWrapper {}

pub fn get_gc_stw_num() -> usize {
    GC_STW_COUNT.load(Ordering::SeqCst)
}

pub fn set_shadow_stack(b: bool) {
    USE_SHADOW_STACK.store(b, Ordering::Relaxed);
}

#[cfg(feature = "llvm_gc_plugin")]
pub unsafe fn set_shadow_stack_addr(addr: *mut u8) {
    shadow_stack::SetShadowStackAddr(addr)
}
