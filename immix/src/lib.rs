use std::{
    cell::RefCell,
    sync::atomic::{AtomicBool, AtomicUsize, Ordering},
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
#[cfg(feature = "llvm_stackmap")]
pub use llvm_stackmap::*;

pub use allocator::*;
pub use block::*;
pub use collector::*;
pub use consts::*;

use parking_lot::{lock_api::RawRwLock, RwLock};
#[cfg(feature = "llvm_stackmap")]
use rustc_hash::FxHashMap;

thread_local! {
    pub static SPACE: RefCell<Collector> = unsafe {
        // gc运行中的时候不能增加线程
        let l = GC_RW_LOCK.raw();
        spin_until!(l.try_lock_exclusive());
        let gc = Collector::new(GLOBAL_ALLOCATOR.0.as_mut().unwrap());
        l.unlock_exclusive();
        RefCell::new(gc)
    };
}
#[cfg(feature = "llvm_stackmap")]
lazy_static! {
    static ref STACK_MAP: StackMapWrapper = {
        StackMapWrapper {
            map: RefCell::new(FxHashMap::default()),
        }
    };
}

#[cfg(feature = "llvm_stackmap")]
pub struct StackMapWrapper {
    pub map: RefCell<FxHashMap<*const u8, Function>>,
}
#[cfg(feature = "llvm_stackmap")]
unsafe impl Sync for StackMapWrapper {}
#[cfg(feature = "llvm_stackmap")]
unsafe impl Send for StackMapWrapper {}
const DEFAULT_HEAP_SIZE: usize = 1024 * 1024 * 1024;

lazy_static! {
    pub static ref GLOBAL_ALLOCATOR: GAWrapper = unsafe {
        let mut heap_size = DEFAULT_HEAP_SIZE;
        if let Some(size) = option_env!("PL_IMMIX_HEAP_SIZE") {
            heap_size = size.parse().unwrap();
        }
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
pub fn gc_malloc(size: usize, obj_type: u8) -> *mut u8 {
    SPACE.with(|gc| {
        // println!("start malloc");
        let gc = gc.borrow();
        // println!("malloc");
        gc.alloc(size, ObjectType::from_int(obj_type).unwrap())
    })
}

/// This function is used to force a garbage collection.
pub fn gc_collect() {
    SPACE.with(|gc| {
        // println!("start collect");
        let gc = gc.borrow();
        gc.collect();
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
        gc.borrow().unregister_current_thread();
    })
}

#[cfg(feature = "llvm_stackmap")]
pub fn gc_init(ptr: *mut u8) {
    // println!("stackmap: {:?}", &STACK_MAP.map.borrow());
    build_root_maps(ptr, &mut STACK_MAP.map.borrow_mut());
}

/// notify gc if a thread is going to stuck e.g.
/// lock a mutex or doing sync io
///
/// during thread stucking, if a gc is triggered, it will skip waiting for this thread to
/// reach a safe point
pub fn thread_stuck_start() {
    GC_COLLECTOR_COUNT.fetch_sub(1, Ordering::SeqCst);
}

/// notify gc a thread is not stuck anymore
///
/// if a gc is triggered during thread stucking, this function
/// will block until the gc is finished
pub fn thread_stuck_end() {
    spin_until!(!GC_RUNNING.load(Ordering::Acquire));
    GC_COLLECTOR_COUNT.fetch_add(1, Ordering::SeqCst);
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
static GC_COLLECTOR_COUNT: AtomicUsize = AtomicUsize::new(0);

static GC_MARK_WAITING: AtomicUsize = AtomicUsize::new(0);

static GC_MARKING: AtomicBool = AtomicBool::new(false);

static GC_SWEEPPING_NUM: AtomicUsize = AtomicUsize::new(0);

static GC_SWEEPING: AtomicBool = AtomicBool::new(false);

static GC_RUNNING: AtomicBool = AtomicBool::new(false);

static GC_ID: AtomicUsize = AtomicUsize::new(0);

#[cfg(feature = "auto_gc")]
static GC_AUTOCOLLECT_ENABLE: AtomicBool = AtomicBool::new(true);
#[cfg(not(feature = "auto_gc"))]
static GC_AUTOCOLLECT_ENABLE: AtomicBool = AtomicBool::new(false);

lazy_static! {
    pub static ref GC_RW_LOCK: RwLock<()> = RwLock::new(());
}

unsafe impl Sync for GAWrapper {}
