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
mod macros;
mod mmap;
mod bigobj;

pub use allocator::*;
pub use block::*;
pub use collector::*;
pub use consts::*;
use parking_lot::{lock_api::RawRwLock, RwLock};

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

lazy_static! {
    pub static ref GLOBAL_ALLOCATOR: GAWrapper = unsafe {
        let ga = GlobalAllocator::new(1024 * 1024 * 1024);
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
        let mut gc = gc.borrow_mut();
        gc.collect();
        // println!("collect")
    })
}

pub fn gc_add_root(root: *mut u8, obj_type: u8) {
    SPACE.with(|gc| {
        // println!("start add_root");
        let mut gc = gc.borrow_mut();
        gc.add_root(root, ObjectType::from_int(obj_type).unwrap());
        // println!("add_root")
    })
}

pub fn gc_remove_root(root: *mut u8) {
    SPACE.with(|gc| {
        // println!("start remove_root");
        let mut gc = gc.borrow_mut();
        gc.remove_root(root);
        // println!("remove_root")
    })
}

pub fn no_gc_thread() {
    drop(SPACE)
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
    spin_until!(!GC_RUNNING.load(Ordering::SeqCst));
    GC_COLLECTOR_COUNT.fetch_add(1, Ordering::SeqCst);
}

pub struct GAWrapper(*mut GlobalAllocator);

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

lazy_static! {
    pub static ref GC_RW_LOCK: RwLock<()> = RwLock::new(());
}

unsafe impl Sync for GAWrapper {}
