use std::{
    cell::RefCell,
    sync::atomic::{AtomicBool, AtomicUsize, Ordering},
};

use collector::Collector;
use lazy_static::lazy_static;
use libc::malloc;

use crate::allocator::GlobalAllocator;

mod allocator;
mod block;
mod collector;
mod consts;
mod mmap;

thread_local! {
    pub static SPACE: RefCell< Collector> = unsafe {
        // gc运行中的时候不能增加线程
        while GC_RUNNING.load(Ordering::Acquire) {
            core::hint::spin_loop();
        }
        let gc = Collector::new(GLOBAL_ALLOCATOR.0.as_mut().unwrap());
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
    while GC_RUNNING.load(Ordering::Acquire) {
        core::hint::spin_loop();
    }
    GC_COLLECTOR_COUNT.fetch_add(1, Ordering::SeqCst);
}

pub struct GAWrapper(*mut GlobalAllocator);

/// collector count
///
/// should be the same as the number of threads
static GC_COLLECTOR_COUNT: AtomicUsize = AtomicUsize::new(0);

static GC_MARK_WAITING: AtomicUsize = AtomicUsize::new(0);

static GC_MARKING: AtomicBool = AtomicBool::new(false);

static GC_SWEEP_WAITING: AtomicUsize = AtomicUsize::new(0);

static GC_SWEEPING: AtomicBool = AtomicBool::new(false);

static GC_RUNNING: AtomicBool = AtomicBool::new(false);

static GC_ID: AtomicUsize = AtomicUsize::new(0);

unsafe impl Sync for GAWrapper {}
