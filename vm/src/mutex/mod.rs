use std::{
    cell::Cell,
    mem,
    sync::{Condvar, Mutex, MutexGuard},
};

use internal_macro::is_runtime;

use crate::logger::SimpleLogger;

struct MutexContainer {
    mutex: Mutex<()>,
    guard: Cell<Option<MutexGuard<'static, ()>>>,
}
pub struct OpaqueMutex {
    _data: [usize; 0],
}

#[is_runtime]
fn create_mutex(mutex: *mut *mut OpaqueMutex) -> u64 {
    SimpleLogger::init_from_env("GC_LOG");
    // immix::pin(mutex.cast());
    // immix::gc_keep_live_pinned(mutex.cast());
    *mutex = Box::into_raw(Box::new(MutexContainer {
        mutex: Mutex::new(()),
        guard: Cell::new(None),
    }))
    .cast();
    // eprintln!("mutex: {:p} -> {:p}", mutex,*mutex);
    fn drop_mutex_f(mutex: *mut u8) {
        unsafe {
            drop(Box::from_raw(mutex.cast::<MutexContainer>()));
        }
    }
    immix::gc_register_finalizer(mutex as _, (*mutex) as _, drop_mutex_f);
    0
}

#[is_runtime]
fn lock_mutex(mutex: *mut OpaqueMutex) -> u64 {
    // eprintln!("lock mutex: {:p}", mutex);
    let container: &MutexContainer = &*mutex.cast();
    // immix::thread_stuck_start();
    let lock: MutexGuard<'static, _> = mem::transmute(container.mutex.lock().unwrap());
    // immix::thread_stuck_end();
    container.guard.set(Some(lock));
    0
}

#[is_runtime]
fn unlock_mutex(mutex: *mut OpaqueMutex) -> u64 {
    let container: &MutexContainer = &*mutex.cast();
    if container.mutex.try_lock().is_ok() {
        return !0; //can't unlock an unlocked mutex
    } else {
        container.guard.set(None);
    }
    0
}

#[is_runtime]
fn create_condvar(cv: *mut *mut Condvar) -> u64 {
    let condvar = Box::leak(Box::new(Condvar::new()));
    *cv = condvar;
    0
}

#[is_runtime]
fn drop_condvar(cond: *mut Condvar) -> u64 {
    drop(Box::from_raw(cond));
    0
}

#[is_runtime]
fn condvar_wait(cond: *mut Condvar, mutex: *mut OpaqueMutex) -> u64 {
    // eprintln!("condvar wait mutex: {:p}", mutex);
    let container: &MutexContainer = &*mutex.cast();
    let lock = container.guard.replace(None).unwrap();
    let cond = unsafe { &*cond };
    let lock = cond.wait::<()>(lock).unwrap();
    container.guard.set(Some(lock));
    0
}

#[is_runtime]
fn condvar_notify(cond: *mut Condvar) -> u64 {
    // eprintln!("condvar notify");
    let cond = unsafe { &*cond };
    cond.notify_one();
    0
}

#[is_runtime]
fn condvar_notify_all(cond: *mut Condvar) -> u64 {
    let cond = unsafe { &*cond };
    cond.notify_all();
    0
}
