use std::{
    cell::Cell,
    mem,
    sync::{Mutex, MutexGuard},
};

use internal_macro::is_runtime;

struct MutexContainer {
    mutex: Mutex<()>,
    guard: Cell<Option<MutexGuard<'static, ()>>>,
}
pub struct OpaqueMutex {
    _data: [usize; 0],
}

#[is_runtime]
fn create_mutex(mutex: *mut *mut OpaqueMutex) -> u64 {
    *mutex = Box::into_raw(Box::new(MutexContainer {
        mutex: Mutex::new(()),
        guard: Cell::new(None),
    }))
    .cast();
    0
}

#[is_runtime]
fn lock_mutex(mutex: *mut OpaqueMutex) -> u64 {
    let container: &MutexContainer = &*mutex.cast();
    immix::thread_stuck_start();
    let lock: MutexGuard<'static, _> = mem::transmute(container.mutex.lock().unwrap());
    immix::thread_stuck_end();
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
fn drop_mutex(mutex: *mut OpaqueMutex) -> u64 {
    unlock_mutex(mutex);
    drop(Box::from_raw(mutex.cast::<MutexContainer>()));
    0
}
