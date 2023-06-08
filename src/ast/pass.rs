#![cfg(feature = "llvm")]
use std::{cell::RefCell, sync::atomic::AtomicBool};

use immix::LLVM_GC_STRATEGY_NAME;

use inkwell::module::Module;
use parking_lot::Mutex;

lazy_static::lazy_static! {
    pub static ref MAP_NAMES: GlobalMutWrapper<Vec<String>> = {
        GlobalMutWrapper { inner: Mutex::new( RefCell::new(vec![])) }
    };
}

pub struct GlobalMutWrapper<T> {
    pub inner: Mutex<RefCell<T>>,
}

pub static IS_JIT: AtomicBool = AtomicBool::new(false);

pub fn run_immix_pass(module: &Module) {
    if IS_JIT.load(std::sync::atomic::Ordering::Relaxed) {
        // jit is using shadow stack, skip immix pass
        module.get_functions().for_each(|f| {
            f.set_gc("shadow-stack");
        });
        return;
    }
    // transform the IR
    module.get_functions().for_each(|f| {
        f.set_gc(LLVM_GC_STRATEGY_NAME);
    });
}
