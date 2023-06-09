#![cfg(feature = "llvm")]
use std::sync::atomic::AtomicBool;
pub static IS_JIT: AtomicBool = AtomicBool::new(false);
