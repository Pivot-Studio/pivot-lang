#![allow(improper_ctypes_definitions)]

use internal_macro::is_runtime;
pub mod gc;

#[is_runtime]
fn test_vm_link() -> i64 {
    return 66;
}