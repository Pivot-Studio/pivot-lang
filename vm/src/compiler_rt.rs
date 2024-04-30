//! walkaround for windows, see https://discourse.llvm.org/t/orc-jit-on-windows-cannot-find-divti3/78676/2
use internal_macro::is_runtime;

#[is_runtime]
fn __divti3(a: i128, b: i128) -> i128 {
    a / b
}

#[is_runtime]
fn __modti3(a: i128, b: i128) -> i128 {
    a % b
}
