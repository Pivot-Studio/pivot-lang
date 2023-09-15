#![allow(improper_ctypes_definitions)]
#![allow(clippy::missing_safety_doc)]

use std::process::exit;

use backtrace::Backtrace;
use internal_macro::is_runtime;
pub mod gc;
pub mod libcwrap;
pub mod logger;

#[is_runtime]
fn test_vm_link() -> i64 {
    66
}

#[is_runtime]
fn printi64ln(i: i64) {
    println!("{}", i);
}

#[cfg(feature = "jit")]
pub fn reg() {
    gc::reg();
}

#[is_runtime]
fn pl_panic() {
    println!("pivot lang panic occured!");
    let bt = Backtrace::new();
    println!("{:?}", bt);
    exit(1);
}

#[is_runtime]
fn __cast_panic() {
    println!("invalid cast occured!");
    let bt = Backtrace::new();
    println!("{:?}", bt);
    exit(1);
}

#[is_runtime]
fn ptr_to_int(ptr: *const u8) -> i64 {
    ptr as i64
}

#[is_runtime]
fn int_to_ptr(i: i64) -> *const u8 {
    i as *const u8
}

#[is_runtime]
fn print_raw(bs: *const u8, len: i64) {
    let s = std::str::from_utf8(unsafe { std::slice::from_raw_parts(bs, len as usize) }).unwrap();
    print!("{}", s);
}

#[is_runtime]
fn print_i64(i: i64) {
    print!("{}", i);
}

#[is_runtime]
fn utf8_count(ptr: *mut u8, byte_len: i64) -> i64 {
    let s = unsafe { std::slice::from_raw_parts(ptr, byte_len as usize) };
    bytecount::num_chars(s) as _
}

pub fn count_utf8_char(s: &str) -> usize {
    bytecount::num_chars(s.as_bytes())
}
