#![allow(improper_ctypes_definitions)]
#![allow(clippy::missing_safety_doc)]

use std::{process::exit, sync::mpsc::channel, thread};

use backtrace::Backtrace;
use internal_macro::is_runtime;
pub mod gc;
pub mod libcwrap;
pub mod logger;
pub mod mutex;

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
fn new_thread(f: i128) {
    // f's first 8 byte is fn pointer, next 8 byte is data pointer
    let ptr = &f as *const _ as *const i64;
    let f_ptr = ptr as *const extern "C" fn(i64);
    let data_ptr = unsafe { *ptr.offset(1) };
    let func = unsafe { *f_ptr };
    let (s, r) = channel::<()>();
    let c = move || {
        thread::sleep(std::time::Duration::from_secs(1));
        immix::gc_keep_live(data_ptr as _);
        // immix::gc_add_root(&mut f as *mut _ as *mut _, ObjectType::Trait.int_value());
        s.send(()).unwrap();
        func(data_ptr);
        // immix::gc_remove_root(&mut f as *mut _ as *mut _);
        immix::gc_rm_live(data_ptr as _);
        immix::no_gc_thread();
    };
    thread::spawn(c);
    r.recv().unwrap();
}

#[is_runtime]
fn sleep(secs: u64) {
    gc::DioGC__stuck_begin();
    println!("sleeping for {} secs", secs);
    thread::sleep(std::time::Duration::from_secs(secs));
    gc::DioGC__stuck_end();
    println!("sleeping done");
}

#[is_runtime]
fn print_u64(u: u64) {
    println!("u64( {} )", u);
}
#[is_runtime]
fn print_i128(i: i128) {
    print!("{}", i);
}

#[is_runtime]
fn print_u128(i: u128) {
    print!("{}", i);
}

#[is_runtime]
fn print_hex(i: i64) {
    print!("0x{:X}", i);
}

#[is_runtime]
fn utf8_count(ptr: *mut u8, byte_len: i64) -> i64 {
    let s = unsafe { std::slice::from_raw_parts(ptr, byte_len as usize) };
    bytecount::num_chars(s) as _
}

pub fn count_utf8_char(s: &str) -> usize {
    bytecount::num_chars(s.as_bytes())
}
