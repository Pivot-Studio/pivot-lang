#![allow(improper_ctypes_definitions)]
#![allow(clippy::missing_safety_doc)]

use std::{cell::RefCell, ops::Deref, process::exit, sync::mpsc::channel, thread};

use backtrace::Backtrace;
use context::{
    stack::{ProtectedFixedSizeStack, Stack},
    Context, Transfer,
};
use internal_macro::is_runtime;
pub mod gc;
pub mod libcwrap;
pub mod logger;
pub mod mutex;
pub mod time;

#[is_runtime]
fn test_vm_link() -> i64 {
    // libuv_sys2::uv_close(handle, cb, timeout, repeat)
    // let a = libuv_sys2::uv_async_cb;
    // let re = libuv_sys2::uv_handle_size(libuv_sys2::uv_handle_type_UV_IDLE);
    66
}

#[is_runtime]
fn printi64ln(i: i64) {
    println!("{}", i);
}

#[is_runtime]
fn eprinti64ln(i: i64) {
    eprintln!("{}", i);
}

#[is_runtime]
fn pl_panic() {
    println!("pivot lang panic occured!");
    let bt = Backtrace::new();
    println!("{:?}", bt);
    exit(1);
}

#[is_runtime]
fn pl_index_out_of_bounds(index: i64, len: i64) {
    println!(
        "index out of bounds occured! index: {}, len: {}",
        index, len
    );
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
    let re = std::str::from_utf8(unsafe { std::slice::from_raw_parts(bs, len as usize) });
    let s = re.unwrap();
    print!("{}", s);
}

#[is_runtime]
fn print_i64(i: i64) {
    print!("{}", i);
}

#[is_runtime]
fn vm_dtoa(f: f64, rec: *mut u8) {
    let s = unsafe { std::slice::from_raw_parts_mut(rec, 70) };
    f.to_string()
        .as_bytes()
        .iter()
        .take(70)
        .enumerate()
        .for_each(|(i, b)| {
            s[i] = *b;
        });
}

#[is_runtime]
fn vm_ftoa(f: f32, rec: *mut u8) {
    let s = unsafe { std::slice::from_raw_parts_mut(rec, 64) };
    f.to_string()
        .as_bytes()
        .iter()
        .enumerate()
        .for_each(|(i, b)| {
            s[i] = *b;
        });
}

#[is_runtime]
fn new_thread(f: *mut i128, sp: *mut u8) {
    // f's first 8 byte is fn pointer, next 8 byte is data pointer
    let ptr = f as *const i64;
    let f_ptr = ptr as *const extern "C" fn(i64);
    let data_ptr = unsafe { *ptr.offset(1) };
    let func = unsafe { *f_ptr };
    let (s, r) = channel::<()>();
    // pin data to prevent evacuation
    immix::pin(data_ptr as _);
    let c = move || {
        // initialize gc to prevent evacuation before thread local collector init
        immix::register_current_thread();
        s.send(()).unwrap();
        func(data_ptr);
        immix::no_gc_thread();
    };
    thread::spawn(c);
    immix::thread_stuck_start_fast(sp);
    r.recv().unwrap();
    immix::thread_stuck_end();
    // immix::gc_rm_live(data_ptr as _);
}

#[is_runtime]
fn sleep(secs: u64) {
    // gc::DioGC__stuck_begin(sp);
    thread::sleep(std::time::Duration::from_secs(secs));
    // gc::DioGC__stuck_end();
}

#[is_runtime]
fn print_u64(u: u64) {
    println!("u64( {} )", u);
}

#[is_runtime]
fn print_f64(f: f64) {
    println!("{}", f);
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

extern "C" fn context_function(t: Transfer) -> ! {
    let f = t.data as *mut i128;
    let ptr = f as *const i64;
    let f_ptr = ptr as *const extern "C" fn(i64);
    let data_ptr = unsafe { *ptr.offset(1) };
    let func = unsafe { *f_ptr };
    // let context = unsafe{std::mem::transmute::<Context,usize>(t.context)};
    unsafe {
        set_current_coro(t.context, std::ptr::null_mut());
    }
    func(data_ptr);
    todo!()
}

#[is_runtime]
fn new_coro_ctx(stack: *mut ProtectedFixedSizeStack) -> Context {
    // // Allocate a Context on the stack.
    // let offseted = stack.offset(7680);
    // eprintln!("{:p}->{:p}", offseted,stack);
    Context::new(stack.as_ref().unwrap(), context_function)
}

#[is_runtime]
fn new_coro_stack() -> *mut ProtectedFixedSizeStack {
    Box::leak(Box::new(ProtectedFixedSizeStack::new(8 * 1024).unwrap()))
        as *mut ProtectedFixedSizeStack
}

#[is_runtime]
fn coro_stack_from_heap_ptr(stack: *mut u8) -> *mut Stack {
    Box::leak(Box::new(Stack::new(stack.offset(7000) as _, stack as _))) as *mut _
}

#[is_runtime]
fn dispose_coro_stack(stack: *mut ProtectedFixedSizeStack) {
    drop(unsafe { Box::from_raw(stack) });
}

#[is_runtime]
fn set_current_coro(ctx: Context, stack: *mut u8) {
    TRANS.with(|tr| {
        tr.borrow_mut()
            .replace((unsafe { std::mem::transmute::<Context, usize>(ctx) }, stack));
    });
}

#[is_runtime]
fn set_current_ctx(ctx: Context) {
    TRANS.with(|tr| {
        let stack = tr
            .borrow()
            .deref()
            .map(|(_, s)| s)
            .unwrap_or(std::ptr::null_mut());
        tr.borrow_mut()
            .replace((unsafe { std::mem::transmute::<Context, usize>(ctx) }, stack));
    });
}

#[is_runtime]
fn set_current_stack(stack: *mut u8) {
    // eprintln!("set_current_stack {:p}", stack);
    TRANS.with(|tr| {
        let ctx = tr.borrow().deref().unwrap().0;
        tr.borrow_mut().replace((ctx, stack));
    });
}

#[is_runtime]
fn coro_run(ctx: Context, f: *mut i128, sp: *mut u8) -> *mut CoroRunRet {
    immix::add_coro_stack(sp, std::ptr::null_mut());
    let tr = ctx.resume(f as usize);
    let ctx = tr.context;
    immix::remove_coro_stack(std::ptr::null_mut());
    Box::leak(Box::new(CoroRunRet {
        ctx: std::mem::transmute::<Context, i64>(ctx),
        data: tr.data as _,
    }))
}

#[is_runtime]
fn get_run_ret_ctx(r: *mut CoroRunRet) -> i64 {
    r.as_mut().unwrap().ctx
}

#[is_runtime]
fn get_run_ret_data(r: *mut CoroRunRet) -> i64 {
    r.as_mut().unwrap().data
}

#[is_runtime]
fn dispose_run_ret(r: *mut CoroRunRet) {
    drop(unsafe { Box::from_raw(r) });
}

#[repr(C)]
#[derive(Debug)]
pub struct CoroRunRet {
    pub ctx: i64,
    pub data: i64,
}

#[is_runtime]
fn coro_yield(data: usize, sp: *mut u8) {
    let stack = TRANS.with(|tr| {
        let stack_p;
        let binding = tr.borrow();
        let bind = binding.deref();
        if let Some((t, stack)) = *bind {
            // eprintln!("coro yield {:p}->{:p}", stack, sp);
            stack_p = stack;
            immix::add_coro_stack(sp, stack);
            let c = std::mem::transmute::<usize, Context>(t)
                .resume(data)
                .context;
            unsafe {
                set_current_ctx(c);
            }
        } else {
            unreachable!()
        }
        stack_p
    });
    set_current_stack(stack);
}

#[is_runtime]
fn get_current_coro(ctx: *mut usize, stack_p: *mut *mut u8) {
    TRANS.with(|tr| {
        let binding = tr.borrow();
        let bind = binding.deref();
        if let Some((t, stack)) = *bind {
            // eprintln!("coro yield {:p}->{:p}", stack, sp);
            unsafe {
                *ctx = t;
            }
            unsafe {
                *stack_p = stack;
            }
        } else {
            unreachable!()
        }
    });
}

thread_local! {
    pub static TRANS: RefCell<Option<(usize, * mut u8)>> =  {
        Default::default()
    };
}

#[is_runtime]
fn exit_now(code: i64) {
    std::process::exit(code as _);
}

#[is_runtime]
fn itoa(i: i64, rec: *mut u8) -> i64 {
    let s = unsafe { std::slice::from_raw_parts_mut(rec, 10) };
    i.to_string()
        .as_bytes()
        .iter()
        .enumerate()
        .for_each(|(i, b)| {
            s[i] = *b;
        });
    i.to_string().len() as _
}

#[is_runtime]
fn sqrt_f64(f: f64) -> f64 {
    f.sqrt()
}

#[is_runtime]
fn keep_on_stack(_i: i64) {}

#[is_runtime]
fn millitime() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_millis() as _
}

#[repr(C)]
pub struct UVBuf {
    pub f1: usize,
    pub f2: usize, // Only for placeholder
}

#[no_mangle]
pub extern "C" fn my_uv_buf_init(
    buf: *mut UVBuf,
    base: *mut i8,
    len: u32,
    uv_buf_init: extern "C" fn(base: *mut i8, len: u32) -> UVBuf,
) {
    unsafe {
        *buf = uv_buf_init(base, len);
    }
}

#[cfg(all(windows, feature = "jitdylib"))]
mod compiler_rt;

// (.data._rust_extern_with_linkage___dso_handle+0x0): undefined reference to `__dso_handle'
//  I want to define it manually
#[cfg(target_os = "linux")]
extern "C" {
    pub static __dso_handle: *const u8;
}
