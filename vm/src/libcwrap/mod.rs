#![allow(clippy::useless_conversion)]
use std::ffi::CString;

use internal_macro::is_runtime;

struct LibC {}
#[cfg(feature = "jit")]
pub fn reg() {
    add_symbol_consts();
    add_symbol_impl_libc();
}

#[cfg(not(target_os = "windows"))]
#[no_mangle]
pub static STDIN_FILENO: libc::c_int = libc::STDIN_FILENO;

#[cfg(target_os = "windows")]
#[no_mangle]
pub static STDIN_FILENO: libc::c_int = 0;

#[cfg(not(target_os = "windows"))]
#[no_mangle]
pub static STDOUT_FILENO: libc::c_int = libc::STDOUT_FILENO;

#[cfg(target_os = "windows")]
#[no_mangle]
pub static STDOUT_FILENO: libc::c_int = 1;

#[cfg(not(target_os = "windows"))]
#[no_mangle]
pub static STDERR_FILENO: libc::c_int = libc::STDERR_FILENO;

#[cfg(target_os = "windows")]
#[no_mangle]
pub static STDERR_FILENO: libc::c_int = 2;

#[no_mangle]
pub static O_RDONLY: libc::c_int = libc::O_RDONLY;
#[no_mangle]
pub static O_WRONLY: libc::c_int = libc::O_WRONLY;
#[no_mangle]
pub static O_RDWR: libc::c_int = libc::O_RDWR;
#[no_mangle]
pub static O_CREAT: libc::c_int = libc::O_CREAT;

internal_macro::add_symbol_consts!(
    STDIN_FILENO,
    STDOUT_FILENO,
    STDERR_FILENO,
    O_RDONLY,
    O_WRONLY,
    O_RDWR,
    O_CREAT,
);

#[is_runtime]
impl LibC {
    fn read(fd: libc::c_int, buf: *mut libc::c_void, count: libc::size_t) -> libc::ssize_t {
        unsafe {
            libc::read(fd, buf, count.try_into().unwrap())
                .try_into()
                .unwrap()
        }
    }
    fn write(fd: libc::c_int, buf: *const libc::c_void, count: libc::size_t) -> libc::ssize_t {
        unsafe {
            libc::write(fd, buf, count.try_into().unwrap())
                .try_into()
                .unwrap()
        }
    }
    fn open(path: *const u8, byte_len: i64, flags: libc::c_int) -> libc::c_int {
        // create cstr from ptr and len
        let path = unsafe { std::slice::from_raw_parts(path, byte_len as usize) };
        let cstr = CString::new(path).unwrap();
        let path = cstr.as_ptr();
        unsafe { libc::open(path, flags) }
    }
    fn close(fd: libc::c_int) -> libc::c_int {
        unsafe { libc::close(fd) }
    }

    fn memcpy(
        dest: *mut libc::c_void,
        src: *const libc::c_void,
        n: libc::size_t,
    ) -> *mut libc::c_void {
        unsafe { libc::memcpy(dest, src, n) }
    }

    fn getrandom(
        buf: *mut libc::c_void,
        buflen: libc::size_t,
        flags: libc::c_uint,
    ) -> libc::ssize_t {
        unsafe { libc::syscall(libc::SYS_getrandom, buf, buflen, flags) as libc::ssize_t }
    }
}
