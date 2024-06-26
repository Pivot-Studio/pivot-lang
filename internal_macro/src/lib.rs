#![allow(clippy::missing_safety_doc)]
// Copy and modified from https://github.com/jauhien/iron-llvm
// Copyright 2015 Jauhien Piatlicki.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// LLVM Support
// LLVM-C header Support.h

// function is marked as unsafe as user can trigger execution of an
// arbitrary memory address using it
#[cfg(feature = "jit")]
pub unsafe fn add_symbol(name: &str, ptr: *const ()) {
    use std::os::raw::c_void;
    log::debug!("add symbol {} at {:p}", name, ptr);
    let addr = ptr as *mut c_void;
    SYMBOLS
        .lock()
        .unwrap()
        .insert(name.to_owned(), addr as isize);
    // llvm_sys::support::LLVMAddSymbol(name.as_ptr(), addr)
}

#[cfg(feature = "jit")]
pub fn register_all_symbol_for_mc() {
    unsafe {
        for (name, ptr) in SYMBOLS.lock().unwrap().iter() {
            let name = std::ffi::CString::new(name.as_str()).unwrap();
            llvm_sys::support::LLVMAddSymbol(name.as_ptr(), *ptr as _);
        }
    }
}

lazy_static::lazy_static! {
    static ref SYMBOLS: std::sync::Mutex<std::collections::HashMap<String, isize>> = std::sync::Mutex::new(std::collections::HashMap::new());

}

#[macro_export]
macro_rules! add_runtime_consts {
    () => {

    };
    ($name:ident) => {
        let ptr = &$name as * const _ as *const ();
        let name = stringify!($name);
        unsafe {
            internal_macro::add_symbol(name, ptr);
        }
    };
    ($name:ident, $($names:ident),*) => {
        internal_macro::add_runtime_consts!($name);
        internal_macro::add_runtime_consts!($($names),*);
    };
    ($($names:ident),* ,) => {
        internal_macro::add_runtime_consts!($($names),*);
    };
}

#[macro_export]
macro_rules! add_symbol_consts {
    () => {

    };
    ($($names:ident),* ) => {
        #[cfg(feature = "jit")]
        #[internal_macro::ctor::ctor]
        fn add_symbol_consts() {
            internal_macro::add_runtime_consts!(
                $($names),*
            );
        }
    };
    ($($names:ident),* ,) => {
        internal_macro::add_symbol_consts!($($names),*);
    };

}

pub use add_symbol_macro::is_runtime;
pub use comment_macro::comments;
pub use fmt_macro::fmt;
pub use node_macro::node;
pub use range_macro::range;
pub use test_parser_macro::test_parser;
pub use test_parser_macro::test_parser_error;
