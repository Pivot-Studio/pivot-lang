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
    use llvm_sys::support;
    use std::os::raw::c_void;
    let name = std::ffi::CString::new(name).unwrap();
    let addr = ptr as *mut c_void;
    support::LLVMAddSymbol(name.as_ptr(), addr)
}

pub use add_symbol_macro::is_runtime;
pub use range_macro::range;
pub use test_parser_macro::test_parser;
pub use test_parser_macro::test_parser_error;
#[doc(hidden)]
pub extern crate ctor;
