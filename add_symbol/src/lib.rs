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
use llvm_sys::support;
use std;
use std::os::raw::c_void;
// function is marked as unsafe as user can trigger execution of an
// arbitrary memory address using it
pub unsafe fn add_symbol(name: &str, ptr: *const ()) {
    let name = std::ffi::CString::new(name).unwrap();
    let addr = ptr as *mut c_void;
    support::LLVMAddSymbol(name.as_ptr(), addr)
}
/// The `#[is_runtime]` attribute.  
///
/// used to tag a function as a runtime function  
/// or tag an impl block to indicate that all the pub fn in impl block are runtime functions  
///
/// those functions will be added to the llvm symbol table  
///
/// while tagging a function, you can specify the name of the function in the llvm symbol table like this:  
///
/// ```
/// #[add_symbol::is_runtime("myfunc")]
/// pub fn myfunc1() {
///    // ...
/// }
/// ```
/// if the name is not specified, the name of the function will be used as the name in the llvm symbol table.  
///
/// while tagging an impl block, the name of the function in the llvm symbol table will be like {block_type_name}__{fn_name}.   
///
/// you can override the name of the block_type_name just like the function sample above.
///
/// ```
/// struct MyStruct;
/// #[add_symbol::is_runtime("struct")]
/// impl MyStruct {
///    pub fn myfunc1() {
///       // ...
///    }
/// }
/// ```
/// the function myfunc1 will be added to the llvm symbol table with the name struct__myfunc1
pub use add_symbol_macro::is_runtime;
#[doc(hidden)]
pub extern crate ctor;
