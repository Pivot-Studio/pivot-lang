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

use std;
use std::os::raw::{c_char, c_void};
// function is marked as unsafe as user can trigger execution of an
// arbitrary memory address using it
pub unsafe fn add_symbol(name: &str, ptr: *const ()) {
    let name = std::ffi::CString::new(name).unwrap();
    let addr = ptr as *mut c_void;
    support::LLVMAddSymbol(name.as_ptr(), addr)
}

// test block below

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::values::FunctionValue;
use inkwell::OptimizationLevel;
use llvm_sys::support;
use llvm_sys::target::LLVM_InitializeNativeTarget;
use std::error::Error;

/// Convenience type alias for the `sum` function.
///
/// Calling this is innately `unsafe` because there's no guarantee it doesn't
/// do `unsafe` operations internally.
type demoFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    fn jit_compile_sum(&self) -> Option<JitFunction<demoFunc>> {
        let i64_type = self.context.i64_type();
        let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);
        let function = self.module.add_function("sum", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        let x = function.get_nth_param(0)?.into_int_value();
        let y = function.get_nth_param(1)?.into_int_value();
        let z = function.get_nth_param(2)?.into_int_value();

        let demo = self.module.add_function("demo", fn_type, None);
        let sum = self
            .builder
            .build_call(demo, &[x.into(), y.into(), z.into()], "xxx");

        self.builder
            .build_return(Some(&sum.try_as_basic_value().left().unwrap()));
        unsafe {
            let demo = self.execution_engine.get_function("sum");
            if let Ok(demo1) = demo {
                return Some(demo1);
            } else {
                println!("{}", demo.err().unwrap());
                return None;
            }
        }
    }
}

fn demo(a: u64, b: u64, c: u64) -> u64 {
    println!("Hello, world!");
    a + b + c
}

#[test]
fn test_add_symbol() -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("sum");
    unsafe {
        add_symbol("demo", demo as *const ());
    }
    unsafe {
        LLVM_InitializeNativeTarget();
    }
    let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;
    let codegen = CodeGen {
        context: &context,
        module,
        builder: context.create_builder(),
        execution_engine,
    };

    let demo = codegen
        .jit_compile_sum()
        .ok_or("Unable to JIT compile `sum`")?;

    let x = 1u64;
    let y = 2u64;
    let z = 3u64;

    unsafe {
        println!("{} + {} + {} = {}", x, y, z, demo.call(x, y, z));
        assert_eq!(demo.call(x, y, z), x + y + z);
    }
    return Ok(());
}
