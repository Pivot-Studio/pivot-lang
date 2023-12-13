use inkwell::execution_engine::ExecutionEngine;
use inkwell::targets::InitializationConfig;
use inkwell::{memory_buffer::MemoryBuffer, module::Module};

use inkwell::context::Context;

use llvm_sys::execution_engine::LLVMExecutionEngineRef;
use log;
use std::ffi::{c_char, CString};
use std::rc::Rc;

use inkwell::support;

use inkwell::OptimizationLevel;

use std::path::Path;

/// # run
///
/// jit run a module
///
/// ## Arguments
///
/// * `p` - module path
/// * `opt` - optimization level
pub fn run(p: &Path, opt: OptimizationLevel) -> i32 {
    vm::logger::SimpleLogger::init_from_env_default("PL_LOG", log::LevelFilter::Error);
    vm::reg();

    support::enable_llvm_pretty_stack_trace();
    let ctx = &Context::create();
    extern "C" {
        fn parse_ir(path: *const c_char) -> llvm_sys::prelude::LLVMMemoryBufferRef;
    }
    // expected to be called by memory manager on stackmap section initialized
    extern "C" fn gc_init(map: *mut u8) {
        immix::gc_init(map);
    }
    let re = if p.to_str().unwrap().ends_with(".ll") {
        let c_str = CString::new(p.to_str().unwrap()).unwrap();
        let m = unsafe { parse_ir(c_str.as_ptr()) };
        let mb = unsafe { MemoryBuffer::new(m) };
        Module::parse_bitcode_from_buffer(&mb, ctx).unwrap()
    } else {
        Module::parse_bitcode_from_path(p, ctx).unwrap()
    };
    // remove all static link related global variables and gc init function
    // because in jit gc init is done by memory manager
    re.get_global("llvm.global_ctors")
        .map(|v| unsafe { v.delete() });
    re.get_global("llvm.used").map(|v| unsafe { v.delete() });
    re.get_global("__LLVM_StackMaps")
        .map(|v| unsafe { v.delete() });
    re.get_global("_LLVM_StackMaps")
        .map(|v| unsafe { v.delete() });
    re.get_function("__gc_init_stackmap")
        .map(|v| unsafe { v.delete() });
    inkwell::targets::Target::initialize_native(&InitializationConfig::default()).unwrap();
    unsafe {
        let jit = immix::CreatePLJITEngine(re.as_mut_ptr() as _, opt as u32, gc_init);
        let engine = ExecutionEngine::new(Rc::new(*(jit as *mut LLVMExecutionEngineRef)), true);
        let r = re
            .get_function("main")
            .map(|f| engine.run_function_as_main(f, &[]))
            .expect("cannot find main");
        std::mem::forget(re);
        r
    }
}
