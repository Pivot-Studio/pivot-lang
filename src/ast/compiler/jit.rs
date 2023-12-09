use inkwell::targets::InitializationConfig;
use inkwell::{memory_buffer::MemoryBuffer, module::Module};

use inkwell::context::Context;

use log;
use std::ffi::{c_char, CString};

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
///
/// ## Limitation
///
/// * windows doesn't support jit with shadow stack yet
/// * currently stackmap support on jit code is not possible, gc is using shadow stack
/// * shadow stack is not thread safe, may cause crash in multi-threaded environment
pub fn run(p: &Path, opt: OptimizationLevel) {
    vm::logger::SimpleLogger::init_from_env_default("PL_LOG", log::LevelFilter::Error);
    vm::reg();

    support::enable_llvm_pretty_stack_trace();
    let ctx = &Context::create();
    extern "C" {
        fn parse_ir(path: *const c_char) -> llvm_sys::prelude::LLVMMemoryBufferRef;
    }
    extern "C" fn gc_init(map: *mut u8) {
        eprintln!("gc init {:p}", map);
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
        immix::CreateAndRunPLJITEngine(re.as_mut_ptr() as _, opt as u32, gc_init);
    };
}
