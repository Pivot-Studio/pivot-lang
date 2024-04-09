use inkwell::targets::InitializationConfig;
use inkwell::{memory_buffer::MemoryBuffer, module::Module};

use inkwell::context::Context;

use log;
use std::ffi::{c_char, CString};

use inkwell::support;

use inkwell::OptimizationLevel;

use std::path::Path;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum EngineType {
    MCJit,
    #[default]
    OrcJit,
}

impl From<&str> for EngineType {
    fn from(s: &str) -> Self {
        match s {
            "mc" => EngineType::MCJit,
            "orc" => EngineType::OrcJit,
            _ => panic!("unknown engine type"),
        }
    }
}

/// # run
///
/// jit run a module
///
/// ## Arguments
///
/// * `p` - module path
/// * `opt` - optimization level
pub fn run(p: &Path, opt: OptimizationLevel, engine: EngineType) -> i32 {
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
    match engine {
        EngineType::MCJit => {
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
            // some opt may change it's name
            re.get_functions()
                .find(|f| {
                    f.get_name()
                        .to_str()
                        .unwrap()
                        .starts_with("__gc_init_stackmap")
                })
                .map(|v| unsafe { v.delete() });
            inkwell::targets::Target::initialize_native(&InitializationConfig::default()).unwrap();
            unsafe {
                let ret = immix::CreateAndRunPLJITEngine(re.as_mut_ptr() as _, opt as u32, gc_init);
                std::mem::forget(re);
                ret
            }
        }
        EngineType::OrcJit => {
            let cstr = CString::new(p.to_str().unwrap()).unwrap();
            let cstr_p = cstr.as_ptr();
            unsafe { immix::CreateAndRunPLOrcJITEngine(cstr_p, opt as u32, gc_init) }
        }
    }
}
