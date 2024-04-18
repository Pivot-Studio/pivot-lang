use std::ffi::CString;

use inkwell::support;

use inkwell::OptimizationLevel;

use std::path::Path;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum EngineType {
    // MCJit,
    #[default]
    OrcJit,
}

impl From<&str> for EngineType {
    fn from(s: &str) -> Self {
        match s {
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
    support::enable_llvm_pretty_stack_trace();
    // expected to be called by memory manager on stackmap section initialized
    extern "C" fn gc_init(map: *mut u8) {
        immix::gc_init(map);
    }
    match engine {
        EngineType::OrcJit => {
            let cstr = CString::new(p.to_str().unwrap()).unwrap();
            let cstr_p = cstr.as_ptr();
            unsafe { immix::CreateAndRunPLOrcJITEngine(cstr_p, opt as u32, gc_init) }
        }
    }
}
