use inkwell::module::Module;

use inkwell::context::Context;

use log;
use std::ffi::CString;

use immix::set_shadow_stack_addr;
use inkwell::support;
use llvm_sys::execution_engine::LLVMGetGlobalValueAddress;

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
pub fn run(p: &Path, opt: OptimizationLevel) -> i64 {
    // windows doesn't support jit with shadow stack yet
    if cfg!(windows) {
        eprintln!("jit is not yet supported on windows");
        return 0;
    }
    type MainFunc = unsafe extern "C" fn() -> i64;
    vm::logger::SimpleLogger::init_from_env_default("PL_LOG", log::LevelFilter::Error);
    vm::reg();
    // FIXME: currently stackmap support on jit code is not possible due to
    // lack of support in inkwell https://github.com/TheDan64/inkwell/issues/296
    // so we disable gc in jit mode for now
    // immix::gc_disable_auto_collect();
    // extern "C" fn gc_init(ptr: *mut u8) {
    //     println!("gc init {:p}", ptr);
    //     immix::gc_init(ptr);
    // }
    immix::set_shadow_stack(true);

    support::enable_llvm_pretty_stack_trace();
    let ctx = &Context::create();
    let re = Module::parse_bitcode_from_path(p, ctx).unwrap();
    // let chain = re.get_global("llvm_gc_root_chain").unwrap();
    // // chain.set_thread_local(true);
    // // chain.set_thread_local_mode(Some(inkwell::ThreadLocalMode::InitialExecTLSModel));
    // // add a new function to the module, which return address of llvm_gc_root_chain

    // re.as_mut_ptr()
    // inkwell::targets::Target::initialize_native(&InitializationConfig::default()).unwrap();
    // let mut engine: MaybeUninit<*mut LLVMOpaqueExecutionEngine> = MaybeUninit::uninit();
    // let engine = unsafe {
    //     immix::CreatePLJITEngine(
    //         engine.as_mut_ptr() as *mut _ as _,
    //         re.as_mut_ptr() as _,
    //         opt as u32,
    //         gc_init,
    //     );
    //     let engine = engine.assume_init();
    //     ExecutionEngine::new(Rc::new(engine), true)
    // };
    let engine = re.create_jit_execution_engine(opt).unwrap();

    unsafe {
        engine.run_static_constructors();

        let c_str = CString::new("llvm_gc_root_chain").unwrap();
        let addr = LLVMGetGlobalValueAddress(engine.as_mut_ptr(), c_str.as_ptr());

        set_shadow_stack_addr(addr as *mut u8);
        let f = engine.get_function::<MainFunc>("main").unwrap();
        f.call()
    }
}
