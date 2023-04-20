#![cfg(feature = "llvm")]
use std::{cell::RefCell, sync::atomic::AtomicBool};

use immix::LLVM_GC_STRATEGY_NAME;

use inkwell::{
    module::{Linkage, Module},
    AddressSpace,
};
use parking_lot::Mutex;

lazy_static::lazy_static! {
    pub static ref MAP_NAMES: GlobalMutWrapper<Vec<String>> = {
        GlobalMutWrapper { inner: Mutex::new( RefCell::new(vec![])) }
    };
}

pub struct GlobalMutWrapper<T> {
    pub inner: Mutex<RefCell<T>>,
}

pub static IS_JIT: AtomicBool = AtomicBool::new(false);

pub fn run_immix_pass(module: &Module) {
    if IS_JIT.load(std::sync::atomic::Ordering::Relaxed) {
        // jit is using shadow stack, skip immix pass
        module.get_functions().for_each(|f| {
            f.set_gc("shadow-stack");
        });
        return;
    }
    let ctx = module.get_context();
    let builder = ctx.create_builder();
    // add global _GC_MAP_
    let gcmap_name = "_GC_MAP_".to_string() + module.get_source_file_name().to_str().unwrap();
    let stack_map = module.add_global(ctx.i8_type(), Default::default(), &gcmap_name);
    // println!("gcmap_name: {}", gcmap_name);
    MAP_NAMES
        .inner
        .lock()
        .borrow_mut()
        .push(gcmap_name.clone() + "__init");
    stack_map.set_linkage(Linkage::ExternalWeak);
    let ptr_tp = ctx.i8_type().ptr_type(AddressSpace::default());
    let init_f_tp = ctx.void_type().fn_type(&[ptr_tp.into()], false);
    let init_f = module.add_function("immix_gc_init", init_f_tp, None);
    // transform the IR
    module.get_functions().for_each(|f| {
        f.set_gc(LLVM_GC_STRATEGY_NAME);
    });
    let mod_init_f = module.add_function(
        &(gcmap_name + "__init"),
        ctx.void_type().fn_type(&[], false),
        None,
    );
    let bb = ctx.append_basic_block(mod_init_f, "entry");
    builder.position_at_end(bb);
    builder.build_call(init_f, &[stack_map.as_pointer_value().into()], "gc_init");
    builder.build_return(None);

    module.get_function("main").and_then(|fmain| {
        fmain
            .get_first_basic_block()
            .and_then(|bb| bb.get_first_instruction())
            .map(|inst| {
                builder.position_before(&inst);
                MAP_NAMES.inner.lock().borrow().iter().for_each(|name| {
                    let f = module.get_function(name).unwrap_or_else(|| {
                        module.add_function(name, ctx.void_type().fn_type(&[], false), None)
                    });
                    builder.build_call(f, &[], "gc_init");
                });
            })
    });
}
