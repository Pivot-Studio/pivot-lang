use std::cell::RefCell;

use lazy_static::lazy_static;
use llvm_plugin::inkwell::AddressSpace;
use llvm_plugin::inkwell::module::{Module, Linkage};
use llvm_plugin::{
     ModuleAnalysisManager, PreservedAnalyses, PassBuilder, PipelineParsing,
};
pub use llvm_plugin::LlvmModulePass;

use crate::LLVM_GC_STRATEGY_NAME;

// A name and version is required.
#[llvm_plugin::plugin(name = "plimmixgc", version = "0.1")]
fn plugin_register(builder: &mut PassBuilder) {
    // Add a callback to parse a name from the textual representation of
    // the pipeline to be run.
    builder.add_module_pipeline_parsing_callback(|name, manager| {
        if name == "plimmixgc" {
            // the input pipeline contains the name "custom-pass",
            // so we add our custom pass to the pass manager
            manager.add_pass(PLImmixPass);

            // we notify the caller that we were able to parse
            // the given name
            PipelineParsing::Parsed
        } else {
            // in any other cases, we notify the caller that our
            // callback wasn't able to parse the given name
            PipelineParsing::NotParsed
        }
    });
}

pub fn run_immix_pass(module:*const u8) {
    unsafe{run_pass((module as *const Module).as_ref().unwrap());}
}


pub struct PLImmixPass;
impl LlvmModulePass for PLImmixPass {
    fn run_pass(
        &self,
        module: &mut Module,
        _manager: &ModuleAnalysisManager
    ) -> PreservedAnalyses {
        run_pass(module)
        
    }
}

lazy_static!{
    static ref MAP_NAMES: NameListWrapper = {
        NameListWrapper { names: RefCell::new(vec![]) }
    };
}

pub struct NameListWrapper {
    pub names: RefCell<Vec<String>>,
}

unsafe impl Send for NameListWrapper {}
unsafe impl Sync for NameListWrapper {}


fn run_pass(
    module: & Module,
) -> PreservedAnalyses {
    let ctx = module.get_context();
    let builder = ctx.create_builder();
    // add global _GC_MAP_
    let gcmap_name = "_GC_MAP_".to_string() + module.get_source_file_name().to_str().unwrap();
    let stack_map = module.add_global(ctx.i8_type(), Default::default(), &gcmap_name);
    println!("gcmap_name: {}", gcmap_name);
    MAP_NAMES.names.borrow_mut().push(gcmap_name.clone()+"__init");
    stack_map.set_linkage(Linkage::ExternalWeak);
    // module.get_function("main").unwrap().set_gc(LLVM_GC_STRATEGY_NAME);
    let ptr_tp = ctx.i8_type().ptr_type(AddressSpace::default());
    let init_f_tp = ctx.void_type().fn_type(&[ptr_tp.into()], false);
    let init_f = module.add_function("immix_gc_init", init_f_tp, None);
    // transform the IR
    module.get_functions().for_each(|f| {
        f.set_gc(LLVM_GC_STRATEGY_NAME);
    });
    let mod_init_f = module.add_function(&(gcmap_name+"__init"), ctx.void_type().fn_type(&[], false), None);
    let bb = ctx.append_basic_block(mod_init_f, "entry");
    builder.position_at_end(bb);
    builder.build_call(init_f, &[stack_map.as_pointer_value().into()], "gc_init");
    builder.build_return(None);

    module.get_function("main").and_then(|fmain|{
        fmain.get_first_basic_block().and_then(|bb|{
            bb.get_first_instruction()
        }).and_then(|inst|{
            builder.position_before(&inst);
            MAP_NAMES.names.borrow().iter().for_each(|name|{
                let f = module.get_function(name).unwrap_or_else(
                    || module.add_function(name, ctx.void_type().fn_type(&[], false), None)
                );
                builder.build_call(f, &[], "gc_init");
            });
            Some(())
        })
    });
    PreservedAnalyses::All
    
}