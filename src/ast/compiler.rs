use std::path::Path;

use inkwell::{context::Context, module::Module};

use crate::nomparser::PLParser;

use super::ctx;

pub struct Compiler<'a> {
    parser: PLParser<'a>,
}
type MainFunc = unsafe extern "C" fn() -> i64;
impl<'a> Compiler<'a> {
    pub fn new(input: &'a str) -> Self {
        let parser = PLParser::new(input);
        Compiler { parser: parser }
    }

    pub fn run(p: &Path) {
        let ctx = &Context::create();
        let re = Module::parse_bitcode_from_path(p, ctx).unwrap();
        let engine = re
            .create_jit_execution_engine(inkwell::OptimizationLevel::Aggressive)
            .unwrap();
        unsafe {
            let f = engine.get_function::<MainFunc>("main").unwrap();
            println!("a = {}", f.call());
        }
    }

    pub fn compile(&mut self, file: &str) {
        let tp = &Context::create();
        let bd = tp.create_builder();
        let mo = tp.create_module("test");
        let context = ctx::Ctx::new(tp, &bd, &mo);
        let mut mc = ctx::MutCtx::new(context.context, context.module);
        context.builder.position_at_end(mc.basic_block);
        let ctx = &context;
        let m = &mut mc;
        let (_, mut node) = self.parser.parse().unwrap();
        node.emit(ctx, m);
        let v = mc.get_symbol("a");
        let v = v.unwrap();
        let load = context.builder.build_load(*v, "load");
        context.builder.build_return(Some(&load));
        context.module.write_bitcode_to_path(Path::new(file));
    }
}
