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
        let context = &Context::create();
        let builder = &context.create_builder();
        let module = &context.create_module("test");
        let mut ctx = ctx::Ctx::new(context, module, builder);
        ctx.builder.position_at_end(ctx.basic_block);
        let m = &mut ctx;
        let (_, mut node) = self.parser.parse().unwrap();
        node.emit(m);
        let v = ctx.get_symbol("a");
        let v = v.unwrap();
        let load = ctx.builder.build_load(*v, "load");
        ctx.builder.build_return(Some(&load));
        ctx.module.write_bitcode_to_path(Path::new(file));
    }
}
