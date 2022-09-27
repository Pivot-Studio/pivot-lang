use std::{fs, path::Path, time::Instant};

use inkwell::{context::Context, module::Module, OptimizationLevel};

use crate::nomparser::PLParser;

use super::ctx;

pub struct Compiler<'a> {
    parser: PLParser<'a>,
}

pub struct Option {
    pub verbose: bool,
    pub genir: bool,
    pub printast: bool,
}

type MainFunc = unsafe extern "C" fn() -> i64;
impl<'a> Compiler<'a> {
    pub fn new(input: &'a str) -> Self {
        let parser = PLParser::new(input);
        Compiler { parser }
    }

    pub fn run(p: &Path, opt: OptimizationLevel) {
        let ctx = &Context::create();
        let re = Module::parse_bitcode_from_path(p, ctx).unwrap();
        let engine = re.create_jit_execution_engine(opt).unwrap();
        unsafe {
            let f = engine.get_function::<MainFunc>("main").unwrap();
            println!("a = {}", f.call());
        }
    }

    pub fn compile(&mut self, file: &str, op: Option) {
        let now = Instant::now();
        let context = &Context::create();
        let builder = &context.create_builder();
        let module = &context.create_module("test");
        let mut ctx = ctx::Ctx::new(context, module, builder);
        let m = &mut ctx;
        let (_, mut node) = self.parser.parse().unwrap();
        if op.printast {
            node.print();
        }
        node.emit(m);
        let v = ctx.get_symbol("a");
        let v = v.unwrap();
        let load = ctx.builder.build_load(*v, "load");
        ctx.builder.build_return(Some(&load));
        if op.genir {
            let mut s = file.to_string();
            s.push_str(".ll");
            let llp = Path::new(&s[..]);
            fs::write(llp, ctx.module.to_string()).unwrap();
        }
        let time = now.elapsed();
        if op.verbose {
            println!("compile succ, time: {:?}", time);
        }
        ctx.module.write_bitcode_to_path(Path::new(file));
    }
}
