use std::{fs, path::Path, time::Instant};

use inkwell::{
    context::Context,
    module::Module,
    targets::{FileType, InitializationConfig, Target, TargetMachine},
    OptimizationLevel,
};

use crate::nomparser::PLParser;
use std::process::Command;

use super::ctx;

pub struct Compiler<'a> {
    parser: PLParser<'a>,
}

pub struct Option {
    pub verbose: bool,
    pub genir: bool,
    pub printast: bool,
    pub optimization: OptimizationLevel,
}

type MainFunc = unsafe extern "C" fn() -> i64;
impl<'a> Compiler<'a> {
    pub fn new(input: &'a str) -> Self {
        let parser = PLParser::new(input);
        Compiler { parser }
    }
    #[cfg(feature = "jit")]
    pub fn run(p: &Path, opt: OptimizationLevel) {
        vm::gc::reg();
        let ctx = &Context::create();
        let re = Module::parse_bitcode_from_path(p, ctx).unwrap();
        let engine = re.create_jit_execution_engine(opt).unwrap();
        unsafe {
            let f = engine.get_function::<MainFunc>("main").unwrap();
            println!("ret = {}", f.call());
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
            println!("{}", node.string(0));
        }
        node.emit(m);
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
        let triple = &TargetMachine::get_default_triple();
        let s1 = TargetMachine::get_host_cpu_name();
        let cpu = s1.to_str().unwrap();
        let s2 = TargetMachine::get_host_cpu_features();
        let features = s2.to_str().unwrap();
        Target::initialize_native(&InitializationConfig::default()).unwrap();
        let target = Target::from_triple(triple).unwrap();
        let tm = target
            .create_target_machine(
                triple,
                cpu,
                features,
                op.optimization,
                inkwell::targets::RelocMode::Static,
                inkwell::targets::CodeModel::Default,
            )
            .unwrap();
        let mut f = file.to_string();
        f.push_str(".o");
        let mut fo = file.to_string();
        fo.push_str(".out");
        tm.write_to_file(ctx.module, FileType::Object, Path::new(&f))
            .unwrap();
        let link = Command::new("clang")
            .arg("-pthread")
            .arg("-ldl")
            .arg(&f)
            .arg("vm/target/release/libvm.a")
            .arg("-o")
            .arg(&fo)
            .status();
        if link.is_err() || !link.unwrap().success() {
            println!(
                "warning: link with pivot lang vm failed, could be caused by vm pkg not found."
            );
            println!("warning: you can build vm pkg by `cargo build --release` in vm dir.");
        } else {
            println!("link succ, output file: {}", fo);
        }
        //  TargetMachine::get_default_triple()
        ctx.module.write_bitcode_to_path(Path::new(file));
    }
}
