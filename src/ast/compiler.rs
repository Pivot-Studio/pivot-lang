use std::{cell::RefCell, fs, path::Path, time::Instant};

use colored::Colorize;
use crossbeam_channel::Sender;
use inkwell::{
    context::Context,
    module::Module,
    targets::{FileType, InitializationConfig, Target, TargetMachine},
    OptimizationLevel,
};
use lsp_server::Message;

use crate::{
    lsp::{diagnostics::send_diagnostics, mem_docs::MemDocs},
    nomparser::PLParser,
};
use std::process::Command;

use super::ctx::{self, create_ctx_info};

pub struct Compiler {}

#[derive(Debug, Clone, Default)]
pub struct Options {
    pub verbose: bool,
    pub genir: bool,
    pub printast: bool,
    pub optimization: OptimizationLevel,
}

type MainFunc = unsafe extern "C" fn() -> i64;

pub fn get_target_machine(level: OptimizationLevel) -> TargetMachine {
    let triple = &TargetMachine::get_default_triple();
    let s1 = TargetMachine::get_host_cpu_name();
    let cpu = s1.to_str().unwrap();
    let s2 = TargetMachine::get_host_cpu_features();
    let features = s2.to_str().unwrap();
    Target::initialize_native(&InitializationConfig::default()).unwrap();
    let target = Target::from_triple(triple).unwrap();
    target
        .create_target_machine(
            triple,
            cpu,
            features,
            level,
            inkwell::targets::RelocMode::Static,
            inkwell::targets::CodeModel::Default,
        )
        .unwrap()
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {}
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

    pub fn compile_dry(&self, file: &str, docs: &MemDocs, op: Options, sender: &Sender<Message>) {
        let context = &Context::create();
        let filepath = Path::new(file);
        let abs = fs::canonicalize(filepath).unwrap();
        let dir = abs.parent().unwrap().to_str().unwrap();
        let fname = abs.file_name().unwrap().to_str().unwrap();

        let (a, b, c, d, e, f) = create_ctx_info(context, dir, fname);
        let v = RefCell::new(Vec::new());
        let mut ctx = ctx::Ctx::new(context, &a, &b, &c, &d, &e, &f, abs.to_str().unwrap(), &v);
        let m = &mut ctx;
        let str = docs.get_file_content(file).unwrap();
        let input = str.as_str();
        let mut parser = PLParser::new(input);
        let parse_result = parser.parse();
        if let Err(e) = parse_result {
            eprintln!("{}", e);
            return;
        }
        let mut node = parse_result.unwrap();
        if op.printast {
            node.print(0, true, vec![]);
        }

        _ = node.emit(m);
        let vs = v.borrow().iter().map(|v| v.get_diagnostic()).collect();
        send_diagnostics(&sender, file.to_string(), vs)
    }

    pub fn compile(&self, file: &str, docs: MemDocs, out: &str, op: Options) {
        let now = Instant::now();
        let context = &Context::create();
        let filepath = Path::new(file);
        let abs = fs::canonicalize(filepath).unwrap();
        let dir = abs.parent().unwrap().to_str().unwrap();
        let fname = abs.file_name().unwrap().to_str().unwrap();

        let (a, b, c, d, e, f) = create_ctx_info(context, dir, fname);
        let v = RefCell::new(Vec::new());
        let mut ctx = ctx::Ctx::new(context, &a, &b, &c, &d, &e, &f, abs.to_str().unwrap(), &v);
        let m = &mut ctx;
        let str = docs.get_file_content(file).unwrap();
        let input = str.as_str();
        let mut parser = PLParser::new(input);
        let parse_result = parser.parse();
        if let Err(e) = parse_result {
            println!("{}", e);
            return;
        }
        let mut node = parse_result.unwrap();
        if op.printast {
            node.print(0, true, vec![]);
        }

        _ = node.emit(m);
        let errs = m.errs.borrow();
        if errs.len() > 0 {
            for e in errs.iter() {
                e.print();
            }
            if errs.len() == 1 {
                println!(
                    "{}",
                    format!("compile failed: there is {} error", errs.len()).bright_red()
                );
                return;
            }
            println!(
                "{}",
                format!("compile failed: there are {} errors", errs.len()).bright_red()
            );
            return;
        }
        if op.optimization == OptimizationLevel::None {
            m.dibuilder.finalize();
        }
        if op.genir {
            let mut s = out.to_string();
            s.push_str(".ll");
            let llp = Path::new(&s[..]);
            fs::write(llp, ctx.module.to_string()).unwrap();
        }
        // m.module.verify().unwrap();
        let time = now.elapsed();
        if op.verbose {
            println!("compile succ, time: {:?}", time);
        }
        let tm = get_target_machine(op.optimization);
        let mut f = out.to_string();
        f.push_str(".o");
        let mut fo = out.to_string();
        fo.push_str(".out");
        tm.write_to_file(ctx.module, FileType::Object, Path::new(&f))
            .unwrap();
        let link = Command::new("clang")
            .arg(format!("-O{}", op.optimization as u32))
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
        ctx.module.write_bitcode_to_path(Path::new(out));
    }
}
