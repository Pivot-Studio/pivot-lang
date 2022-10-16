use crate::{
    ast::node::{program::Program, Node},
    lsp::mem_docs::MemDocsInput,
    nomparser::parse,
    Db,
};
use std::{cell::RefCell, fs, path::Path, time::Instant};

use colored::Colorize;
use inkwell::{
    context::Context,
    module::Module,
    targets::{FileType, InitializationConfig, Target, TargetMachine},
    OptimizationLevel,
};
use std::process::Command;

use super::ctx::{self, create_ctx_info};

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, Copy)]
pub struct Options {
    pub verbose: bool,
    pub genir: bool,
    pub printast: bool,
    pub optimization: HashOptimizationLevel,
}

#[repr(u32)]
#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum HashOptimizationLevel {
    None = 0,
    Less = 1,
    Default = 2,
    Aggressive = 3,
}

impl Default for HashOptimizationLevel {
    /// Returns the default value for `OptimizationLevel`, namely `OptimizationLevel::Default`.
    fn default() -> Self {
        HashOptimizationLevel::Default
    }
}

impl HashOptimizationLevel {
    pub fn to_llvm(&self) -> OptimizationLevel {
        match self {
            HashOptimizationLevel::None => OptimizationLevel::None,
            HashOptimizationLevel::Less => OptimizationLevel::Less,
            HashOptimizationLevel::Default => OptimizationLevel::Default,
            HashOptimizationLevel::Aggressive => OptimizationLevel::Aggressive,
        }
    }
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

/// # ActionType
/// lsp action type
#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub enum ActionType {
    Completion,
    GotoDef,
    FindReferences,
    SemanticTokensFull,
    Diagnostic,
    Hover,
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

#[salsa::tracked(lru = 32)]
pub fn compile_dry(db: &dyn Db, docs: MemDocsInput) {
    let src = docs.get_file_content(db).unwrap();
    let parse_result = parse(db, src);
    if let Err(e) = parse_result {
        eprintln!("{}", e);
        return;
    }
    let node = parse_result.unwrap();
    let program = Program::new(db, node, docs.get_emit_params(db));
    program.emit(db);
}

#[salsa::tracked]
pub fn compile(db: &dyn Db, docs: MemDocsInput, out: String, op: Options) {
    let now = Instant::now();
    let context = &Context::create();
    let filepath = Path::new(docs.file(db));
    let abs = dunce::canonicalize(filepath).unwrap();
    let dir = abs.parent().unwrap().to_str().unwrap();
    let fname = abs.file_name().unwrap().to_str().unwrap();

    let (a, b, c, d, e, f) = create_ctx_info(context, dir, fname);
    let v = RefCell::new(Vec::new());
    let mut ctx = ctx::Ctx::new(
        context,
        &a,
        &b,
        &c,
        &d,
        &e,
        &f,
        abs.to_str().unwrap(),
        &v,
        None,
        None,
    );
    let m = &mut ctx;
    m.module.set_triple(&TargetMachine::get_default_triple());
    let src = docs.get_file_content(db).unwrap();
    let parse_result = parse(db, src);
    if let Err(e) = parse_result {
        println!("{}", e);
        return;
    }
    let node = parse_result.unwrap();
    if op.printast {
        node.node(db).print(0, true, vec![]);
    }
    let mut nn = node.node(db);
    _ = nn.emit(m);
    let errs = m.errs.borrow();
    let mut errs_num = 0;
    if errs.len() > 0 {
        for e in errs.iter() {
            e.print();
            if e.is_err() {
                errs_num = errs_num + 1
            }
        }
        if errs_num > 0 {
            if errs_num == 1 {
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
    }
    if op.optimization.to_llvm() == OptimizationLevel::None {
        m.dibuilder.finalize();
    }
    if op.genir {
        let mut s = out.to_string();
        s.push_str(".ll");
        let llp = Path::new(&s[..]);
        fs::write(llp, m.module.to_string()).unwrap();
    }
    m.module.verify().unwrap();
    let time = now.elapsed();
    if op.verbose {
        println!("compile succ, time: {:?}", time);
    }
    let tm = get_target_machine(op.optimization.to_llvm());
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
        println!("warning: link with pivot lang vm failed, could be caused by vm pkg not found.");
        println!("warning: you can build vm pkg by `cargo build --release` in vm dir.");
    } else {
        println!("link succ, output file: {}", fo);
    }
    //  TargetMachine::get_default_triple()
    ctx.module.write_bitcode_to_path(Path::new(&out));
}
