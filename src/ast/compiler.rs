use super::{dot, node::program::ModWrapper};
use crate::{
    ast::{
        accumulators::{Diagnostics, ModBuffer},
        builder::llvmbuilder::get_target_machine,
        node::program::Program,
    },
    lsp::mem_docs::{FileCompileInput, MemDocsInput},
    nomparser::parse,
    utils::read_config::get_config_path,
    Db,
};
use colored::Colorize;
use inkwell::{
    context::Context,
    module::Module,
    passes::{PassManager, PassManagerBuilder},
    targets::FileType,
    OptimizationLevel,
};
use log::{info, trace, warn};
use rustc_hash::FxHashSet;
use std::process::Command;
use std::{
    env,
    fs::{self, remove_file},
    path::Path,
    time::Instant,
};

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, Copy)]
pub struct Options {
    pub genir: bool,
    pub printast: bool,
    pub optimization: HashOptimizationLevel,
    pub fmt: bool,
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
    Compile,
    PrintAst,
    Fmt,
    LspFmt,
    Hint,
    DocSymbol,
    SignatureHelp,
}

#[cfg(feature = "jit")]
pub fn run(p: &Path, opt: OptimizationLevel) {
    use inkwell::support;

    vm::reg();
    support::enable_llvm_pretty_stack_trace();
    let ctx = &Context::create();
    let re = Module::parse_bitcode_from_path(p, ctx).unwrap();
    let engine = re.create_jit_execution_engine(opt).unwrap();
    unsafe {
        let f = engine.get_function::<MainFunc>("main").unwrap();
        println!("ret = {}", f.call());
    }
}

#[salsa::tracked]
pub fn compile_dry(db: &dyn Db, docs: MemDocsInput) {
    let path = get_config_path(docs.file(db).to_string());
    if path.is_err() {
        log::error!("lsp error: {}", path.err().unwrap());
        return;
    }

    let input = docs.get_file_params(db, docs.file(db).clone(), true);
    if input.is_none() {
        return;
    }
    compile_dry_file(db, input.unwrap());
}

#[salsa::tracked]
pub fn compile_dry_file(db: &dyn Db, docs: FileCompileInput) -> Option<ModWrapper> {
    if docs.file(db).ends_with(".toml") {
        // skip toml
        return None;
    }
    // eprintln!("compile_dry_file: {:?}", docs.debug_all(db));
    let re = docs.get_file_content(db);
    if re.is_none() {
        return None;
    }
    let src = re.unwrap();
    let parse_result = parse(db, src);
    if let Err(e) = parse_result {
        log::error!("source code parse failed {}", e);
        return None;
    }
    let node = parse_result.unwrap();
    let program = Program::new(
        db,
        node,
        docs.get_emit_params(db),
        docs.docs(db),
        docs.config(db),
    );
    Some(program.emit(db))
}

#[salsa::tracked]
pub fn compile(db: &dyn Db, docs: MemDocsInput, out: String, op: Options) {
    let now = Instant::now();
    compile_dry(db, docs);
    let errs = compile_dry::accumulated::<Diagnostics>(db, docs);
    let mut errs_num = 0;
    if errs.len() > 0 {
        for e in errs.iter() {
            let path = &e.0;
            for e in e.1.iter() {
                e.print(&path, docs.get_file_content(db).unwrap().text(db));
                if e.is_err() {
                    errs_num = errs_num + 1
                }
            }
        }
        if errs_num > 0 {
            if errs_num == 1 {
                log::error!(
                    "{}",
                    format!("compile failed: there is {} error", errs_num).bright_red()
                );
                println!("{}", format!("{}", dot::ERROR));
                return;
            }
            log::error!(
                "{}",
                format!("compile failed: there are {} errors", errs_num).bright_red()
            );
            println!("{}", format!("{}", dot::TOOMANYERROR));
            return;
        }
    }
    if op.printast {
        let time = now.elapsed();
        info!("print ast done, time: {:?}", time);
        return;
    }
    if op.fmt {
        let time = now.elapsed();
        info!("gen source done, time: {:?}", time);
        return;
    }
    let mut mods = compile_dry::accumulated::<ModBuffer>(db, docs);
    let ctx = Context::create();
    let m = mods.pop().unwrap();
    let llvmmod = Module::parse_bitcode_from_path(m.clone(), &ctx).unwrap();
    let mut set = FxHashSet::default();
    set.insert(m.clone());
    _ = remove_file(m.clone()).unwrap();
    log::debug!("rm {}", m.to_str().unwrap());
    for m in mods {
        if set.contains(&m) {
            continue;
        }
        set.insert(m.clone());
        // println!("{}", m.clone().to_str().unwrap());
        _ = llvmmod.link_in_module(Module::parse_bitcode_from_path(m.clone(), &ctx).unwrap());
        _ = remove_file(m.clone()).unwrap();
        log::debug!("rm {}", m.to_str().unwrap());
    }
    llvmmod.verify().unwrap();
    let tm = get_target_machine(op.optimization.to_llvm());
    let mut f = out.to_string();
    f.push_str(".asm");

    if op.optimization != HashOptimizationLevel::None {
        let pass_manager_builder = PassManagerBuilder::create();

        pass_manager_builder.set_optimization_level(op.optimization.to_llvm());
        pass_manager_builder.set_size_level(2);

        // Create FPM MPM
        let fpm = PassManager::create(&llvmmod);
        let mpm = PassManager::create(());

        pass_manager_builder.populate_function_pass_manager(&fpm);
        pass_manager_builder.populate_module_pass_manager(&mpm);
        let b = fpm.initialize();
        trace!("fpm init: {}", b);
        for f in llvmmod.get_functions() {
            let optimized = fpm.run_on(&f);
            trace!("try to optimize func {}", f.get_name().to_str().unwrap());
            let oped = if optimized { "yes" } else { "no" };
            trace!("optimized: {}", oped,);
        }
        fpm.finalize();
        mpm.run_on(&llvmmod);
    }
    if op.genir {
        let mut s = out.to_string();
        s.push_str(".ll");
        let llp = Path::new(&s[..]);
        fs::write(llp, llvmmod.to_string()).unwrap();
    }
    tm.write_to_file(&llvmmod, FileType::Assembly, Path::new(&f))
        .unwrap();
    let mut fo = out.to_string();
    let mut out = out.to_string();
    out.push_str(".bc");
    llvmmod.write_bitcode_to_path(Path::new(&out));
    println!("jit executable file writted to: {}", &out);
    let mut cmd = Command::new("clang-14");
    if cfg!(target_os = "linux") {
        trace!("target os is linux");
        cmd.arg("-ltinfo");
    }
    let root = env::var("PL_ROOT");
    if root.is_err() {
        warn!("warn: PL_ROOT not set, skip linking libvm");
        return;
    }
    let root = root.unwrap();
    let vmpath;
    if cfg!(target_os = "windows") {
        cmd = Command::new("clang");
        f = out;
        fo.push_str(".exe");
        vmpath = format!("{}/vm.lib", root);
        cmd.arg("-lws2_32")
            .arg("-lbcrypt")
            .arg("-luserenv")
            .arg("-ladvapi32");
    } else {
        vmpath = format!("{}/libvm.a", root);
        cmd.arg("-pthread").arg("-ldl");
    }

    cmd.arg(format!("-O{}", op.optimization as u32))
        .arg(&f)
        .arg(&vmpath)
        .arg("-o")
        .arg(&fo)
        .arg("-g");
    let res = cmd.status();
    if res.is_err() || !res.as_ref().unwrap().success() {
        warn!("{}", format!("link failed: {}", res.unwrap()).bright_red());
    } else {
        println!("link succ, output file: {}", fo);
    }
    // //  TargetMachine::get_default_triple()
    let time = now.elapsed();
    println!("compile succ, time: {:?}", time);
}
