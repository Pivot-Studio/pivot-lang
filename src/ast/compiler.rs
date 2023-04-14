use super::{dot, node::program::ModWrapper};
use crate::{
    ast::{
        accumulators::{Diagnostics, ModBuffer},
        builder::llvmbuilder::get_target_machine,
        node::program::Program,
        pass::MAP_NAMES,
    },
    lsp::mem_docs::{FileCompileInput, MemDocsInput},
    nomparser::parse,
    utils::read_config::get_config_path,
    Db,
};
use ariadne::Source;
use colored::Colorize;
use indicatif::{ProgressBar, ProgressDrawTarget, ProgressStyle};
use inkwell::{
    context::Context,
    module::Module,
    passes::{PassManager, PassManagerBuilder},
    OptimizationLevel,
};
use lazy_static::lazy_static;
use log::{debug, info, trace, warn};
use pl_linker::{linker::create_with_target, mun_target::spec::Target};
use rustc_hash::FxHashSet;
use std::{
    env, fs,
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, Copy)]
pub struct Options {
    pub genir: bool,
    pub printast: bool,
    pub flow: bool,
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
    pub fn to_llvm(self) -> OptimizationLevel {
        match self {
            HashOptimizationLevel::None => OptimizationLevel::None,
            HashOptimizationLevel::Less => OptimizationLevel::Less,
            HashOptimizationLevel::Default => OptimizationLevel::Default,
            HashOptimizationLevel::Aggressive => OptimizationLevel::Aggressive,
        }
    }
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
    Compile,
    PrintAst,
    Flow,
    Fmt,
    LspFmt,
    Hint,
    DocSymbol,
    SignatureHelp,
}

lazy_static::lazy_static! {
    pub static ref COMPILE_PROGRESS: ProgressBar = {
        ProgressBar::hidden()
    };
}

#[cfg(feature = "jit")]
pub fn run(p: &Path, opt: OptimizationLevel) {
    type MainFunc = unsafe extern "C" fn() -> i64;
    use inkwell::support;
    vm::reg();
    // FIXME: currently stackmap support on jit code is not possible due to
    // lack of support in inkwell https://github.com/TheDan64/inkwell/issues/296
    // so we disable gc in jit mode for now
    immix::gc_disable_auto_collect();
    support::enable_llvm_pretty_stack_trace();
    let ctx = &Context::create();
    let re = Module::parse_bitcode_from_path(p, ctx).unwrap();
    let engine = re.create_jit_execution_engine(opt).unwrap();
    unsafe {
        engine.run_static_constructors();
        let f = engine.get_function::<MainFunc>("main").unwrap();
        println!("ret = {}", f.call());
    }
}

#[salsa::tracked]
pub fn compile_dry(db: &dyn Db, docs: MemDocsInput) -> Option<ModWrapper> {
    let path = get_config_path(docs.file(db).to_string());
    if path.is_err() {
        log::error!("lsp error: {}", path.err().unwrap());
        return None;
    }

    let input = docs.get_file_params(db, docs.file(db).clone(), true);
    input?;
    let input = input.unwrap();
    let re = compile_dry_file(db, input);
    if let Some(res) = db.get_ref_str() {
        if let Some(plmod) = re {
            plmod
                .plmod(db)
                .get_refs(&res, db, &mut FxHashSet::default());
        }
        db.set_ref_str(None);
    }
    re
}

#[salsa::tracked]
pub fn compile_dry_file(db: &dyn Db, docs: FileCompileInput) -> Option<ModWrapper> {
    if docs.file(db).ends_with(".toml") {
        // skip toml
        return None;
    }
    // eprintln!("compile_dry_file: {:#?}", docs.debug_all(db));
    let re = docs.get_file_content(db);
    re?;
    let src = re.unwrap();
    debug!("src {:#?} id {:?}", src.text(db), src);
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

pub fn run_pass(llvmmod: &Module, op: OptimizationLevel) {
    let pass_manager_builder = PassManagerBuilder::create();
    pass_manager_builder.set_optimization_level(op);
    // Create FPM MPM
    let fpm = PassManager::create(llvmmod);

    let mpm: PassManager<Module> = PassManager::create(());
    if op != OptimizationLevel::None {
        pass_manager_builder.set_size_level(2);
        pass_manager_builder.populate_function_pass_manager(&fpm);
        pass_manager_builder.populate_module_pass_manager(&mpm);
        pass_manager_builder.populate_lto_pass_manager(&mpm, false, true);
    }
    let b = fpm.initialize();
    trace!("fpm init: {}", b);
    for f in llvmmod.get_functions() {
        let optimized = fpm.run_on(&f);
        trace!("try to optimize func {}", f.get_name().to_str().unwrap());
        let oped = if optimized { "yes" } else { "no" };
        trace!("optimized: {}", oped,);
    }
    fpm.finalize();
    mpm.run_on(llvmmod);
}

lazy_static! {
    static ref PROGRESS_STYLE: ProgressStyle = ProgressStyle::with_template(
        "{prefix:.bold.dim} {spinner} [{bar:40.cyan/blue}] {wide_msg:.green} ({elapsed})",
    )
    .unwrap()
    .progress_chars("#>-");
    static ref MSG_PROGRESS_STYLE: ProgressStyle =
        ProgressStyle::with_template("{prefix:.bold.dim} {spinner} {wide_msg:.green} ({elapsed})",)
            .unwrap();
}

#[salsa::tracked]
pub fn compile(db: &dyn Db, docs: MemDocsInput, out: String, op: Options) {
    MAP_NAMES.inner.lock().borrow_mut().clear();
    let pb = &COMPILE_PROGRESS;
    pb.enable_steady_tick(Duration::from_millis(50));
    pb.set_style(PROGRESS_STYLE.clone());
    pb.set_draw_target(ProgressDrawTarget::stderr());
    pb.set_prefix(format!("[{:2}/{:2}]", 1, 3));

    inkwell::execution_engine::ExecutionEngine::link_in_mc_jit();
    immix::register_llvm_gc_plugins();
    let targetdir = PathBuf::from("target");
    if !targetdir.exists() {
        fs::create_dir(&targetdir).unwrap();
    }
    let now = Instant::now();
    compile_dry(db, docs).unwrap();
    pb.finish_with_message("中间代码编译完成");
    let errs = compile_dry::accumulated::<Diagnostics>(db, docs);
    let mut errs_num = 0;
    let mods = compile_dry::accumulated::<ModBuffer>(db, docs);
    if !errs.is_empty() {
        for e in errs.iter() {
            let mut path = e.0.clone();
            for e in e.1.iter() {
                if let Some(src) = e.raw.source.clone() {
                    path = src;
                }
                e.print(
                    &path,
                    move |db, id| {
                        Source::from(docs.get_file_content(db, id.to_string()).unwrap().text(db))
                    },
                    db,
                );
                if e.is_err() {
                    errs_num += 1
                }
            }
        }
        if errs_num > 0 {
            if errs_num == 1 {
                log::error!(
                    "{}",
                    format!("compile failed: there is {} error", errs_num).bright_red()
                );
                println!("{}", dot::ERROR);
                return;
            }
            log::error!(
                "{}",
                format!("compile failed: there are {} errors", errs_num).bright_red()
            );
            println!("{}", dot::TOOMANYERROR);
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
    if op.flow {
        let time = now.elapsed();
        info!("gen flow done, time: {:?}", time);
        return;
    }
    let mut objs = vec![];
    let ctx = Context::create();
    let tm = get_target_machine(op.optimization.to_llvm());
    let llvmmod = ctx.create_module("main");
    let mut set = FxHashSet::default();
    let pb = ProgressBar::new(mods.len() as u64);
    pb.enable_steady_tick(Duration::from_millis(50));
    pb.set_style(PROGRESS_STYLE.clone());
    pb.set_prefix(format!("[{:2}/{:2}]", 2, 3));
    for m in mods {
        pb.inc(1);
        // pb.set_prefix(format!("[{:3}/{:3}]", pb.position(), pb.length().unwrap()));
        let m = m.path;
        if set.contains(&m) {
            continue;
        }
        set.insert(m.clone());
        let o = m.with_extension("o");
        // println!("{}", m.clone().to_str().unwrap());
        let module = Module::parse_bitcode_from_path(m.clone(), &ctx)
            .unwrap_or_else(|_| panic!("parse {} failed", m.to_str().unwrap()));
        pb.set_message(format!(
            "正在优化模块 {} ",
            module.get_name().to_str().unwrap().yellow()
        ));
        run_pass(&module, op.optimization.to_llvm());
        pb.set_message(format!(
            "正在生成模块 {} 的目标文件",
            module.get_name().to_str().unwrap().yellow()
        ));
        module.verify().unwrap();
        tm.write_to_file(&module, inkwell::targets::FileType::Object, &o)
            .unwrap();
        objs.push(o);
        _ = llvmmod.link_in_module(module);
    }
    pb.finish_with_message("目标文件编译优化完成");
    let pb = ProgressBar::new(1);
    pb.enable_steady_tick(Duration::from_millis(50));
    pb.set_style(MSG_PROGRESS_STYLE.clone());
    pb.set_prefix(format!("[{:2}/{:2}]", 3, 3));
    pb.set_message("正在链接目标文件");
    llvmmod.verify().unwrap();
    if op.genir {
        let mut s = out.to_string();
        s.push_str(".ll");
        let llp = Path::new(&s[..]);
        fs::write(llp, llvmmod.to_string()).unwrap();
    }
    let mut fo = out.to_string();
    let mut out = out;
    #[cfg(all(target_os = "macos", target_arch = "x86_64"))]
    let pl_target = Target::search("x86_64-apple-darwin").expect("get target failed");
    #[cfg(not(all(target_os = "macos", target_arch = "x86_64")))]
    let pl_target = Target::host_target().expect("get host target failed");
    out.push_str(".bc");
    llvmmod.set_triple(&tm.get_triple());
    llvmmod.set_data_layout(&tm.get_target_data().get_data_layout());
    llvmmod.write_bitcode_to_path(Path::new(&out));
    // println!("jit executable file writted to: {}", &out);
    let mut t = create_with_target(&pl_target);

    // let mut cmd = Command::new("clang-14");
    if cfg!(target_os = "linux") {
        trace!("target os is linux");
        // cmd.arg("-ltinfo");
    }
    let root = env::var("PL_ROOT");
    if root.is_err() {
        warn!("warn: PL_ROOT not set, skip linking libvm");
        return;
    }
    let root = root.unwrap();
    let vmpath = if cfg!(target_os = "windows") {
        // cmd = Command::new("clang");
        // f = out.clone();
        fo.push_str(".exe");
        format!("{}\\vm.lib", root)
        // cmd.arg("-lws2_32")
        //     .arg("-lbcrypt")
        //     .arg("-luserenv")
        //     .arg("-ladvapi32");
    } else {
        let mut p = PathBuf::from(&root);
        p.push("libvm.a");
        dunce::canonicalize(&p)
            .expect("failed to find libvm")
            .to_str()
            .unwrap()
            .to_string()
        // cmd.arg("-pthread").arg("-ldl");
    };
    for o in objs {
        t.add_object(o.as_path()).unwrap();
    }
    t.add_object(Path::new(&vmpath)).unwrap();
    t.output_to(&fo);
    let res = t.finalize();
    if res.is_err() {
        pb.abandon_with_message("目标文件链接失败");
        eprintln!(
            "{}",
            format!("link failed: {}", res.unwrap_err()).bright_red()
        );
        // eprintln!("target triple: {}", tm.get_triple());
    } else {
        pb.finish_with_message("目标文件链接完成");
        eprintln!("link succ, output file: {}", fo);
    }
    // //  TargetMachine::get_default_triple()
    // let time = now.elapsed();
    // println!("compile succ, time: {:?}", time);
}
