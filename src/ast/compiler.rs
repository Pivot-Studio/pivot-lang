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
use ariadne::Source;
use colored::Colorize;
use inkwell::{
    context::Context,
    module::Module,
    passes::{PassManager, PassManagerBuilder},
    OptimizationLevel,
};
use log::{info, trace, warn};
use pl_linker::{linker::create_with_target, mun_target::spec::Target};
use rustc_hash::FxHashSet;
use std::{
    env,
    fs::{self, remove_file},
    path::{Path, PathBuf},
    time::Instant,
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
    Flow,
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
pub fn compile_dry(db: &dyn Db, docs: MemDocsInput) -> Option<ModWrapper> {
    let path = get_config_path(docs.file(db).to_string());
    if path.is_err() {
        log::error!("lsp error: {}", path.err().unwrap());
        return None;
    }

    let input = docs.get_file_params(db, docs.file(db).clone(), true);
    if input.is_none() {
        return None;
    }
    let re = compile_dry_file(db, input.unwrap());
    if let Some(res) = db.get_ref_str() {
        re.and_then(|plmod| {
            plmod
                .plmod(db)
                .get_refs(&res, db, &mut FxHashSet::default());
            Some(())
        });
    }
    re
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
    immix::register_llvm_gc_plugins();
    let targetdir = PathBuf::from("target");
    if !targetdir.exists() {
        fs::create_dir(&targetdir).unwrap();
    }
    let now = Instant::now();
    compile_dry(db, docs).unwrap();
    let errs = compile_dry::accumulated::<Diagnostics>(db, docs);
    let mut errs_num = 0;
    if errs.len() > 0 {
        for e in errs.iter() {
            let path = &e.0;
            for e in e.1.iter() {
                e.print(
                    &path,
                    Source::from(docs.get_file_content(db, path.clone()).unwrap().text(db)),
                );
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
    if op.flow {
        let time = now.elapsed();
        info!("gen flow done, time: {:?}", time);
        return;
    }
    let mut mods = compile_dry::accumulated::<ModBuffer>(db, docs);
    let mut objs = vec![];
    let ctx = Context::create();
    let m = mods.remove(0).path;
    let tm = get_target_machine(op.optimization.to_llvm());
    let llvmmod = Module::parse_bitcode_from_path(m.clone(), &ctx).unwrap();
    let o = m.with_extension("o");
    tm.write_to_file(&llvmmod, inkwell::targets::FileType::Object, &o)
        .unwrap();
    objs.push(o);
    let mut set = FxHashSet::default();
    set.insert(m.clone());
    _ = remove_file(m.clone()).unwrap();
    log::debug!("rm {}", m.to_str().unwrap());
    for m in mods {
        let m = m.path;
        if set.contains(&m) {
            continue;
        }
        set.insert(m.clone());
        let o = m.with_extension("o");
        // println!("{}", m.clone().to_str().unwrap());
        let module = Module::parse_bitcode_from_path(m.clone(), &ctx)
            .expect(format!("parse {} failed", m.to_str().unwrap()).as_str());
        module.verify().unwrap();
        tm.write_to_file(&module, inkwell::targets::FileType::Object, &o)
            .unwrap();
        objs.push(o);
        _ = llvmmod.link_in_module(module);
    }
    // run_immix_pass(& mut llvmmod as * mut Module as * mut u8);
    llvmmod.verify().unwrap();
    let pass_manager_builder = PassManagerBuilder::create();
    pass_manager_builder.set_optimization_level(op.optimization.to_llvm());
    // Create FPM MPM
    let fpm = PassManager::create(&llvmmod);
    
    let mpm:PassManager<Module> = PassManager::create(());
    if op.optimization != HashOptimizationLevel::None {
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
    mpm.run_on(&llvmmod);
    if op.genir {
        let mut s = out.to_string();
        s.push_str(".ll");
        let llp = Path::new(&s[..]);
        fs::write(llp, llvmmod.to_string()).unwrap();
    }
    let mut fo = out.to_string();
    let mut out = out.to_string();
    let pl_target = Target::host_target().expect("get host target failed");
    out.push_str(".bc");
    llvmmod.set_triple(&tm.get_triple());
    llvmmod.set_data_layout(&tm.get_target_data().get_data_layout());
    llvmmod.write_bitcode_to_path(Path::new(&out));
    println!("jit executable file writted to: {}", &out);
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
    let vmpath;
    if cfg!(target_os = "windows") {
        // cmd = Command::new("clang");
        // f = out.clone();
        fo.push_str(".exe");
        vmpath = format!("{}\\vm.lib", root);
        // cmd.arg("-lws2_32")
        //     .arg("-lbcrypt")
        //     .arg("-luserenv")
        //     .arg("-ladvapi32");
    } else {
        let mut p = PathBuf::from(&root);
        p.push("libvm.a");
        vmpath = dunce::canonicalize(&p)
            .expect("failed to find libvm")
            .to_str()
            .unwrap()
            .to_string();
        // cmd.arg("-pthread").arg("-ldl");
    }
    for o in objs {
        t.add_object(o.as_path()).unwrap();
    }
    t.add_object(Path::new(&vmpath)).unwrap();
    t.output_to(&fo);
    let res = t.finalize();
    if res.is_err() {
        eprint!(
            "{}",
            format!("link failed: {}", res.unwrap_err()).bright_red()
        );
    } else {
        println!("link succ, output file: {}", fo);
    }
    // //  TargetMachine::get_default_triple()
    let time = now.elapsed();
    println!("compile succ, time: {:?}", time);
}
