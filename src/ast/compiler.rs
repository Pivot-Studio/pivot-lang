use super::node::program::ModWrapper;
#[cfg(feature = "llvm")]
use crate::ast::jit_config::IS_JIT;
use crate::{
    ast::{accumulators::ModBuffer, diag::handle_errors, node::program::Program},
    lsp::mem_docs::{FileCompileInput, MemDocsInput},
    nomparser::parse,
    utils::read_config::get_config_path,
    Db,
};
use colored::Colorize;
use indicatif::{ProgressBar, ProgressDrawTarget};
#[cfg(feature = "llvm")]
use inkwell::{
    context::Context,
    module::Module,
    passes::{PassManager, PassManagerBuilder},
    OptimizationLevel,
};
use log::{debug, trace, warn};
#[cfg(feature = "llvm")]
use pl_linker::{linker::create_with_target, mun_target::spec::Target};
use rustc_hash::FxHashSet;
use std::{
    env, fs,
    path::{Path, PathBuf},
    sync::atomic::Ordering,
    time::Duration,
};

mod options;

pub use options::*;
mod progress;
pub use progress::*;
#[cfg(feature = "jit")]
mod jit;

#[cfg(feature = "jit")]
pub use jit::*;

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
    // calculate find references results
    if docs.action(db) != ActionType::FindReferences {
        return re;
    }
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
        log::error!("lsp error: toml file {}", docs.file(db));
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

#[cfg(feature = "llvm")]
pub fn run_pass(llvmmod: &Module, op: OptimizationLevel) {
    let pass_manager_builder = PassManagerBuilder::create();
    // unsafe { llvmaddPass(pass_manager_builder.as_mut_ptr() as _) };
    pass_manager_builder.set_optimization_level(op);
    // Create FPM MPM
    let fpm = PassManager::create(llvmmod);
    let mpm: PassManager<Module> = PassManager::create(());
    if op != OptimizationLevel::None {
        pass_manager_builder.set_size_level(0);
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

#[cfg(not(feature = "llvm"))]
#[salsa::tracked]
pub fn compile(db: &dyn Db, docs: MemDocsInput, out: String, op: Options) {
    unimplemented!()
}

#[cfg(feature = "llvm")]
#[salsa::tracked]
pub fn compile(db: &dyn Db, docs: MemDocsInput, out: String, op: Options) {
    let total_steps = if op.jit { 2 } else { 3 };
    IS_JIT.store(op.jit, Ordering::Relaxed);
    let pb = &COMPILE_PROGRESS;
    pb.enable_steady_tick(Duration::from_millis(50));
    pb.set_style(PROGRESS_STYLE.clone());
    pb.set_draw_target(ProgressDrawTarget::stderr());
    pb.set_prefix(format!("{}[{:2}/{:2}]", LOOKING_GLASS, 1, total_steps));
    #[cfg(feature = "jit")]
    inkwell::execution_engine::ExecutionEngine::link_in_mc_jit();
    immix::register_llvm_gc_plugins();
    let targetdir = PathBuf::from("target");
    if !targetdir.exists() {
        fs::create_dir(&targetdir).unwrap();
    }
    compile_dry(db, docs).unwrap();
    pb.finish_with_message("中间代码编译完成");
    let mods = compile_dry::accumulated::<ModBuffer>(db, docs);
    handle_errors(db, docs);

    let mut objs = vec![];
    let ctx = Context::create();
    let tm = crate::ast::builder::llvmbuilder::get_target_machine(op.optimization.to_llvm());
    let llvmmod = ctx.create_module("main");
    let mut set = FxHashSet::default();
    let pb = ProgressBar::new(mods.len() as u64);
    pb.enable_steady_tick(Duration::from_millis(50));
    pb.set_style(PROGRESS_STYLE.clone());
    pb.set_prefix(format!("{}[{:2}/{:2}]", TRUCK, 2, total_steps));
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
    pb.finish_with_message("中间代码优化完成");
    llvmmod.verify().unwrap();
    if op.genir {
        let mut s = out.to_string();
        s.push_str(".ll");
        let llp = Path::new(&s[..]);
        fs::write(llp, llvmmod.to_string()).unwrap();
    }
    let mut fo = out.to_string();
    let mut out = out;
    let pb = ProgressBar::new(1);
    out.push_str(".bc");
    llvmmod.set_triple(&tm.get_triple());
    llvmmod.set_data_layout(&tm.get_target_data().get_data_layout());
    if op.jit {
        llvmmod.write_bitcode_to_path(Path::new(&out));
        eprintln!("{}jit executable file written to: {}", SPARKLE, &out);
    } else {
        pb.enable_steady_tick(Duration::from_millis(50));
        pb.set_style(MSG_PROGRESS_STYLE.clone());
        pb.set_prefix(format!("{}[{:2}/{:2}]", CLIP, 3, total_steps));
        pb.set_message("正在链接目标文件");
        #[cfg(all(target_os = "macos", target_arch = "x86_64"))]
        let pl_target = Target::search("x86_64-apple-darwin").expect("get target failed");
        #[cfg(not(all(target_os = "macos", target_arch = "x86_64")))]
        let pl_target = Target::host_target().expect("get host target failed");
        let mut t = create_with_target(&pl_target);

        if cfg!(target_os = "linux") {
            trace!("target os is linux");
        }
        let root = env::var("PL_ROOT");
        if root.is_err() {
            warn!("warn: PL_ROOT not set, skip linking libvm");
            return;
        }
        let root = root.unwrap();
        let vmpath = if cfg!(target_os = "windows") {
            fo.push_str(".exe");
            format!("{}\\vm.lib", root)
        } else {
            let mut p = PathBuf::from(&root);
            p.push("libvm.a");
            crate::utils::canonicalize(&p)
                .expect("failed to find libvm")
                .to_str()
                .unwrap()
                .to_string()
        };
        for o in objs {
            t.add_object(o.as_path()).unwrap();
        }
        t.add_object(Path::new(&vmpath)).unwrap();
        t.output_to(&fo);
        let res = t.finalize();
        if res.is_err() {
            pb.abandon_with_message(format!("{}", "目标文件链接失败".red()));
            eprintln!(
                "{}",
                format!("link failed: {}", res.unwrap_err()).bright_red()
            );
        } else {
            pb.finish_with_message("目标文件链接完成");
            eprintln!("{}link succ, output file: {}", SPARKLE, fo);
        }
    }
}
