use super::node::program::cycle_deps_recover;
use super::node::program::ModWrapper;
use super::node::program::ASSET_PATH;
#[cfg(feature = "llvm")]
use crate::ast::jit_config::IS_JIT;
use crate::{
    ast::{accumulators::ModBuffer, node::program::Program},
    lsp::mem_docs::{FileCompileInput, MemDocsInput},
    nomparser::parse,
    utils::read_config::search_config_file,
    Db,
};

use colored::Colorize;
use indicatif::ProgressBar;
#[cfg(feature = "llvm")]
use inkwell::{
    context::Context,
    module::Module,
    passes::{PassManager, PassManagerBuilder},
    OptimizationLevel,
};
use log::{trace, warn};
#[cfg(feature = "llvm")]
use pl_linker::{linker::create_with_target, mun_target::spec::Target};
use rustc_hash::FxHashSet;
use std::{
    env, fs,
    path::{Path, PathBuf},
    sync::atomic::Ordering,
};

mod options;

pub use options::*;
mod progress;
pub use progress::*;
#[cfg(feature = "jit")]
mod jit;

#[cfg(feature = "jit")]
pub use jit::*;

/// compile is the core function which composes multiple compiler stages together,
/// as it processes all pivot-lang files and generates the executable binary.
///
/// function compile is consisted with three components: (1) parser, (2) llvm ir processor and (3) linker.
/// the (1) parser reads each pivot-lang file and parses them into llvm format IR.
/// the (2) llvm ir processor optimizes the llvm IR and constructs the whole module.
/// the (3) linker converts the llvm module into executable binary depends on the concrete platform.
///
/// todo(griffin): add JIT document because i know nothing about it now.
#[cfg(feature = "llvm")]
#[salsa::tracked]
pub fn compile(db: &dyn Db, docs: MemDocsInput, out: String, op: Options) {
    IS_JIT.store(op.jit, Ordering::Relaxed);

    #[cfg(feature = "jit")]
    inkwell::execution_engine::ExecutionEngine::link_in_mc_jit();

    immix::register_llvm_gc_plugins();
    ensure_target_folder();

    let total_steps = 3;
    let pb = &COMPILE_PROGRESS;
    prepare_prgressbar(
        pb,
        op,
        format!("{}[{:2}/{:2}]", LOOKING_GLASS, 1, total_steps),
    );
    compile_dry(db, docs).unwrap();

    pb.finish_with_message("中间代码分析完成");
    let is_present_only = op.printast || op.flow;
    if is_present_only {
        return;
    }

    let ctx = Context::create();
    // ensure_no_error(db, docs);
    let (llvmmod, files) = process_llvm_ir(db, docs, &ctx, op);

    pl_link(llvmmod, files, out.clone(), op);
}

/// compile_dry compiles the source code of pivot-lang into the pivot-lang AST with a wrapper.
/// the `dry` refers the function ends up parsing at LLVM IR or LSP analysis.
#[salsa::tracked]
pub fn compile_dry(db: &dyn Db, docs: MemDocsInput) -> Option<ModWrapper> {
    let path = search_config_file(docs.file(db).to_string());
    if path.is_err() {
        log::error!("lsp error: {}", path.err().unwrap());
        return None;
    }

    let parser_entry = docs
        .finalize_parser_input(db, docs.file(db).clone(), true)
        .unwrap();

    log::trace!("entering compile_dry_file");
    let re = compile_dry_file(db, parser_entry);

    // calculate find references results for lsp
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

/// compile_dry_file parses the file inside parser_entry into AST,
/// and then emit the llvm IR code represented by Mod according to the entry file AST.
#[salsa::tracked(recovery_fn=cycle_deps_recover)]
pub fn compile_dry_file(db: &dyn Db, parser_entry: FileCompileInput) -> Option<ModWrapper> {
    if parser_entry.file(db).ends_with(".toml") {
        log::error!("lsp error: toml file {}", parser_entry.file(db));
        // skip toml
        return None;
    }

    let entry_file_content = parser_entry.get_file_content(db).unwrap();
    log::trace!(
        "src {:#?} id {:?}",
        entry_file_content.text(db),
        entry_file_content.path(db)
    );

    let parsing_result = parse(db, entry_file_content);
    match parsing_result {
        Err(e) => {
            log::error!("source code parse failed {}", e);
            None
        }
        Ok(root_node) => {
            let program = Program::new(
                db,
                root_node,
                parser_entry.get_emit_params(db),
                parser_entry.docs(db),
                parser_entry.config(db),
                parser_entry.opt(db),
            );
            log::trace!("entering emit");
            Some(program.emit(db))
        }
    }
}

/// process_llvm_ir retrieves llvm IR from db and docs and optimizes it with the help of llvm optimizer.
#[cfg(feature = "llvm")]
pub fn process_llvm_ir<'a>(
    db: &dyn Db,
    docs: MemDocsInput,
    ctx: &'a Context,
    op: Options,
) -> (Module<'a>, Vec<PathBuf>) {
    let mods = compile_dry::accumulated::<ModBuffer>(db, docs);

    let total_steps = 3;
    let pb = ProgressBar::hidden();
    prepare_prgressbar(&pb, op, format!("{}[{:2}/{:2}]", TRUCK, 2, total_steps));
    pb.set_length(mods.len() as u64);

    let mut set = FxHashSet::default();
    let mut output_files = vec![];

    let tm = crate::ast::builder::llvmbuilder::get_target_machine(op.optimization.to_llvm());
    let llvmmod = ctx.create_module("main");

    for m in mods {
        pb.inc(1);
        let m = m.path;
        if set.contains(&m) {
            continue;
        }
        set.insert(m.clone());
        let o = m.with_extension("o");
        // println!("{}", m.clone().to_str().unwrap());
        let module = Module::parse_bitcode_from_path(m.clone(), ctx)
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
        if op.debug {
            // if debug, generate one obj file per file, or debug info will be lost
            tm.write_to_file(&module, inkwell::targets::FileType::Object, &o)
                .unwrap();
            output_files.push(o);
        }
        module.get_global("llvm.used").map(|v| unsafe {
            v.delete();
        });
        llvmmod.link_in_module(module).unwrap();
    }
    llvmmod.get_functions().for_each(|f| {
        if f.get_name().to_str().unwrap().contains("_visitorf@") && f.get_basic_blocks().is_empty()
        {
            let b = llvmmod.get_context().create_builder();
            b.position_at_end(llvmmod.get_context().append_basic_block(f, "entry"));
            b.build_return(None).unwrap();
        }
        if f.get_linkage() == inkwell::module::Linkage::LinkOnceAny {
            f.set_linkage(inkwell::module::Linkage::External);
        }
    });
    unsafe {
        immix::run_module_pass(
            llvmmod.as_mut_ptr() as _,
            op.optimization as _,
            op.debug as _,
        )
    };
    if !op.debug {
        tm.write_to_file(
            &llvmmod,
            inkwell::targets::FileType::Object,
            Path::new("out.o"),
        )
        .unwrap();
        output_files.push("out.o".into());
    }
    pb.finish_with_message("中间代码优化完成");
    (llvmmod, output_files)
}

#[cfg(feature = "llvm")]
pub fn pl_link(llvmmod: Module, oxbjs: Vec<PathBuf>, out: String, op: Options) {
    llvmmod.verify().unwrap();
    llvmmod.strip_debug_info();
    if op.genir {
        let mut s = out.to_string();
        s.push_str(".ll");
        let llp = Path::new(&s[..]);
        fs::write(llp, llvmmod.to_string()).unwrap();
    }
    let mut fo = out.to_string();
    let mut out = out;

    out.push_str(".bc");

    let total_steps = 3;
    let pb = ProgressBar::hidden();
    prepare_prgressbar(
        &pb,
        op,
        format!("{}[{:2}/{:2}]", CLIP, total_steps, total_steps),
    );
    pb.set_length(1);

    if op.jit {
        pb.set_message("正在输出jit文件");
        llvmmod.write_bitcode_to_path(Path::new(&out));
        pb.finish_with_message("JIT完成文件写入");

        eprintln!("jit executable file written to: {}", &out);
        return;
    }

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
    for o in oxbjs {
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
        // pass_manager_builder.populate_lto_pass_manager(&mpm, false, true);
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

/// ensure_target_folder ensures the existence of target folder. It tries to create target folder if the folder doesn't exist.
/// It will panic if fail to ensure.
/// todo(griffin): move it to util crate as it's not the compiler logic
pub fn ensure_target_folder() {
    let targetdir = PathBuf::from(ASSET_PATH.lock().unwrap().as_str());
    if !targetdir.exists() {
        fs::create_dir_all(&targetdir).unwrap();
    }
}
