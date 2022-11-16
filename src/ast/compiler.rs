use super::{dot, node::program::ModWrapper};
use crate::{
    ast::{
        accumulators::{Diagnostics, ModBuffer},
        node::program::Program,
    },
    lsp::mem_docs::{FileCompileInput, MemDocsInput},
    nomparser::parse,
    utils::read_config::{get_config, get_config_path},
    Db,
};
use colored::Colorize;
use inkwell::{
    context::Context,
    module::Module,
    passes::{PassManager, PassManagerBuilder},
    targets::{FileType, InitializationConfig, Target, TargetMachine},
    OptimizationLevel,
};
use rustc_hash::FxHashSet;
use std::{
    env,
    fs::{self, remove_file},
    path::Path,
    time::Instant,
};
use std::{path::PathBuf, process::Command};

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, Copy)]
pub struct Options {
    pub verbose: bool,
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

#[salsa::tracked(lru = 32)]
pub fn compile_dry(db: &dyn Db, docs: MemDocsInput) {
    let path = get_config_path(docs.file(db).to_string());
    if path.is_err() {
        eprintln!("lsp error: {}", path.err().unwrap());
        return;
    }
    let confinput = FileCompileInput::new(
        db,
        path.clone().unwrap(),
        "".to_string(),
        docs,
        Default::default(),
        Default::default(),
    );

    let re = get_config(db, confinput.get_file_content(db).unwrap());
    if re.is_err() {
        eprintln!("lsp error: {}", re.err().unwrap());
        return;
    }
    let mut config = re.unwrap();
    let buf = PathBuf::from(path.unwrap());
    let parant = buf.parent().unwrap();
    config.entry = parant.join(config.entry).to_str().unwrap().to_string();
    let file = config.entry.clone();
    config.root = parant.to_str().unwrap().to_string();
    let input = FileCompileInput::new(
        db,
        file.clone(),
        PathBuf::from(file)
            .parent()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string(),
        docs,
        Default::default(),
        config,
    );
    compile_dry_file(db, input);
}

#[salsa::tracked(lru = 32)]
pub fn compile_dry_file(db: &dyn Db, docs: FileCompileInput) -> Option<ModWrapper> {
    // eprintln!("compile_dry_file: {:?}", docs.debug_all(db));
    let re = docs.get_file_content(db);
    if re.is_none() {
        return None;
    }
    let src = re.unwrap();
    let parse_result = parse(db, src);
    if let Err(e) = parse_result {
        eprintln!("{}", e);
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
                e.print(&path);
                if e.is_err() {
                    errs_num = errs_num + 1
                }
            }
        }
        if errs_num > 0 {
            if errs_num == 1 {
                println!(
                    "{}",
                    format!("compile failed: there is {} error", errs_num).bright_red()
                );
                println!("{}", format!("{}", dot::ERROR));
                return;
            }
            println!(
                "{}",
                format!("compile failed: there are {} errors", errs_num).bright_red()
            );
            println!("{}", format!("{}", dot::TOOMANYERROR));
            return;
        }
    }
    if op.printast {
        let time = now.elapsed();
        println!("print ast done, time: {:?}", time);
        return;
    }
    if op.fmt {
        let time = now.elapsed();
        println!("gen source done, time: {:?}", time);
        return;
    }
    let mut mods = compile_dry::accumulated::<ModBuffer>(db, docs);
    let ctx = Context::create();
    let m = mods.pop().unwrap();
    let llvmmod = Module::parse_bitcode_from_path(m.clone(), &ctx).unwrap();
    let mut set = FxHashSet::default();
    set.insert(m.clone());
    _ = remove_file(m.clone()).unwrap();
    // println!("rm {}", m.to_str().unwrap());
    for m in mods {
        if set.contains(&m) {
            continue;
        }
        set.insert(m.clone());
        // println!("{}", m.clone().to_str().unwrap());
        _ = llvmmod.link_in_module(Module::parse_bitcode_from_path(m.clone(), &ctx).unwrap());
        _ = remove_file(m.clone()).unwrap();
        // println!("rm {}", m.to_str().unwrap());
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
        println!("fpm init: {}", b);
        for f in llvmmod.get_functions() {
            let optimized = fpm.run_on(&f);
            if op.verbose {
                println!("try to optimize func {}", f.get_name().to_str().unwrap());
                let oped = if optimized { "yes" } else { "no" };
                println!("optimized: {}", oped,);
            }
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
        println!("target os is linux");
        cmd.arg("-ltinfo");
    }
    let root = env::var("PL_ROOT");
    if root.is_err() {
        println!("warn: PL_ROOT not set, skip linking libvm");
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
        println!("{}", format!("link failed: {}", res.unwrap()).bright_red());
        println!("warning: link with pivot lang vm failed, could be caused by libvm not found.");
    } else {
        println!("link succ, output file: {}", fo);
    }
    // //  TargetMachine::get_default_triple()
    let time = now.elapsed();
    println!("compile succ, time: {:?}", time);
}
#[cfg(test)]
mod test {
    use std::{
        cell::RefCell,
        sync::{Arc, Mutex},
    };

    use salsa::{accumulator::Accumulator, storage::HasJar};

    use crate::{
        ast::{accumulators::Completions, range::Pos},
        db::Database,
        lsp::mem_docs::{MemDocs, MemDocsInput},
        Db,
    };

    use super::{compile_dry, ActionType};

    fn test_lsp<'db, A>(
        db: &'db dyn Db,
        params: Option<(Pos, Option<String>, ActionType)>,
        action: ActionType,
        src: &str,
    ) -> Vec<<A as Accumulator>::Data>
    where
        A: Accumulator,
        dyn Db + 'db: HasJar<<A as Accumulator>::Jar>,
    {
        let docs = MemDocs::new();
        // let db = Database::default();
        let input = MemDocsInput::new(
            db,
            Arc::new(Mutex::new(RefCell::new(docs))),
            src.to_string(),
            Default::default(),
            action,
            params,
        );
        compile_dry(db, input);
        compile_dry::accumulated::<A>(db, input)
    }

    #[test]
    fn test_struct_field_completion() {
        let comps = test_lsp::<Completions>(
            &Database::default(),
            Some((
                Pos {
                    line: 9,
                    column: 10,
                    offset: 0,
                },
                Some(".".to_string()),
                ActionType::Completion,
            )),
            ActionType::Completion,
            "test/lsp/test_completion.pi",
        );
        assert_eq!(comps.len(), 1);
        let compstr = vec!["a", "b", "c"];
        for comp in comps[0].iter() {
            assert!(compstr.contains(&comp.label.as_str()));
        }
    }

    #[test]
    fn test_completion() {
        let comps = test_lsp::<Completions>(
            &Database::default(),
            Some((
                Pos {
                    line: 10,
                    column: 6,
                    offset: 0,
                },
                None,
                ActionType::Completion,
            )),
            ActionType::Completion,
            "test/lsp/test_completion.pi",
        );
        assert_eq!(comps.len(), 1);
        let lables = comps[0].iter().map(|c| c.label.clone()).collect::<Vec<_>>();
        assert!(lables.contains(&"test1".to_string()));
        assert!(lables.contains(&"name".to_string()));
        assert!(lables.contains(&"if".to_string()));
    }
    #[test]
    fn test_type_completion() {
        let comps = test_lsp::<Completions>(
            &Database::default(),
            Some((
                Pos {
                    line: 5,
                    column: 7,
                    offset: 0,
                },
                Some(":".to_string()),
                ActionType::Completion,
            )),
            ActionType::Completion,
            "test/lsp/test_completion.pi",
        );
        assert_eq!(comps.len(), 1);
        let lables = comps[0].iter().map(|c| c.label.clone()).collect::<Vec<_>>();
        // assert!(lables.contains(&"test".to_string())); TODO: self refernece
        assert!(lables.contains(&"i64".to_string()));
        assert!(!lables.contains(&"name".to_string()));
        assert!(lables.contains(&"test1".to_string()));
    }
    #[test]
    #[cfg(feature = "jit")]
    fn test_jit() {
        use std::path::PathBuf;

        use crate::ast::compiler::{compile, run, Options};

        let docs = MemDocs::new();
        let mut db = Database::default();
        let input = MemDocsInput::new(
            &db,
            Arc::new(Mutex::new(RefCell::new(docs))),
            "test/main.pi".to_string(),
            Default::default(),
            ActionType::Compile,
            None,
        );
        let outplb = "testout.bc";
        let out = "testout";
        compile(
            &db,
            input,
            out.to_string(),
            Options {
                verbose: true,
                optimization: crate::ast::compiler::HashOptimizationLevel::Aggressive,
                genir: true,
                printast: false,
                fmt: false,
            },
        );
        run(
            &PathBuf::from(outplb).as_path(),
            inkwell::OptimizationLevel::Default,
        );
        input.set_action(&mut db).to(ActionType::PrintAst);
        compile(
            &db,
            input,
            out.to_string(),
            Options {
                verbose: true,
                optimization: crate::ast::compiler::HashOptimizationLevel::Aggressive,
                genir: false,
                printast: true,
                fmt: false,
            },
        );
        input.set_action(&mut db).to(ActionType::Fmt);
        compile(
            &db,
            input,
            out.to_string(),
            Options {
                verbose: true,
                optimization: crate::ast::compiler::HashOptimizationLevel::Aggressive,
                genir: false,
                printast: true,
                fmt: true,
            },
        );
    }
}
