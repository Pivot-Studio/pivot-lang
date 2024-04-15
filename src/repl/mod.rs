use std::env;
use std::ops::ControlFlow;
use std::path::PathBuf;
use std::process::exit;
use std::sync::atomic::AtomicI32;
use std::sync::{Arc, Mutex};

use clap::Parser;
use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module;
use nom::branch::alt;
use nom::combinator::{eof, map};
use nom::sequence::terminated;
use rustc_hash::{FxHashMap, FxHashSet};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

mod repl_cmd;

use crate::ast::accumulators::{Diagnostics, ModBuffer};
use crate::ast::compiler::{self, ActionType};
use crate::ast::diag::print_diags;
use crate::ast::plmod::GlobalVar;
use crate::db::Database;
use crate::lsp::mem_docs::{self, MemDocsInput};
use crate::nomparser::expression::general_exp;
use crate::nomparser::pkg::use_statement;
use crate::nomparser::statement::{new_variable, statement};
use crate::nomparser::Span;
use crate::utils::read_config::{self, Config, Dependency};
#[cfg(not(windows))]
pub const REPL_VIRTUAL_ENTRY: &str = "@__repl__/__anon__.pi";
#[cfg(not(windows))]
pub const REPL_VIRTUAL_CONF: &str = "@__repl__/Kagari.toml";
#[cfg(windows)]
pub const REPL_VIRTUAL_ENTRY: &str = "@__repl__\\__anon__.pi";
#[cfg(windows)]
pub const REPL_VIRTUAL_CONF: &str = "@__repl__\\Kagari.toml";

static REPL_COUNTER: AtomicI32 = AtomicI32::new(0);

lazy_static::lazy_static! {
    pub static ref REPL_VARIABLES: Arc<Mutex<FxHashMap<String, GlobalVar>>> = Arc::new(Mutex::new(FxHashMap::default()));
    pub static ref LOADED_SET:Arc<Mutex<FxHashSet<PathBuf>>> = Arc::new(Mutex::new(FxHashSet::default()));
}

pub fn start_repl() -> ! {
    // `()` can be used when no completer is required
    let mut rl = DefaultEditor::new().unwrap();
    let mut db = Database::default();
    let mut db2 = Database::default();
    let op = compiler::Options {
        genir: false,
        printast: false,
        flow: false,
        fmt: false,
        optimization: compiler::HashOptimizationLevel::Default,
        jit: true,
        debug: false,
        print_escape: false,
    };

    let action = ActionType::Compile;
    let docs = Arc::new(Mutex::new(mem_docs::MemDocs::default()));
    let docs2 = Arc::new(Mutex::new(mem_docs::MemDocs::default()));
    let mem = MemDocsInput::new(
        &db,
        docs.clone(),
        REPL_VIRTUAL_ENTRY.to_string(),
        op,
        action,
        None,
        None,
    );
    let mem_check = MemDocsInput::new(
        &db2,
        docs2.clone(),
        REPL_VIRTUAL_ENTRY.to_string(),
        op,
        ActionType::Diagnostic,
        None,
        None,
    );
    let docs = mem.docs(&db);
    docs.lock().unwrap().insert(
        &db,
        REPL_VIRTUAL_CONF.to_string(),
        r#"entry = "__anon__.pi"
project = "repl"
[deps]
"#
        .to_owned(),
        REPL_VIRTUAL_CONF.to_string(),
    );
    docs.lock().unwrap().insert(
        &db,
        REPL_VIRTUAL_ENTRY.to_string(),
        "".to_string(),
        REPL_VIRTUAL_ENTRY.to_string(),
    );
    let docs2 = mem_check.docs(&db2);
    docs2.lock().unwrap().insert(
        &db2,
        REPL_VIRTUAL_CONF.to_string(),
        r#"entry = "__anon__.pi"
project = "repl"
[deps]
"#
        .to_owned(),
        REPL_VIRTUAL_CONF.to_string(),
    );
    docs2.lock().unwrap().insert(
        &db2,
        REPL_VIRTUAL_ENTRY.to_string(),
        "".to_string(),
        REPL_VIRTUAL_ENTRY.to_string(),
    );
    let root = env::var("PL_ROOT").unwrap_or_default();
    let _ = rl.load_history(&PathBuf::from(&root).join("history.txt"));
    let ctx = Context::create();
    let mut loaded_set = vec![];
    let mut anon_mods = vec![];
    unsafe { immix::CreateGlobalOrcJITEngine() };
    let mut used_headers = "".to_owned();
    loop {
        let docs = mem.docs(&db);
        let docs2 = mem_check.docs(&db2);
        let readline = rl.readline("pivot> ");
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());
                let line = line.trim().to_owned();
                if line.is_empty() {
                    continue;
                }
                if line.starts_with("@repl") {
                    if try_parse_commands(&line)
                        .map(|repl| match repl.command {
                            repl_cmd::Commands::Load { proj_path, _as } => {
                                if let ControlFlow::Break(_) =
                                    validate_proj_cfg(&proj_path, &db2, &docs2)
                                {
                                    return;
                                }

                                add_deps(
                                    &docs2,
                                    &db,
                                    &_as,
                                    &Dependency {
                                        path: proj_path.to_str().unwrap().to_owned(),
                                        ..Default::default()
                                    },
                                    &db2,
                                    &docs,
                                );
                            }
                            repl_cmd::Commands::LoadDeps { proj_path } => {
                                match validate_proj_cfg(&proj_path, &db2, &docs2) {
                                    ControlFlow::Continue(cfg) => {
                                        for (as_name, path) in &cfg.deps.unwrap_or_default() {
                                            add_deps(&docs2, &db, as_name, path, &db2, &docs);
                                        }
                                    }
                                    ControlFlow::Break(_) => (),
                                }
                            }
                            repl_cmd::Commands::Reload { file_path } => {
                                let file_path = file_path.to_str().unwrap();
                                docs2.lock().unwrap().remove(file_path);
                                docs.lock().unwrap().remove(file_path);
                            }
                        })
                        .is_some()
                    {
                        mem_check
                            .set_docs(&mut db2)
                            .to(Arc::new(Mutex::new(docs2.lock().unwrap().clone())));
                        mem.set_docs(&mut db)
                            .to(Arc::new(Mutex::new(docs.lock().unwrap().clone())));
                    }
                    continue;
                }

                let cl = line.clone();
                let sp = Span::from(cl.as_str());
                let re = alt((
                    map(new_variable, |_| {
                        let mut global = line.replacen("let", "var", 1);
                        if !global.ends_with(';') {
                            global.push(';');
                        }
                        ("".to_owned(), global, "".to_owned(), "".to_owned())
                    }),
                    map(terminated(general_exp, eof), |n| box_expr(&n, &line)),
                    map(statement, |_| {
                        ("".to_owned(), "".to_owned(), line.clone(), line.clone())
                    }),
                    map(use_statement, |_| {
                        let mut used_headers_line = "".to_owned();
                        let mut line = line.clone();
                        if !line.ends_with(';') {
                            line.push(';');
                        }
                        used_headers_line.push_str(&line);
                        used_headers_line.push('\n');

                        (
                            used_headers_line,
                            "".to_owned(),
                            "".to_owned(),
                            "".to_owned(),
                        )
                    }),
                ))(sp);
                if re.is_err() {
                    eprintln!("Error: unsupport statement in REPL mode");
                    continue;
                }
                let (_, (used_headers_line, global, line, check_line)) = re.unwrap();
                let id = REPL_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                let anon_fn = format!(
                    "{}{}{}\n pub fn __anon__{}() void {{ \n{};\nreturn; }}",
                    used_headers, used_headers_line, global, id, check_line
                );
                docs2
                    .lock()
                    .unwrap()
                    .get(REPL_VIRTUAL_ENTRY)
                    .unwrap()
                    .set_text(&mut db2)
                    .to(anon_fn.clone());
                mem_check
                    .set_docs(&mut db2)
                    .to(Arc::new(Mutex::new(docs2.lock().unwrap().clone())));
                let _ = compiler::compile_dry(&db2, mem_check).unwrap();
                let diags = compiler::compile_dry::accumulated::<Diagnostics>(&db2, mem_check);
                let mut errs_num = 0;
                let mut warn_num = 0;
                print_diags(diags, mem_check, &db2, &mut errs_num, &mut warn_num, true);
                if errs_num > 0 {
                    REPL_COUNTER.fetch_sub(1, std::sync::atomic::Ordering::SeqCst);
                    continue;
                }
                let anon_fn = format!(
                    "{}{}{}\n pub fn __anon__{}() void {{ \n{};\nreturn; }}",
                    used_headers, used_headers_line, global, id, line
                );
                // eprintln!("{}", anon_fn);
                used_headers.push_str(&used_headers_line);

                docs.lock()
                    .unwrap()
                    .get(REPL_VIRTUAL_ENTRY)
                    .unwrap()
                    .set_text(&mut db)
                    .to(anon_fn.clone());
                mem.set_docs(&mut db)
                    .to(Arc::new(Mutex::new(docs.lock().unwrap().clone())));
                let plmodule = compiler::compile_dry(&db, mem).unwrap();
                let plmodule = plmodule.plmod(&db);

                REPL_VARIABLES
                    .lock()
                    .unwrap()
                    .extend(plmodule.global_table.clone());

                let mods = compiler::compile_dry::accumulated::<ModBuffer>(&db, mem);

                for m in &mods {
                    if !m.name.starts_with("__anon__") {
                        if LOADED_SET.lock().unwrap().contains(&m.path) {
                            continue;
                        }
                        unsafe {
                            let mo = Module::parse_bitcode_from_buffer(
                                &MemoryBuffer::create_from_memory_range(
                                    &m.buf,
                                    m.path.file_name().unwrap().to_str().unwrap(),
                                ),
                                &ctx,
                            )
                            .unwrap();
                            let p = mo.as_mut_ptr();
                            mo.strip_debug_info();
                            log::trace!("Loaded module, content:\n{}", mo.to_string());
                            loaded_set.push(mo);
                            LOADED_SET.lock().unwrap().insert(m.path.clone());
                            immix::AddModuleToOrcJIT(p as _);

                            log::info!("Loaded module: {:?}", m.name);
                        };
                    }
                }
                for m in &mods {
                    if m.name.starts_with("__anon__") {
                        unsafe {
                            let m = Module::parse_bitcode_from_buffer(
                                &MemoryBuffer::create_from_memory_range(
                                    &m.buf,
                                    m.path.file_name().unwrap().to_str().unwrap(),
                                ),
                                &ctx,
                            )
                            .unwrap();

                            let p = m.as_mut_ptr();
                            m.strip_debug_info();
                            log::trace!("Evaluate module, content:\n{}", m.to_string());
                            anon_mods.push(m);
                            immix::RunExpr(p as _);
                        }
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    let _ = rl.save_history(&PathBuf::from(&root).join("history.txt"));
    exit(0);
}

fn validate_proj_cfg(
    proj_path: &PathBuf,
    db2: &Database,
    docs2: &Arc<Mutex<mem_docs::MemDocs>>,
) -> ControlFlow<(), Config> {
    if !proj_path.exists() {
        eprintln!("Error: project path not found: {:?}", proj_path);
        return ControlFlow::Break(());
    }
    let conf_path = proj_path.join("Kagari.toml");
    if !conf_path.exists() {
        eprintln!("Error: Kagari.toml not found in {:?}", proj_path);
        return ControlFlow::Break(());
    }
    // check proj_path existance
    // check proj_path/Kagari.toml existance
    // check is config valid
    let cfg = read_config::prepare_build_envs(
        db2,
        docs2
            .lock()
            .unwrap()
            .get_file_content(db2, conf_path.to_str().unwrap())
            .unwrap(),
    );
    if cfg.is_err() {
        eprintln!("Error: Kagari.toml is invalid in {:?}", proj_path);
    }
    ControlFlow::Continue(cfg.unwrap())
}

fn add_deps(
    docs2: &Arc<Mutex<mem_docs::MemDocs>>,
    db: &Database,
    _as: &str,
    proj_path: &Dependency,
    db2: &Database,
    docs: &Arc<Mutex<mem_docs::MemDocs>>,
) {
    let mut ori_cfg = read_config::prepare_build_envs(
        db2,
        *docs2.lock().unwrap().get(REPL_VIRTUAL_CONF).unwrap(),
    )
    .unwrap();
    ori_cfg.entry = "__anon__.pi".to_string();
    ori_cfg
        .deps
        .get_or_insert(Default::default())
        .insert(_as.to_owned(), proj_path.clone());
    let new_cfg = toml::to_string(&ori_cfg).unwrap();
    docs2.lock().unwrap().insert(
        db2,
        REPL_VIRTUAL_CONF.to_string(),
        new_cfg.clone(),
        REPL_VIRTUAL_CONF.to_string(),
    );
    docs.lock().unwrap().insert(
        db,
        REPL_VIRTUAL_CONF.to_string(),
        new_cfg,
        REPL_VIRTUAL_CONF.to_string(),
    );
}

fn box_expr(n: &crate::ast::node::NodeEnum, line: &String) -> (String, String, String, String) {
    match &n {
        crate::ast::node::NodeEnum::Primary(p) => box_expr(&p.value, line),
        crate::ast::node::NodeEnum::FuncCall(_) | crate::ast::node::NodeEnum::MacroCallNode(_) =>
        // call expr, skip print for it may return void
        {
            ("".to_owned(), "".to_owned(), line.clone(), line.clone())
        }
        _ => (
            // expr, print it!
            "".to_owned(),
            "".to_owned(),
            format!("println!({})", &line),
            format!("let __rtmp = {}", &line),
        ),
    }
}

fn try_parse_commands(line: &str) -> Option<repl_cmd::REPLCli> {
    // sh lex, parse the cmd
    shlex::split(line).and_then(|args| {
        // use repl_cmd to parse the args
        repl_cmd::REPLCli::try_parse_from(args)
            .map_err(|err| err.print())
            .ok()
    })
}
