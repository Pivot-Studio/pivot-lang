use std::process::exit;
use std::sync::atomic::AtomicI32;
use std::sync::{Arc, Mutex};

use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module;
use nom::branch::alt;
use nom::combinator::{eof, map};
use nom::sequence::terminated;
use rustc_hash::FxHashMap;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

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
pub const REPL_VIRTUAL_ENTRY: &str = "@__repl__/main.pi";
pub const REPL_VIRTUAL_CONF: &str = "@__repl__/Kagari.toml";

static REPL_COUNTER: AtomicI32 = AtomicI32::new(0);

lazy_static::lazy_static! {
    pub static ref REPL_VARIABLES: Arc<Mutex<FxHashMap<String, GlobalVar>>> = Arc::new(Mutex::new(FxHashMap::default()));
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
        r#"entry = "main.pi"
        project = "repl""#
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
        r#"entry = "main.pi"
            project = "repl""#
            .to_owned(),
        REPL_VIRTUAL_CONF.to_string(),
    );
    docs2.lock().unwrap().insert(
        &db2,
        REPL_VIRTUAL_ENTRY.to_string(),
        "".to_string(),
        REPL_VIRTUAL_ENTRY.to_string(),
    );
    #[cfg(feature = "with-file-history")]
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let ctx = Context::create();
    let mut loaded_set = FxHashMap::default();
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
                    map(terminated(general_exp, eof), |_| {
                        // expr, print it!
                        (
                            "".to_owned(),
                            "".to_owned(),
                            format!("println!({})", &line),
                            format!("let __rtmp = {}", &line),
                        )
                    }),
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
                    if !m.path.to_str().unwrap().starts_with("target/main") {
                        if loaded_set.contains_key(&m.path) {
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
                            loaded_set.insert(m.path.clone(), mo);
                            immix::AddModuleToOrcJIT(p as _)
                        };
                    }
                }

                for m in &mods {
                    if m.path.to_str().unwrap().starts_with("target/main") {
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
    #[cfg(feature = "with-file-history")]
    rl.save_history("history.txt");
    exit(0);
}
