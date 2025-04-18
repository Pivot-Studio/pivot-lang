#![cfg(test)]
use std::{
    fs::remove_file,
    path::PathBuf,
    sync::{Arc, Mutex},
};

use expect_test::expect_file;
use lsp_types::{CompletionItemKind, GotoDefinitionResponse, HoverContents, MarkedString};
use rustc_hash::FxHashMap;
use salsa::Accumulator;
use wait_timeout::ChildExt;

use crate::{
    ast::{
        accumulators::{
            Completions, Diagnostics, DocSymbols, GotoDef, Hints, PLFormat, PLHover, PLReferences,
            PLSignatureHelp,
        },
        compiler::{compile_dry, ActionType},
        diag::{ensure_no_error, PLDiag},
        node::program::ASSET_PATH,
        range::Pos,
    },
    db::Database,
    lsp::mem_docs::{MemDocs, MemDocsInput},
    Db,
};

fn test_lsp<A>(
    db: &dyn Db,
    params: Option<(Pos, Option<String>)>,
    action: ActionType,
    src: &str,
) -> Vec<A>
where
    A: Accumulator,
{
    let docs = MemDocs::default();
    let pos = if let Some((pos, _)) = params {
        Some(pos)
    } else {
        None
    };

    // let db = Database::default();
    let input = MemDocsInput::new(
        db,
        Arc::new(Mutex::new(docs)),
        src.to_string(),
        Default::default(),
        action,
        params,
        pos,
    );
    compile_dry(db, input).unwrap();
    compile_dry::accumulated::<A>(db, input)
}
#[test]
fn test_diag() {
    let comps = test_lsp::<Diagnostics>(
        &Database::default(),
        None,
        ActionType::Diagnostic,
        "test/lsp_diag/test_diag.pi",
    );
    assert!(!comps.is_empty());
    let mut new_comps = FxHashMap::<String, Vec<PLDiag>>::default();
    for d in &comps {
        let (k, comp) = &d.0;
        new_comps
            .entry(k.to_string())
            .and_modify(|v| {
                v.extend(comp.clone());
            })
            .or_insert(comp.clone());
    }
    for comp in &new_comps {
        test_diag_expect(comp);
    }
}

fn test_diag_expect(comp: (&String, &Vec<super::diag::PLDiag>)) {
    let (f, diag) = &comp;
    let f: PathBuf = f.into();
    let f = "expects/".to_string() + f.file_name().unwrap().to_str().unwrap() + ".expect";
    let diag = sanitize_diag(diag);
    let expected = expect_file![f];
    expected.assert_eq(&format!("{:#?}", diag));
}

fn sanitize_diag(diag: &[super::diag::PLDiag]) -> Vec<super::diag::PLDiag> {
    let mut diag = diag
        .iter()
        .map(|d| {
            let mut d = d.clone();
            d.rm_file();
            d
        })
        .collect::<Vec<_>>();
    diag.sort();
    diag.iter_mut().for_each(|d| d.raw.labels.sort());
    diag
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
        )),
        ActionType::Completion,
        "test/lsp/test_completion.pi",
    );
    assert!(!comps.is_empty());
    assert_eq!(comps[0].0.len(), 3);
    let compstr = ["a", "b", "c"];
    for comp in comps[0].0.iter() {
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
        )),
        ActionType::Completion,
        "test/lsp/test_completion.pi",
    );
    assert!(!comps.is_empty());
    let lables = comps[0]
        .0
        .iter()
        .map(|c| c.label.clone())
        .collect::<Vec<_>>();
    assert!(lables.contains(&"test1".to_string()));
    assert!(lables.contains(&"name".to_string()));
    assert!(lables.contains(&"if".to_string()));
    assert!(lables.contains(&"GLOB".to_string()));
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
        )),
        ActionType::Completion,
        "test/lsp/test_completion.pi",
    );
    assert!(!comps.is_empty());
    let lables = comps[0]
        .0
        .iter()
        .map(|c| c.label.clone())
        .collect::<Vec<_>>();
    assert!(lables.contains(&"test".to_string())); // self refernece
    assert!(lables.contains(&"i64".to_string()));
    assert!(!lables.contains(&"name".to_string()));
    assert!(lables.contains(&"test1".to_string()));
}

#[test]
fn test_st_field_completion() {
    let comps = test_lsp::<Completions>(
        &Database::default(),
        Some((
            Pos {
                line: 37,
                column: 8,
                offset: 0,
            },
            Some(":".to_string()),
        )),
        ActionType::Completion,
        "test/lsp/test_completion.pi",
    );
    assert!(!comps.is_empty());
    let lables = comps[0].0.to_vec();
    assert!(
        lables
            .iter()
            .any(|c| c.label == "mod" && c.kind == Some(CompletionItemKind::MODULE)),
        "mod not found in completion"
    );
}

#[test]
fn test_st_field_exttp_completion() {
    let comps = test_lsp::<Completions>(
        &Database::default(),
        Some((
            Pos {
                line: 38,
                column: 8,
                offset: 0,
            },
            Some(":".to_string()),
        )),
        ActionType::Completion,
        "test/lsp/test_completion.pi",
    );
    assert!(!comps.is_empty());
    let lables = comps[0].0.to_vec();
    assert!(
        lables
            .iter()
            .any(|c| c.label == "pubname" && c.kind == Some(CompletionItemKind::STRUCT)),
        "`pubname` not found in completion"
    );
}
#[test]
fn test_hint() {
    let hints = test_lsp::<Hints>(
        &Database::default(),
        None,
        ActionType::Hint,
        "test/lsp/test_completion.pi",
    );

    let expect = expect_file!["expects/hinttest.expect"];
    expect.assert_eq(&format!("{:#?}", hints));
}
fn new_diag_range(sl: u32, sc: u32, el: u32, ec: u32) -> lsp_types::Range {
    lsp_types::Range {
        start: lsp_types::Position {
            line: sl,
            character: sc,
        },
        end: lsp_types::Position {
            line: el,
            character: ec,
        },
    }
}
#[test]
fn test_goto_def() {
    let def = test_lsp::<GotoDef>(
        &Database::default(),
        Some((
            Pos {
                line: 39,
                column: 14,
                offset: 0,
            },
            None,
        )),
        ActionType::GotoDef,
        "test/lsp/test_completion.pi",
    );
    assert!(!def.is_empty());
    if let GotoDefinitionResponse::Scalar(sc) = def[0].0.clone() {
        assert!(sc.uri.to_string().contains("test/lsp/mod.pi"));
        assert_eq!(sc.range, new_diag_range(1, 7, 1, 11));
    } else {
        panic!("expect goto def to be scalar, found {:?}", def[0])
    }
}
#[test]
fn test_hover_struct() {
    let hovers = test_lsp::<PLHover>(
        &Database::default(),
        Some((
            Pos {
                line: 4,
                column: 19,
                offset: 0,
            },
            None,
        )),
        ActionType::Hover,
        "test/lsp/mod2.pi",
    );
    assert!(!hovers.is_empty());
    if let HoverContents::Array(v) = hovers[0].clone().0.contents {
        if let MarkedString::String(st) = v[0].clone() {
            assert_eq!(st.trim(), "# content".to_string());
        } else {
            panic!("expect hover to be string, found {:?}", hovers[0])
        }
    } else {
        panic!("expect goto def to be scalar, found {:?}", hovers[0])
    }
}

#[test]
fn test_sig_help() {
    let hovers = test_lsp::<PLSignatureHelp>(
        &Database::default(),
        Some((
            Pos {
                line: 11,
                column: 19,
                offset: 0,
            },
            None,
        )),
        ActionType::SignatureHelp,
        "test/lsp/mod2.pi",
    );
    assert!(!hovers.is_empty());
    assert!(
        hovers[0].0.signatures.iter().any(|s| {
            s.label == "test_sig_help(i: i64, ii: bool)" && s.active_parameter == Some(0)
        }),
        "expect to find test_sig_help(i: i64, ii: bool) with active parameter 0, found {:?}",
        hovers[0]
    );
}

#[test]
fn test_find_refs() {
    let refs = test_lsp::<PLReferences>(
        &Database::default(),
        Some((
            Pos {
                line: 2,
                column: 8,
                offset: 0,
            },
            None,
        )),
        ActionType::FindReferences,
        "test/lsp/mod.pi",
    );
    assert!(!refs.is_empty());
    let mut locs = vec![];
    for r in refs.iter() {
        for l in r.0.iter() {
            locs.push(l.clone());
        }
    }
    // assert_eq!(locs.len(), 3);
    assert!(locs.iter().any(|l| {
        let ok = l.uri.to_string().contains("test/lsp/mod.pi");
        if ok {
            assert!(l.range == new_diag_range(1, 7, 1, 11))
        }
        ok
    }));
    assert!(locs.iter().any(|l| {
        let ok = l.uri.to_string().contains("test/lsp/test_completion.pi");
        if ok {
            assert!(l.range == new_diag_range(38, 11, 38, 15))
        }
        ok
    }));
    assert!(locs.iter().any(|l| {
        let ok = l.uri.to_string().contains("test/lsp/mod2.pi");
        if ok {
            assert!(l.range == new_diag_range(3, 17, 3, 21))
        }
        ok
    }));
}

#[test]
fn test_doc_symbol() {
    let symbols = test_lsp::<DocSymbols>(
        &Database::default(),
        None,
        ActionType::DocSymbol,
        "test/lsp/test_completion.pi",
    );
    assert!(!symbols.is_empty());
    assert!(!symbols[0].0.is_empty());
    let testst = symbols[0].0.iter().filter(|s| s.name == "test").last();
    assert!(testst.is_some(), "test struct not found");
    assert_eq!(
        testst.unwrap().kind,
        lsp_types::SymbolKind::STRUCT,
        "expect test's type to be struct, found {:?}",
        testst.unwrap().kind
    );
    let expect = new_diag_range(0, 0, 5, 1);
    assert_eq!(
        testst.unwrap().range,
        expect,
        "expect test's range to be {:?}, found {:?}",
        expect,
        testst.unwrap().range
    );
    let name1fn = symbols[0].0.iter().filter(|s| s.name == "name1").last();
    assert_eq!(
        name1fn.unwrap().kind,
        lsp_types::SymbolKind::FUNCTION,
        "expect name1's type to be struct, found {:?}",
        name1fn.unwrap().kind
    );
    let expect = new_diag_range(26, 0, 29, 1);
    assert_eq!(
        name1fn.unwrap().range,
        expect,
        "expect name1's range to be {:?}, found {:?}",
        expect,
        name1fn.unwrap().range
    );
}

#[test]
// #[ignore = "may get SIGSEGV on jit engine exit somehow, need to investigate"]
#[cfg(all(feature = "jit", not(windows), not(target_os = "linux")))]
fn test_orc_jit() {
    use crate::ast::compiler::{compile, Options};
    use std::path::PathBuf;
    let l = crate::utils::plc_new::tests::TEST_COMPILE_MUTEX
        .lock()
        .unwrap();
    set_test_asset();
    let out = "testjitout";
    let docs = MemDocs::default();
    let db = Database::default();
    let input = MemDocsInput::new(
        &db,
        Arc::new(Mutex::new(docs)),
        "test/main.pi".to_string(),
        Default::default(),
        ActionType::Compile,
        None,
        None,
    );
    let outplb = "testjitout.bc";
    compile(
        &db,
        input,
        out.to_string(),
        Options {
            optimization: crate::ast::compiler::HashOptimizationLevel::None,
            genir: true,
            printast: false,
            flow: false,
            fmt: false,
            jit: true,
            debug: false,
            ..Default::default()
        },
    );
    assert!(
        crate::ast::compiler::run(
            PathBuf::from(outplb).as_path(),
            inkwell::OptimizationLevel::None,
            crate::ast::compiler::EngineType::OrcJit
        ) == 0,
        "jit compiled program exit with non-zero status"
    );
    drop(l);
}

#[test]
fn test_compile() {
    let l = crate::utils::plc_new::tests::TEST_COMPILE_MUTEX
        .lock()
        .unwrap();
    set_test_asset();
    let out = "testout";
    let exe = PathBuf::from(out);
    #[cfg(target_os = "windows")]
    let exe = exe.with_extension("exe");
    _ = remove_file(&exe);
    use std::{path::PathBuf, process::Command};

    use crate::ast::compiler::{compile, Options};

    let docs = MemDocs::default();
    let db = Database::default();
    let input = MemDocsInput::new(
        &db,
        Arc::new(Mutex::new(docs)),
        "test/main.pi".to_string(),
        Default::default(),
        ActionType::Compile,
        None,
        None,
    );
    compile(
        &db,
        input,
        out.to_string(),
        Options {
            optimization: crate::ast::compiler::HashOptimizationLevel::None,
            genir: true,
            printast: false,
            flow: false,
            fmt: false,
            jit: false,
            debug: false,
            ..Default::default()
        },
    );
    ensure_no_error(&db, input);
    let exe = crate::utils::canonicalize(&exe)
        .unwrap_or_else(|_| panic!("static compiled file not found {:?}", exe));
    eprintln!("exec: {:?}", exe);
    eprintln!(
        "start: {:?}",
        std::time::SystemTime::now().duration_since(std::time::SystemTime::UNIX_EPOCH)
    );
    let mut child = Command::new(exe.to_str().unwrap())
        .env("GC_LOG", "info")
        .spawn()
        .expect("failed to execute compiled program");

    let o = child
        .wait_timeout(std::time::Duration::from_secs(500))
        .expect("failed to wait on child");
    if o.is_none() {
        child.kill().expect("failed to kill child");
        panic!("compiled program timed out");
    }
    let o = o.unwrap();
    assert!(
        o.success(),
        "static compiled program failed with status {:?}",
        o,
    );
    drop(l);
}

#[test]
fn test_printast() {
    use crate::ast::compiler::{compile, Options};
    let out = "testout";
    set_test_asset();
    let docs = MemDocs::default();
    let db = Database::default();
    let input = MemDocsInput::new(
        &db,
        Arc::new(Mutex::new(docs)),
        "test/main.pi".to_string(),
        Default::default(),
        ActionType::PrintAst,
        None,
        None,
    );
    compile(
        &db,
        input,
        out.to_string(),
        Options {
            optimization: crate::ast::compiler::HashOptimizationLevel::Aggressive,
            genir: false,
            printast: true,
            flow: false,
            fmt: false,
            jit: false,
            debug: false,
            ..Default::default()
        },
    );
}

#[test]
fn test_fmt() {
    let testfile = "test/fmt/test_fmt.pi";
    let text_edit = test_lsp::<PLFormat>(&Database::default(), None, ActionType::LspFmt, testfile);
    debug_assert!(text_edit[0].0.is_empty());
}

#[test]
fn test_lsp_incremental() {
    let raw_db = Database::default().enable_logging();
    let db = &raw_db;
    let docs = MemDocs::default();
    let input = MemDocsInput::new(
        db,
        Arc::new(Mutex::new(docs)),
        "test/lsp_incremental/main.pi".to_string(),
        Default::default(),
        ActionType::Diagnostic,
        None,
        None,
    );
    compile_dry(db, input).unwrap();
    let _ = raw_db.take_logs();
    compile_dry(db, input).unwrap();
    let ll = raw_db.take_logs();
    assert_eq!(ll.len(), 0);
}

#[test]
fn test_tail_call_opt() {
    let l = crate::utils::plc_new::tests::TEST_COMPILE_MUTEX
        .lock()
        .unwrap();
    set_test_asset();
    let out = "testout2";
    let exe = PathBuf::from(out);
    #[cfg(target_os = "windows")]
    let exe = exe.with_extension("exe");
    _ = remove_file(&exe);
    use std::{path::PathBuf, process::Command};

    use crate::ast::compiler::{compile, Options};

    let docs = MemDocs::default();
    let db = Database::default();
    let input = MemDocsInput::new(
        &db,
        Arc::new(Mutex::new(docs)),
        "test/tail/main.pi".to_string(),
        Default::default(),
        ActionType::Compile,
        None,
        None,
    );
    compile(
        &db,
        input,
        out.to_string(),
        Options {
            optimization: crate::ast::compiler::HashOptimizationLevel::Less,
            genir: true,
            printast: false,
            flow: false,
            fmt: false,
            jit: false,
            debug: false,
            ..Default::default()
        },
    );
    let exe = crate::utils::canonicalize(&exe)
        .unwrap_or_else(|_| panic!("static compiled file not found {:?}", exe));
    eprintln!("exec: {:?}", exe);
    let o = Command::new(exe.to_str().unwrap())
        .output()
        .expect("failed to execute compiled program");
    assert!(
        o.status.success(),
        "static compiled program failed with status {:?} and output {:?} and error {:?}",
        o.status,
        String::from_utf8_lossy(&o.stdout),
        String::from_utf8_lossy(&o.stderr)
    );
    drop(l);
}

#[test]
fn test_assert_index_out_of_bounds() {
    let l = crate::utils::plc_new::tests::TEST_COMPILE_MUTEX
        .lock()
        .unwrap();
    set_test_asset();
    let out = "testout3";
    let exe = PathBuf::from(out);
    #[cfg(target_os = "windows")]
    let exe = exe.with_extension("exe");
    _ = remove_file(&exe);
    use std::{path::PathBuf, process::Command};

    use crate::ast::compiler::{compile, Options};

    let docs = MemDocs::default();
    let db = Database::default();
    let input = MemDocsInput::new(
        &db,
        Arc::new(Mutex::new(docs)),
        "test/arr_bounds/main.pi".to_string(),
        Default::default(),
        ActionType::Compile,
        None,
        None,
    );
    compile(
        &db,
        input,
        out.to_string(),
        Options {
            optimization: crate::ast::compiler::HashOptimizationLevel::Less,
            genir: true,
            printast: false,
            flow: false,
            fmt: false,
            jit: false,
            debug: false,
            ..Default::default()
        },
    );
    let exe = crate::utils::canonicalize(&exe)
        .unwrap_or_else(|_| panic!("static compiled file not found {:?}", exe));
    eprintln!("exec: {:?}", exe);
    let o = Command::new(exe.to_str().unwrap())
        .output()
        .expect("failed to execute compiled program");
    // should trigger index out of bounds, so status should be non-zero
    assert!(!o.status.success(), "should trigger index out of bounds");
    drop(l);
}

#[cfg(test)]
pub(crate) fn set_test_asset() {
    use std::time::SystemTime;

    let mut p = ASSET_PATH.lock().unwrap();
    let duration_since_epoch = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap();
    let timestamp_nanos = duration_since_epoch.as_nanos(); // u128
    *p = format!("target/test{}", timestamp_nanos);
    std::fs::create_dir_all(&*p).unwrap();
}
