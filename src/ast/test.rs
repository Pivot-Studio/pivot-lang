#[cfg(test)]
mod test {
    use std::{
        cell::RefCell,
        fs::remove_file,
        sync::{Arc, Mutex},
    };

    use lsp_types::{
        CompletionItemKind, GotoDefinitionResponse, HoverContents, InlayHintLabel, MarkedString,
    };
    use salsa::{accumulator::Accumulator, storage::HasJar};

    use crate::{
        ast::{
            accumulators::{
                Completions, Diagnostics, DocSymbols, GotoDef, Hints, PLFormat, PLHover,
                PLReferences, PLSignatureHelp,
            },
            compiler::{compile_dry, ActionType},
            diag::DiagCode,
            range::Pos,
        },
        db::Database,
        lsp::mem_docs::{MemDocs, MemDocsInput},
        Db,
    };

    fn test_lsp<'db, A>(
        db: &'db dyn Db,
        params: Option<(Pos, Option<String>)>,
        action: ActionType,
        src: &str,
    ) -> Vec<<A as Accumulator>::Data>
    where
        A: Accumulator,
        dyn Db + 'db: HasJar<<A as Accumulator>::Jar>,
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
            Arc::new(Mutex::new(RefCell::new(docs))),
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
        let (file, diag) = &comps[0];
        assert!(file.contains("test_diag.pi"));
        let mut diag = diag.clone();
        diag.sort_by(|a, b| {
            if a.raw.range.start.line < b.raw.range.start.line
                || (a.raw.range.start.line == b.raw.range.start.line
                    && a.raw.range.start.column < b.raw.range.start.column)
            {
                std::cmp::Ordering::Less
            } else if a.raw.range.start.line == b.raw.range.start.line
                && a.raw.range.start.column == b.raw.range.start.column
            {
                std::cmp::Ordering::Equal
            } else {
                std::cmp::Ordering::Greater
            }
        });
        assert_eq!(diag.len(), 6);
        assert_eq!(
            new_diag_range(10, 14, 10, 15),
            diag[0].get_range().to_diag_range()
        );
        assert_eq!(
            diag[0].get_diag_code(),
            DiagCode::Err(crate::ast::diag::ErrorCode::TYPE_MISMATCH)
        );
        assert_eq!(
            new_diag_range(20, 16, 20, 19),
            diag[1].get_range().to_diag_range()
        );
        assert_eq!(
            diag[1].get_diag_code(),
            DiagCode::Err(crate::ast::diag::ErrorCode::TYPE_MISMATCH)
        );
        assert_eq!(
            new_diag_range(22, 12, 22, 21),
            diag[2].get_range().to_diag_range()
        );
        assert_eq!(
            diag[2].get_diag_code(),
            DiagCode::Err(crate::ast::diag::ErrorCode::INVALID_DIRECT_UNION_CAST)
        );
        assert_eq!(
            new_diag_range(23, 13, 23, 22),
            diag[3].get_range().to_diag_range()
        );
        assert_eq!(
            diag[3].get_diag_code(),
            DiagCode::Err(crate::ast::diag::ErrorCode::INVALID_UNION_CAST)
        );
        assert_eq!(
            new_diag_range(24, 18, 24, 21),
            diag[4].get_range().to_diag_range()
        );
        assert_eq!(
            diag[4].get_diag_code(),
            DiagCode::Err(crate::ast::diag::ErrorCode::UNION_DOES_NOT_CONTAIN_TYPE)
        );
        assert_eq!(
            new_diag_range(25, 13, 25, 21),
            diag[5].get_range().to_diag_range()
        );
        assert_eq!(
            diag[5].get_diag_code(),
            DiagCode::Err(crate::ast::diag::ErrorCode::INVALID_IS_EXPR)
        );
    }
    #[test]
    fn test_memory_leak() {
        let db = &mut Database::default();
        let docs = MemDocs::default();
        let params = Some((
            Pos {
                line: 2,
                column: 8,
                offset: 0,
            },
            None,
        ));
        let pos = if let Some((pos, _)) = params {
            Some(pos)
        } else {
            None
        };

        // let db = Database::default();
        let input = MemDocsInput::new(
            db,
            Arc::new(Mutex::new(RefCell::new(docs))),
            "test/lsp/mod.pi".to_string(),
            Default::default(),
            ActionType::FindReferences,
            params,
            pos,
        );
        let m = compile_dry(db, input).unwrap();
        let mod1 = m.plmod(db);
        let path = dunce::canonicalize("test/lsp/mod.pi")
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();
        input.docs(db).lock().unwrap().borrow_mut().change(
            db,
            new_diag_range(0, 0, 0, 0),
            path,
            "\n".repeat(2),
        );
        input.set_action(db).to(ActionType::Diagnostic);
        input.set_params(db).to(Some((
            Pos {
                line: 1,
                column: 1,
                offset: 0,
            },
            Some("\n".repeat(100)),
        )));
        let m = compile_dry(db, input).unwrap();
        let mod2 = m.plmod(db);
        assert_ne!(mod1, mod2);
        let modstr1 = format!("{:?}", mod1);
        let modstr2 = format!("{:?}", mod2);
        assert_eq!(modstr1.len(), modstr2.len());
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
        assert_eq!(comps[0].len(), 3);
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
            )),
            ActionType::Completion,
            "test/lsp/test_completion.pi",
        );
        assert!(!comps.is_empty());
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
            )),
            ActionType::Completion,
            "test/lsp/test_completion.pi",
        );
        assert!(!comps.is_empty());
        let lables = comps[0].iter().map(|c| c.label.clone()).collect::<Vec<_>>();
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
        let lables = comps[0].to_vec();
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
        let lables = comps[0].to_vec();
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
        assert!(!hints.is_empty());
        assert!(!hints[0].is_empty());
        assert_eq!(
            hints[0][0].label,
            InlayHintLabel::String(": i64".to_string())
        );
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
        if let GotoDefinitionResponse::Scalar(sc) = def[0].clone() {
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
        if let HoverContents::Array(v) = hovers[0].clone().contents {
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
            hovers[0]
                .signatures
                .iter()
                .find(|s| {
                    s.label == "test_sig_help(i: i64, ii: bool)" && s.active_parameter == Some(0)
                })
                .is_some(),
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
            for l in r.iter() {
                locs.push(l.clone());
            }
        }
        // assert_eq!(locs.len(), 3);
        assert!(locs
            .iter()
            .find(|l| {
                let ok = l.uri.to_string().contains("test/lsp/mod.pi");
                if ok {
                    assert!(l.range == new_diag_range(1, 7, 1, 11))
                }
                ok
            })
            .is_some());
        assert!(locs
            .iter()
            .find(|l| {
                let ok = l.uri.to_string().contains("test/lsp/test_completion.pi");
                if ok {
                    assert!(l.range == new_diag_range(38, 11, 38, 15))
                }
                ok
            })
            .is_some());
        assert!(locs
            .iter()
            .find(|l| {
                let ok = l.uri.to_string().contains("test/lsp/mod2.pi");
                if ok {
                    assert!(l.range == new_diag_range(3, 17, 3, 21))
                }
                ok
            })
            .is_some());
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
        assert!(!symbols[0].is_empty());
        let testst = symbols[0].iter().filter(|s| s.name == "test").last();
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
        let name1fn = symbols[0].iter().filter(|s| s.name == "name1").last();
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
    #[cfg(feature = "jit")]
    fn test_jit() {
        use crate::ast::compiler::{compile, Options};
        use std::path::PathBuf;
        let _l = crate::utils::plc_new::tests::TEST_COMPILE_MUTEX
            .lock()
            .unwrap();
        let out = "testjitout";
        let docs = MemDocs::default();
        let db = Database::default();
        let input = MemDocsInput::new(
            &db,
            Arc::new(Mutex::new(RefCell::new(docs))),
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
            },
        );
        assert!(
            crate::ast::compiler::run(
                &PathBuf::from(outplb).as_path(),
                inkwell::OptimizationLevel::None,
            ) == 0,
            "jit compiled program exit with non-zero status"
        );
    }
    #[test]
    fn test_compile() {
        let out = "testout";
        let exe = PathBuf::from(out);
        #[cfg(target_os = "windows")]
        let exe = exe.with_extension("exe");
        _ = remove_file(&exe);
        let _l = crate::utils::plc_new::tests::TEST_COMPILE_MUTEX
            .lock()
            .unwrap();
        use std::{path::PathBuf, process::Command};

        use crate::ast::compiler::{compile, Options};

        let docs = MemDocs::default();
        let mut db = Database::default();
        let input = MemDocsInput::new(
            &db,
            Arc::new(Mutex::new(RefCell::new(docs))),
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
            },
        );
        let exe = dunce::canonicalize(&exe)
            .unwrap_or_else(|_| panic!("static compiled file not found {:?}", exe));
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
        input.set_action(&mut db).to(ActionType::PrintAst);
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
                jit: true,
            },
        );
        test_lsp::<Completions>(
            &Database::default(),
            Some((
                Pos {
                    line: 10,
                    column: 6,
                    offset: 0,
                },
                None,
            )),
            ActionType::LspFmt,
            "test/main.pi",
        );
    }

    #[test]
    fn test_fmt() {
        let testfile = "test/fmt/test_fmt.pi";
        let text_edit =
            test_lsp::<PLFormat>(&Database::default(), None, ActionType::LspFmt, testfile);
        debug_assert!(text_edit[0].is_empty());
    }
}
