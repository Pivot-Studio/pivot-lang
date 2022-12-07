#[cfg(test)]
mod test {
    use std::{
        cell::RefCell,
        sync::{Arc, Mutex},
    };

    use lsp_types::InlayHintLabel;
    use salsa::{accumulator::Accumulator, storage::HasJar};

    use crate::{
        ast::{
            accumulators::{Completions, DocSymbols, Hints},
            compiler::{compile_dry, ActionType},
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
        let docs = MemDocs::new();
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
            )),
            ActionType::Completion,
            "test/lsp/test_completion.pi",
        );
        assert!(comps.len() > 0);
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
        assert!(comps.len() > 0);
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
        assert!(comps.len() > 0);
        let lables = comps[0].iter().map(|c| c.label.clone()).collect::<Vec<_>>();
        assert!(lables.contains(&"test".to_string())); // self refernece
        assert!(lables.contains(&"i64".to_string()));
        assert!(!lables.contains(&"name".to_string()));
        assert!(lables.contains(&"test1".to_string()));
    }
    #[test]
    fn test_hint() {
        let hints = test_lsp::<Hints>(
            &Database::default(),
            None,
            ActionType::Hint,
            "test/lsp/test_completion.pi",
        );
        assert!(hints.len() > 0);
        assert!(hints[0].len() > 0);
        assert_eq!(
            hints[0][0].label,
            InlayHintLabel::String(": i64".to_string())
        );
    }
    fn new_range(sl: u32, sc: u32, el: u32, ec: u32) -> lsp_types::Range {
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
    fn test_doc_symbol() {
        let symbols = test_lsp::<DocSymbols>(
            &Database::default(),
            None,
            ActionType::DocSymbol,
            "test/lsp/test_completion.pi",
        );
        assert!(symbols.len() > 0);
        assert!(symbols[0].len() > 0);
        let testst = symbols[0].iter().filter(|s| s.name == "test").last();
        assert!(testst.is_some(), "test struct not found");
        assert_eq!(
            testst.unwrap().kind,
            lsp_types::SymbolKind::STRUCT,
            "expect test's type to be struct, found {:?}",
            testst.unwrap().kind
        );
        let expect = new_range(0, 0, 5, 1);
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
        let expect = new_range(26, 0, 29, 1);
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
            None,
        );
        let outplb = "testout.bc";
        let out = "testout";
        compile(
            &db,
            input,
            out.to_string(),
            Options {
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
                optimization: crate::ast::compiler::HashOptimizationLevel::Aggressive,
                genir: false,
                printast: true,
                fmt: false,
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
}
