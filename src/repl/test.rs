use std::collections::VecDeque;

struct TestEditor {
    input: VecDeque<String>,
    err_expects: VecDeque<bool>,
}

impl TestEditor {
    fn new(input: VecDeque<String>, err_expects: VecDeque<bool>) -> Self {
        Self { input, err_expects }
    }
}

impl super::editor::TermEditor for TestEditor {
    fn readline(&mut self, _prompt: &str) -> Result<String, rustyline::error::ReadlineError> {
        self.input
            .pop_front()
            .ok_or(rustyline::error::ReadlineError::Eof)
    }

    fn set_helper(&mut self, _helper: Option<super::REPLCompleter>) {}

    fn load_history<P: AsRef<std::path::Path> + ?Sized>(
        &mut self,
        _path: &P,
    ) -> Result<(), rustyline::error::ReadlineError> {
        Ok(())
    }

    fn save_history<P: AsRef<std::path::Path> + ?Sized>(
        &mut self,
        _path: &P,
    ) -> Result<(), rustyline::error::ReadlineError> {
        Ok(())
    }

    fn add_history_entry<S: AsRef<str> + Into<String>>(
        &mut self,
        _line: S,
    ) -> Result<bool, rustyline::error::ReadlineError> {
        Ok(true)
    }
    fn assert_err(&mut self, err: bool) {
        let expect = self.err_expects.pop_front().unwrap();
        assert_eq!(
            err, expect,
            "expect err: {}, got: {}. buffer remains: {:#?}",
            expect, err, self.input
        );
    }
}

#[test]
fn test_repl() {
    let rl = TestEditor::new(
        VecDeque::from(vec![
            "let a = 1".to_owned(),
            "a".to_owned(),
            "b".to_owned(),
            "let a = 2;".to_owned(),
            "use std::cols::hashtable;".to_owned(),
            "@repl load test --as test".to_owned(),
            "use test::main".to_owned(),
            "main::main()".to_owned(),
            "@repl load-deps test".to_owned(),
            "use project2::main::test".to_owned(),
            "test::test()".to_owned(),
            "@repl reload test/main.pi".to_owned(),
            "main::main()".to_owned(),
            "@repl watch test".to_owned(),
            "main::main()".to_owned(),
            "@repl config".to_owned(),
            "@repl symbol".to_owned(),
        ]),
        VecDeque::from(vec![
            false, false, true, false, false, false, false, false, false, false, false, false,
            false, false, false, false, false,
        ]),
    );
    super::start_repl(rl);
}
