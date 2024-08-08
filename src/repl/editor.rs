use std::{io::BufRead, path::Path, sync::atomic::AtomicBool};

use rustyline::error::ReadlineError;

use super::completer::REPLCompleter;

pub trait TermEditor {
    fn set_helper(&mut self, helper: Option<REPLCompleter>);
    fn readline(&mut self, prompt: &str) -> Result<String, ReadlineError>;
    /// Load the history from the specified file.
    fn load_history<P: AsRef<Path> + ?Sized>(&mut self, path: &P) -> Result<(), ReadlineError>;

    /// Save the history in the specified file.
    fn save_history<P: AsRef<Path> + ?Sized>(&mut self, path: &P) -> Result<(), ReadlineError>;

    /// Add a new entry in the history.
    fn add_history_entry<S: AsRef<str> + Into<String>>(
        &mut self,
        line: S,
    ) -> Result<bool, ReadlineError>;

    fn assert_err(&mut self, _err: bool) {
        // noop
    }
}

pub fn default_editor() -> rustyline::Editor<REPLCompleter, rustyline::history::FileHistory> {
    rustyline::Editor::<REPLCompleter, rustyline::history::FileHistory>::new().unwrap()
}

impl TermEditor for rustyline::Editor<REPLCompleter, rustyline::history::FileHistory> {
    fn set_helper(&mut self, helper: Option<REPLCompleter>) {
        self.set_helper(helper);
    }

    fn readline(&mut self, prompt: &str) -> Result<String, ReadlineError> {
        self.readline(prompt)
    }

    fn load_history<P: AsRef<Path> + ?Sized>(&mut self, path: &P) -> Result<(), ReadlineError> {
        self.load_history(path)
    }

    fn save_history<P: AsRef<Path> + ?Sized>(&mut self, path: &P) -> Result<(), ReadlineError> {
        self.save_history(path)
    }

    fn add_history_entry<S: AsRef<str> + Into<String>>(
        &mut self,
        line: S,
    ) -> Result<bool, ReadlineError> {
        self.add_history_entry(line)
    }
}

pub struct HeadlessEditor;

static INIT: AtomicBool = AtomicBool::new(false);

impl TermEditor for HeadlessEditor {
    fn set_helper(&mut self, _helper: Option<REPLCompleter>) {}

    fn readline(&mut self, _prompt: &str) -> Result<String, ReadlineError> {
        if INIT.load(std::sync::atomic::Ordering::Relaxed) {
            println!("@@@done@@@");
            eprintln!("@@@done@@@");
        }
        INIT.store(true, std::sync::atomic::Ordering::Relaxed);
        // 2K is a good default buffer size, but definitely do
        // analyze the situation and adjust its size accordingly
        let mut buffer = String::with_capacity(2048);
        // Lock our standard input to eliminate synchronization overhead (unlocks when dropped)
        let mut stdin = std::io::stdin().lock();

        // Read our first line.
        loop {
            let start = buffer.len();
            stdin.read_line(&mut buffer).expect("Failed to read line");
            // read untile line "@@@done@@@"
            if buffer.len() > start && buffer[start..].contains("@@@done@@@") {
                buffer = buffer[..start].to_string();
                break;
            }
        }
        Ok(buffer)
    }

    fn load_history<P: AsRef<Path> + ?Sized>(&mut self, _path: &P) -> Result<(), ReadlineError> {
        Ok(())
    }

    fn save_history<P: AsRef<Path> + ?Sized>(&mut self, _path: &P) -> Result<(), ReadlineError> {
        Ok(())
    }

    fn add_history_entry<S: AsRef<str> + Into<String>>(
        &mut self,
        _line: S,
    ) -> Result<bool, ReadlineError> {
        Ok(true)
    }
    fn assert_err(&mut self, err: bool) {
        if err {
            eprintln!("@@@error@@@");
        }
    }
}
