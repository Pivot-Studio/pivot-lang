use std::path::Path;

use rustyline::error::ReadlineError;

use super::completer::REPLCompleter;

pub trait TermEditor {
    fn set_helper(&mut self, helper: Option<REPLCompleter>);
    fn readline(&mut self, prompt: &str) -> Result<String, ReadlineError>;
    /// Load the history from the specified file.
    fn load_history<P: AsRef<Path> + ?Sized>(&mut self, path: &P) -> Result<(), ReadlineError>;

    /// Save the history in the specified file.
    fn save_history<P: AsRef<Path> + ?Sized>(&mut self, path: &P) -> Result<(), ReadlineError>;

    /// Append new entries in the specified file.
    fn append_history<P: AsRef<Path> + ?Sized>(&mut self, path: &P) -> Result<(), ReadlineError>;

    /// Add a new entry in the history.
    fn add_history_entry<S: AsRef<str> + Into<String>>(
        &mut self,
        line: S,
    ) -> Result<bool, ReadlineError>;

    #[cfg(test)]
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

    fn append_history<P: AsRef<Path> + ?Sized>(&mut self, path: &P) -> Result<(), ReadlineError> {
        self.append_history(path)
    }

    fn add_history_entry<S: AsRef<str> + Into<String>>(
        &mut self,
        line: S,
    ) -> Result<bool, ReadlineError> {
        self.add_history_entry(line)
    }
}
