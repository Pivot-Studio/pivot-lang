use std::cell::RefCell;
use std::sync::Arc;

use colored::Colorize;
use dyn_fmt::AsStrFormatExt;
use lsp_types::{SemanticTokenType, SemanticTokens};
use nom::Slice;
use parking_lot::Mutex;
use rustyline::completion::{Completer, Pair};
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::validate::Validator;
use rustyline::Helper;

use crate::ast::accumulators::{Completions, PLSemanticTokens};
use crate::ast::compiler::{self, compile_dry};
use crate::ast::range::Pos;
use crate::db::Database;
use crate::lsp::semantic_tokens::SUPPORTED_TYPES;
use crate::MemDocsInput;

use super::REPL_VIRTUAL_ENTRY;

pub struct REPLCompleter {
    db: *mut Database,
    input: MemDocsInput,
    line_no: RefCell<usize>,
}

impl REPLCompleter {
    pub fn new(db: *mut Database, input: MemDocsInput) -> Self {
        Self {
            db,
            input,
            line_no: RefCell::new(0),
        }
    }

    fn analysis<'a>(&self, line: &'a str, col: usize) -> (&'a str, usize, usize, bool) {
        let slice = &line[..col];
        let mut insert = slice;
        let mut insert2 = slice;
        if slice.starts_with("use") {
            insert2 = "";
        } else {
            insert = "";
        }
        let source = REPL_COMLETE_TPL.lock().format(&[insert, insert2]);
        let docs = self.input.docs(unsafe { self.db.as_ref().unwrap() });
        // find which line is `slice` at
        let line_no = source.lines().enumerate().fold(
            0,
            |acc, (i, l)| {
                if l.contains(slice) {
                    i
                } else {
                    acc
                }
            },
        );
        let pos = Pos {
            line: line_no + 1,
            column: col + 1,
            offset: 0,
        };
        docs.lock()
            .unwrap()
            .get(REPL_VIRTUAL_ENTRY)
            .unwrap()
            .set_text(unsafe { self.db.as_mut().unwrap() })
            .to(source);

        self.input
            .set_edit_pos(unsafe { self.db.as_mut().unwrap() })
            .to(Some(pos));
        self.input
            .set_docs(unsafe { self.db.as_mut().unwrap() })
            .to(Arc::new(std::sync::Mutex::new(
                docs.lock().unwrap().clone(),
            )));
        let re = compiler::compile_dry(unsafe { self.db.as_ref().unwrap() }, self.input);
        (slice, col, line_no, re.is_ok())
    }
}

lazy_static::lazy_static! {
    pub static ref REPL_COMLETE_TPL: Arc<Mutex<String>> = Arc::new(Mutex::new(
        "{}\n pub fn __anon__0() void {{ \n{}\nreturn;\n }}".to_owned()));
    static ref COMPL_REGEX: regex::Regex = regex::Regex::new(r"\$\{(\d+):(\w+)\}").unwrap();
}

fn color_token(token: &str, tp: u32) -> String {
    if SUPPORTED_TYPES[tp as usize] == SemanticTokenType::VARIABLE {
        return token.red().to_string();
    }
    if SUPPORTED_TYPES[tp as usize] == SemanticTokenType::STRING {
        return token.green().to_string();
    }
    if SUPPORTED_TYPES[tp as usize] == SemanticTokenType::FUNCTION
        || SUPPORTED_TYPES[tp as usize] == SemanticTokenType::METHOD
    {
        return token.blue().to_string();
    }
    if SUPPORTED_TYPES[tp as usize] == SemanticTokenType::MACRO {
        return token.yellow().to_string();
    }
    if SUPPORTED_TYPES[tp as usize] == SemanticTokenType::TYPE
        || SUPPORTED_TYPES[tp as usize] == SemanticTokenType::NAMESPACE
    {
        return token.bright_yellow().to_string();
    }
    if SUPPORTED_TYPES[tp as usize] == SemanticTokenType::NUMBER {
        return token.bright_green().to_string();
    }

    token.to_string()
}

impl Helper for REPLCompleter {}

impl Hinter for REPLCompleter {
    type Hint = String;
}

impl Validator for REPLCompleter {}

impl Highlighter for REPLCompleter {
    fn highlight<'l>(&self, line: &'l str, _col: usize) -> std::borrow::Cow<'l, str> {
        let line_no = *self.line_no.borrow();
        let mut newtokens = compile_dry::accumulated::<PLSemanticTokens>(
            unsafe { self.db.as_ref().unwrap() },
            self.input,
        );
        if newtokens.is_empty() {
            newtokens.push(SemanticTokens::default());
        }
        let tokens: Vec<lsp_types::SemanticToken> = newtokens[0].data.clone();
        // color the line
        let mut new_line = line.to_owned();
        let mut line_n = 0;
        let mut col = 0;
        let mut offset = 0;
        for token in &tokens {
            line_n += token.delta_line;
            if line_n < line_no as _ {
                continue;
            }
            if line_n > line_no as _ {
                break;
            }
            if token.delta_line > 0 {
                col = token.delta_start;
            } else {
                col += token.delta_start;
            }
            if col > line.len() as _ {
                break;
            }
            let end = col + token.length;
            let end = if end > line.len() as _ {
                line.len() as _
            } else {
                end
            };
            // replace str's token to colored token
            let token = color_token(line.slice(col as _..end as _), token.token_type);
            new_line.replace_range(offset + col as usize..offset + end as usize, &token);
            offset += token.len() - (end - col) as usize;
        }

        std::borrow::Cow::Owned(new_line)
    }

    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        default: bool,
    ) -> std::borrow::Cow<'b, str> {
        let _ = default;
        std::borrow::Cow::Borrowed(prompt)
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> std::borrow::Cow<'h, str> {
        std::borrow::Cow::Borrowed(hint)
    }

    fn highlight_candidate<'c>(
        &self,
        candidate: &'c str, // FIXME should be Completer::Candidate
        completion: rustyline::CompletionType,
    ) -> std::borrow::Cow<'c, str> {
        let _ = completion;
        std::borrow::Cow::Borrowed(candidate)
    }

    fn highlight_char(&self, line: &str, _: usize, _: bool) -> bool {
        let (_, _, line_no, ok) = self.analysis(line, line.len());
        *self.line_no.borrow_mut() = line_no;
        ok
    }
}

impl Completer for REPLCompleter {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        col: usize,
        _ctx: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        let (slice, col, _, _) = self.analysis(line, col);

        let comps = compile_dry::accumulated::<Completions>(
            unsafe { self.db.as_ref().unwrap() },
            self.input,
        );

        // get last world in slice
        let mut last_world = slice
            .split(&[
                ',', '.', ';', ':', ' ', '=', '>', '<', '(', ')', '+', '-', '*', '/', '^', '&',
                '%', '!',
            ])
            .last()
            .unwrap_or_default();
        // eprintln!("last_world: {:?}", last_world);
        // if last world is not alpha numeric, return empty
        let is_alpha = last_world.chars().all(|c| c.is_alphanumeric());
        if !is_alpha {
            last_world = "";
        }
        let start = col - last_world.len();

        let compls = comps
            .iter()
            .flat_map(|v| {
                v.iter()
                    .filter(|i| i.label.starts_with(last_world) && !i.label.is_empty())
                    .map(|i| {
                        Pair {
                            display: i.label.clone(),
                            replacement: {
                                let mut text = i.insert_text.clone().unwrap_or(i.label.to_string());
                                // remove $0, replace ${number:name} with name
                                // e.g. `a.append(${1:str})$0` -> `a.append(str)`
                                text = text.replace("$0", "");
                                text = COMPL_REGEX.replace_all(&text, "$2").to_string();
                                text
                            },
                        }
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
        Ok((start, compls))
    }
}
