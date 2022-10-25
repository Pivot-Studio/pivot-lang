use std::{
    cell::RefCell,
    fs::read_to_string,
    sync::{Arc, Mutex},
};

use rustc_hash::FxHashMap;

use crate::{
    ast::{
        compiler::{ActionType, Options},
        ctx::Mod,
        range::Pos,
    },
    nomparser::SourceProgram,
    utils::read_config::Config,
    Db,
};

use super::helpers::position_to_offset;

#[derive(Debug, Clone)]
pub struct MemDocs {
    docs: FxHashMap<String, String>,
}

#[salsa::tracked]
pub struct EmitParams {
    #[return_ref]
    pub file: String,
    #[return_ref]
    pub modpath: String,
    pub action: ActionType,
    pub params: Option<(Pos, Option<String>, ActionType)>,
    pub config: Config,
}

#[salsa::input]
pub struct MemDocsInput {
    pub docs: Arc<Mutex<RefCell<MemDocs>>>,
    #[return_ref]
    pub file: String,
    pub op: Options,
    pub action: ActionType,
    pub params: Option<(Pos, Option<String>, ActionType)>,
}

#[salsa::tracked]
pub struct FileCompileInput {
    #[return_ref]
    pub file: String,
    #[return_ref]
    pub modpath: String,
    pub docs: MemDocsInput,
    pub submods: FxHashMap<String, Mod>,
    pub config: Config,
}
#[salsa::tracked]
impl FileCompileInput {
    #[salsa::tracked(lru = 32)]
    pub fn get_file_content(self, db: &dyn Db) -> Option<SourceProgram> {
        // let f = self.file(db);
        // eprintln!("get_file_content {}", f);
        let re = self
            .docs(db)
            .docs(db)
            .lock()
            .unwrap()
            .borrow()
            .get_file_content(self.file(db));
        if let Some(c) = re {
            Some(SourceProgram::new(db, c))
        } else {
            None
        }
    }
    #[salsa::tracked(lru = 32)]
    pub fn get_emit_params(self, db: &dyn Db) -> EmitParams {
        let file = self.file(db);
        if self.docs(db).file(db) != file {
            let action = if self.docs(db).action(db) == ActionType::Compile
                || self.docs(db).action(db) == ActionType::PrintAst
            {
                self.docs(db).action(db)
            } else {
                ActionType::Diagnostic
            };
            return EmitParams::new(
                db,
                file.clone(),
                self.modpath(db).clone(),
                action,
                Some((Default::default(), None, action)),
                self.config(db),
            );
        }
        let action = self.docs(db).action(db);
        let params = self.docs(db).params(db);
        EmitParams::new(
            db,
            file.clone(),
            self.modpath(db).clone(),
            action,
            params,
            self.config(db),
        )
    }
}

#[salsa::tracked]
impl MemDocsInput {
    #[salsa::tracked(lru = 32)]
    pub fn get_file_content(self, db: &dyn Db) -> Option<SourceProgram> {
        let re = self
            .docs(db)
            .lock()
            .unwrap()
            .borrow()
            .get_file_content(self.file(db));
        if let Some(c) = re {
            Some(SourceProgram::new(db, c))
        } else {
            None
        }
    }
}

impl MemDocs {
    pub fn new() -> Self {
        MemDocs {
            docs: FxHashMap::default(),
        }
    }
    pub fn change(&mut self, range: lsp_types::Range, uri: String, text: String) {
        let doc = self.docs.get_mut(&uri.as_str().to_string()).unwrap();
        doc.replace_range(
            position_to_offset(doc, range.start)..position_to_offset(doc, range.end),
            &text,
        );
    }
    pub fn insert(&mut self, key: String, value: String) {
        self.docs.insert(key, value);
    }
    pub fn get(&self, key: &str) -> Option<&String> {
        self.docs.get(key)
    }
    pub fn get_file_content(&self, key: &str) -> Option<String> {
        let mem = self.get(key);
        if let Some(mem) = mem {
            return Some(mem.clone());
        }
        let re = read_to_string(key);
        if let Ok(re) = re {
            return Some(re);
        }
        None
    }
    pub fn get_mut(&mut self, key: &str) -> Option<&mut String> {
        self.docs.get_mut(key)
    }
    pub fn remove(&mut self, key: &str) -> Option<String> {
        self.docs.remove(key)
    }
    pub fn iter(&self) -> impl Iterator<Item = &String> {
        self.docs.values()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::Position;

    #[test]
    fn test_mem_docs() {
        let mut mem_docs = MemDocs::new();
        mem_docs.insert("test".to_string(), "test".to_string());
        assert_eq!(mem_docs.get("test"), Some(&"test".to_string()));
        assert_eq!(mem_docs.get_file_content("test"), Some("test".to_string()));
        assert_eq!(mem_docs.get_file_content("test2"), None);
        assert_eq!(mem_docs.remove("test"), Some("test".to_string()));
        assert_eq!(mem_docs.get("test"), None);
        mem_docs.insert("test".to_string(), "test".to_string());
        mem_docs.change(
            lsp_types::Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 3,
                },
            },
            "test".to_string(),
            "哒哒哒".to_string(),
        );
        assert_eq!(mem_docs.get("test"), Some(&"哒哒哒t".to_string()));
        mem_docs.change(
            lsp_types::Range {
                start: Position {
                    line: 0,
                    character: 1,
                },
                end: Position {
                    line: 0,
                    character: 2,
                },
            },
            "test".to_string(),
            "123".to_string(),
        );
        assert_eq!(mem_docs.get("test"), Some(&"哒123哒t".to_string()));
    }
}
