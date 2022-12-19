use std::{
    cell::RefCell,
    fs::read_to_string,
    path::PathBuf,
    sync::{Arc, Mutex},
};

use rustc_hash::FxHashMap;

use crate::{
    ast::{
        compiler::{ActionType, Options},
        range::Pos,
    },
    nomparser::SourceProgram,
    utils::read_config::{get_config, get_config_path, Config},
    Db,
};

use super::helpers::position_to_offset;

#[derive(Debug, Clone)]
pub struct MemDocs {
    docs: FxHashMap<String, SourceProgram>,
}

#[salsa::tracked]
pub struct EmitParams {
    #[return_ref]
    pub file: String,
    #[return_ref]
    pub modpath: String,
    pub action: ActionType,
    pub params: Option<(Pos, Option<String>)>,
    pub config: Config,
}

#[salsa::input]
pub struct MemDocsInput {
    pub docs: Arc<Mutex<RefCell<MemDocs>>>,
    #[return_ref]
    pub file: String,
    pub op: Options,
    pub action: ActionType,
    pub params: Option<(Pos, Option<String>)>,
    pub edit_pos: Option<Pos>,
}

#[salsa::input]
pub struct MemDocsInputTracked {
    pub docs: Arc<Mutex<RefCell<MemDocs>>>,
    #[return_ref]
    pub file: String,
    pub op: Options,
    pub action: ActionType,
    pub params: Option<(Pos, Option<String>)>,
    pub edit_pos: Option<Pos>,
}

/// 必须有#[id]，否则会导致lru cache失效
#[salsa::tracked]
pub struct FileCompileInput {
    #[return_ref]
    pub file: String,
    #[return_ref]
    pub modpath: String,
    pub docs: MemDocsInput,
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
            .borrow_mut()
            .get_file_content(db, self.file(db));
        if let Some(c) = re {
            Some(c)
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
                || self.docs(db).action(db) == ActionType::Flow
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
                Some((Default::default(), None)),
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
            .borrow_mut()
            .get_file_content(db, self.file(db));
        if let Some(c) = re {
            Some(c)
        } else {
            None
        }
    }
    #[salsa::tracked(lru = 32)]
    pub fn get_file_params(self, db: &dyn Db, f: String, entry: bool) -> Option<FileCompileInput> {
        let mut file = f;
        let path = get_config_path(file.clone());
        if path.is_err() {
            log::error!("lsp error: {}", path.err().unwrap());
            return None;
        }
        let path = path.unwrap();
        let buf = PathBuf::from(path.clone());
        let parant = buf.parent().unwrap();
        let re = get_config(
            db,
            self.docs(db)
                .lock()
                .unwrap()
                .borrow_mut()
                .get_file_content(db, &path)
                .unwrap(),
        );
        if re.is_err() {
            log::error!("lsp error: {}", re.err().unwrap());
            return None;
        }
        let mut config = re.unwrap();
        config.entry = parant.join(config.entry).to_str().unwrap().to_string();
        if entry {
            file = config.entry.clone();
        }
        config.root = parant.to_str().unwrap().to_string();
        Some(FileCompileInput::new(
            db,
            file.clone(),
            parant.to_str().unwrap().to_string(),
            self,
            config,
        ))
    }
}

impl MemDocs {
    pub fn new() -> Self {
        MemDocs {
            docs: FxHashMap::default(),
        }
    }
    pub fn change(&mut self, db: &mut dyn Db, range: lsp_types::Range, uri: String, text: String) {
        let doc = self.docs.get_mut(&uri.as_str().to_string()).unwrap();
        let mut txt = doc.text(db).clone();
        txt.replace_range(
            position_to_offset(&txt, range.start)..position_to_offset(&txt, range.end),
            &text,
        );
        doc.set_text(db).to(txt);
    }
    pub fn insert(&mut self, db: &dyn Db, key: String, value: String, path: String) {
        self.docs.insert(key, SourceProgram::new(db, value, path));
    }
    pub fn get(&self, key: &str) -> Option<&SourceProgram> {
        self.docs.get(key)
    }
    pub fn get_file_content(&mut self, db: &dyn Db, key: &str) -> Option<SourceProgram> {
        let mem = self.get(key);
        if let Some(mem) = mem {
            return Some(mem.clone());
        }
        let re = read_to_string(key);
        if let Ok(re) = re {
            log::info!("read file from path{}", key);
            self.insert(db, key.to_string(), re.clone(), key.to_string());
            return self.get_file_content(db, key);
        }
        None
    }
    pub fn get_mut(&mut self, key: &str) -> Option<&mut SourceProgram> {
        self.docs.get_mut(key)
    }
    pub fn remove(&mut self, key: &str) -> Option<SourceProgram> {
        self.docs.remove(key)
    }
    pub fn iter(&self) -> impl Iterator<Item = &SourceProgram> {
        self.docs.values()
    }
}

#[cfg(test)]
mod tests {
    use crate::db::Database;

    use super::*;
    use lsp_types::Position;

    #[test]
    fn test_mem_docs() {
        let mut db = &mut Database::default();
        let mut mem_docs = MemDocs::new();
        mem_docs.insert(
            db,
            "test".to_string(),
            "test".to_string(),
            "test".to_string(),
        );
        assert_eq!(mem_docs.get("test").unwrap().text(db), "test");
        assert_eq!(
            mem_docs.get_file_content(db, "test").unwrap().text(db),
            "test"
        );
        assert_eq!(mem_docs.get_file_content(db, "test2"), None);
        assert_eq!(mem_docs.remove("test").unwrap().text(db), "test");
        assert_eq!(mem_docs.get("test"), None);
        mem_docs.insert(
            db,
            "test".to_string(),
            "test".to_string(),
            "test".to_string(),
        );
        mem_docs.change(
            db,
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
        assert_eq!(mem_docs.get("test").unwrap().text(db), "哒哒哒t");
        mem_docs.change(
            db,
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
        assert_eq!(mem_docs.get("test").unwrap().text(db), "哒123哒t");
    }
}
