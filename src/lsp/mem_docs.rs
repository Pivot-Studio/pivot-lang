use std::{
    fs::read_to_string,
    path::PathBuf,
    sync::{Arc, Mutex},
};

use log::debug;
use rustc_hash::FxHashMap;

use crate::{
    ast::{
        compiler::{ActionType, HashOptimizationLevel, Options},
        range::Pos,
    },
    nomparser::SourceProgram,
    utils::read_config::{prepare_build_envs, search_config_file, Config},
    Db,
};

use super::helpers::position_to_offset;

#[derive(Debug, Clone, Default)]
pub struct MemDocs {
    docs: FxHashMap<String, SourceProgram>,
}

/// EmitParams is used to pass information during processing LLVM IR or LSP work
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
    pub docs: Arc<Mutex<MemDocs>>,

    /// file is the path of file focused by the editor now
    #[return_ref]
    pub file: String,
    pub op: Options,

    /// the following fields are used for lsp server to hold additional information
    pub action: ActionType,
    pub params: Option<(Pos, Option<String>)>,
    pub edit_pos: Option<Pos>,
}

/// FileCompileInput holds the information to parse a program through the entry of file,
/// with processed dependencies required by the file according to its kagari.toml
#[salsa::tracked]
pub struct FileCompileInput {
    /// file represents the entry file path to parse
    #[return_ref]
    pub file: String,

    /// modpath is the path of the project, according to the position of kagari.toml
    #[return_ref]
    pub modpath: String,
    pub docs: MemDocsInput,
    pub config: Config,
    pub opt: HashOptimizationLevel,
}

#[salsa::tracked]
impl FileCompileInput {
    #[salsa::tracked]
    // get_file_content gets the file content from cache or reads from file with the self.file
    pub fn get_file_content(self, db: &dyn Db) -> Option<SourceProgram> {
        // let f = self.file(db);
        // eprintln!("get_file_content {}", f);
        let re = self
            .docs(db)
            .docs(db)
            .lock()
            .unwrap()
            .get_file_content(db, self.file(db));
        match re {
            None => {
                let f = self.file(db);
                log::error!("lsp error: get_file_content failed {}", f);
                None
            }
            _ => re,
        }
    }
    #[salsa::tracked]
    pub fn get_emit_params(self, db: &dyn Db) -> EmitParams {
        let file = self.file(db);
        if crate::utils::canonicalize(self.docs(db).file(db)).unwrap()
            != crate::utils::canonicalize(file).unwrap()
        {
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
    #[salsa::tracked]
    pub fn get_current_file_content(self, db: &dyn Db) -> Option<SourceProgram> {
        let f = self.file(db);
        debug!("memdocinput get_current_file_content {}", f);
        let re = self
            .docs(db)
            .lock()
            .unwrap()
            .get_file_content(db, self.file(db));
        re
    }
    #[salsa::tracked]
    pub fn get_file_content(self, db: &dyn Db, f: String) -> Option<SourceProgram> {
        debug!("memdocinput get_file_content {}", f);
        let re = self.docs(db).lock().unwrap().get_file_content(db, &f);
        re
    }

    /// finalize_parser_input prepares the building environments according to the kagari.toml,
    /// and generates the parser entry file.
    /// it applies the entry_file as the parser entry if override_with_kagari_entry is false,
    /// otherwise the entry field in kagari.toml will be applied.
    #[salsa::tracked]
    pub fn finalize_parser_input(
        self,
        db: &dyn Db,
        entry_file: String,
        override_with_kagari_entry: bool,
    ) -> Option<FileCompileInput> {
        let mut final_entry_file: String;
        let re_entry_file = crate::utils::canonicalize(entry_file);
        match re_entry_file {
            Err(e) => {
                log::debug!("lsp error: {}", e);
                return None;
            }
            Ok(f) => {
                final_entry_file = f.to_string_lossy().to_string();
            }
        }

        let re_kagari_path = search_config_file(final_entry_file.clone());
        if re_kagari_path.is_err() {
            log::debug!("lsp error: {}", re_kagari_path.err().unwrap());
            return None;
        }

        let kagari_path = re_kagari_path.unwrap();
        let re_config = prepare_build_envs(
            db,
            self.docs(db)
                .lock()
                .unwrap()
                .get_file_content(db, &kagari_path)
                .unwrap(),
        );

        match re_config {
            Err(info) => {
                log::debug!("lsp error: {}", info);
                None
            }
            Ok(config) => {
                // use entry path inside kagari.toml instead of the input file
                if override_with_kagari_entry {
                    final_entry_file = config.entry.clone();
                }
                let buf = crate::utils::canonicalize(PathBuf::from(kagari_path.clone())).unwrap();
                let parant = buf.parent().unwrap();
                Some(FileCompileInput::new(
                    db,
                    final_entry_file,
                    parant.to_str().unwrap().to_string(),
                    self,
                    config,
                    self.op(db).optimization,
                ))
            }
        }
    }
}

impl MemDocs {
    pub fn change(&self, db: &mut dyn Db, range: lsp_types::Range, uri: String, text: String) {
        let (doc, txt) = self.change_txt(db, range, &uri, text);
        log::trace!("{} change text to: {}", &uri.as_str().to_string(), txt);
        doc.set_text(db).to(txt);
    }

    pub fn change_txt(
        &self,
        db: &dyn Db,
        range: lsp_types::Range,
        uri: &String,
        text: String,
    ) -> (SourceProgram, String) {
        let doc = self.docs.get(uri).unwrap();
        let mut txt = doc.text(db).clone();
        let offrange = position_to_offset(&txt, range.start)..position_to_offset(&txt, range.end);
        txt.replace_range(offrange, &text);
        (*doc, txt)
    }
    pub fn insert(&mut self, db: &dyn Db, key: String, value: String, path: String) {
        self.docs.insert(key, SourceProgram::new(db, value, path));
    }
    pub fn get(&self, key: &str) -> Option<&SourceProgram> {
        self.docs.get(key)
    }
    pub fn get_file_content(&mut self, db: &dyn Db, key: &str) -> Option<SourceProgram> {
        // let sanitized = crate::utils::canonicalize(key).unwrap();
        // let key = sanitized.to_str().unwrap();
        debug!("memdoc get_file_content {}", key);
        let mem = self.get(key);
        if let Some(mem) = mem {
            debug!("memdoc get_file_content result \n{}", mem.text(db));
            return Some(*mem);
        }
        let re = read_to_string(key);
        if let Ok(re) = re {
            log::info!("read file from path {}", key);
            self.insert(db, key.to_string(), re, key.to_string());
            return self.get_file_content(db, key);
        }
        None
    }
    pub fn get_mut(&mut self, key: &str) -> Option<&mut SourceProgram> {
        self.docs.get_mut(key)
    }
    pub fn remove(&mut self, key: &str) -> Option<SourceProgram> {
        // let key =  crate::utils::canonicalize(key).unwrap();
        // let key = key.to_str().unwrap();
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
        let db = &mut Database::default();
        let mut mem_docs = MemDocs::default();
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
            "🌏哒哒\n".to_string(),
        );
        assert_eq!(mem_docs.get("test").unwrap().text(db), "🌏哒哒\nt");
        mem_docs.change(
            db,
            lsp_types::Range {
                start: Position {
                    line: 0,
                    character: 2,
                },
                end: Position {
                    line: 0,
                    character: 3,
                },
            },
            "test".to_string(),
            "123".to_string(),
        );
        assert_eq!(mem_docs.get("test").unwrap().text(db), "🌏123哒\nt");
        mem_docs.change(
            db,
            lsp_types::Range {
                start: Position {
                    line: 1,
                    character: 0,
                },
                end: Position {
                    line: 1,
                    character: 1,
                },
            },
            "test".to_string(),
            "".to_string(),
        );
        assert_eq!(mem_docs.get("test").unwrap().text(db), "🌏123哒\n");

        mem_docs.change(
            db,
            lsp_types::Range {
                start: Position {
                    line: 1,
                    character: 0,
                },
                end: Position {
                    line: 1,
                    character: 0,
                },
            },
            "test".to_string(),
            "啊est".to_string(),
        );
        assert_eq!(mem_docs.get("test").unwrap().text(db), "🌏123哒\n啊est");
        mem_docs.change(
            db,
            lsp_types::Range {
                start: Position {
                    line: 1,
                    character: 3,
                },
                end: Position {
                    line: 1,
                    character: 4,
                },
            },
            "test".to_string(),
            "".to_string(),
        );
        assert_eq!(mem_docs.get("test").unwrap().text(db), "🌏123哒\n啊es");
    }
}
