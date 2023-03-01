use super::accumulators::PLReferences;
use super::diag::{ErrorCode, PLDiag};

use super::pltype::FNType;
use super::pltype::PLType;
use super::pltype::PriType;
use super::pltype::STType;

use super::range::Range;
use super::tokens::TokenType;

use crate::lsp::semantic_tokens::SemanticTokensBuilder;
use crate::Db;

use lsp_types::Command;
use lsp_types::CompletionItem;
use lsp_types::CompletionItemKind;

use lsp_types::DocumentSymbol;
use lsp_types::Hover;

use lsp_types::InlayHint;

use lsp_types::InsertTextFormat;
use lsp_types::Location;

use lsp_types::SignatureHelp;

use rustc_hash::{FxHashMap, FxHashSet};

use std::cell::RefCell;
use std::collections::BTreeMap;

use std::path::PathBuf;

use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalVar {
    pub tp: Arc<RefCell<PLType>>,
    pub range: Range,
    // pub loc: Arc<RwVec<Location>>,
}

/// # Mod
/// Represent a module
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Mod {
    /// mod name
    pub name: String,
    /// file path of the module
    pub path: String,
    /// func and types
    pub types: FxHashMap<String, Arc<RefCell<PLType>>>,
    /// sub mods
    pub submods: FxHashMap<String, Mod>,
    /// global variable table
    pub global_table: FxHashMap<String, GlobalVar>,
    /// structs methods
    pub methods: FxHashMap<String, FxHashMap<String, FNType>>,
    pub defs: LSPRangeMap<Range, LSPDef>,
    // pub refcache:LSPRangeMap<String, Arc<RwVec<Location>>>,
    pub local_refs: LSPRangeMap<Range, Arc<MutVec<Location>>>, // hold local vars
    pub glob_refs: LSPRangeMap<Range, String>,                 // hold global refs
    pub refs_map: LSPRangeMap<String, Arc<MutVec<Location>>>,
    pub sig_helps: LSPRangeMap<Range, SignatureHelp>,
    pub hovers: LSPRangeMap<Range, Hover>,
    pub completions: Arc<RefCell<Vec<CompletionItemWrapper>>>,
    pub completion_gened: Arc<RefCell<Gened>>,
    pub semantic_tokens_builder: Arc<RefCell<Box<SemanticTokensBuilder>>>, // semantic token builder
    pub hints: Arc<RefCell<Box<Vec<InlayHint>>>>,
    pub doc_symbols: Arc<RefCell<Box<Vec<DocumentSymbol>>>>,
    pub impls: FxHashMap<String, FxHashSet<String>>,
}

pub type MutVec<T> = RefCell<Vec<T>>;

#[derive(Debug, Clone, PartialEq)]
pub struct CompletionItemWrapper(pub CompletionItem);

impl Eq for CompletionItemWrapper {}

impl CompletionItemWrapper {
    pub fn into_completions(&self) -> CompletionItem {
        self.0.clone()
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Gened(bool);

impl Gened {
    pub fn set_true(&mut self) {
        self.0 = true;
    }
    pub fn is_true(&self) -> bool {
        self.0
    }
}

type LSPRangeMap<T, V> = Arc<RefCell<BTreeMap<T, V>>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LSPDef {
    Scalar(Location),
    Array,
}

impl Mod {
    pub fn new(name: String, path: String) -> Self {
        Self {
            name,
            path,
            types: FxHashMap::default(),
            submods: FxHashMap::default(),
            global_table: FxHashMap::default(),
            methods: FxHashMap::default(),
            defs: Arc::new(RefCell::new(BTreeMap::new())),
            // refs: Arc::new(RefCell::new(BTreeMap::new())),
            sig_helps: Arc::new(RefCell::new(BTreeMap::new())),
            hovers: Arc::new(RefCell::new(BTreeMap::new())),
            completions: Arc::new(RefCell::new(vec![])),
            completion_gened: Arc::new(RefCell::new(Gened(false))),
            semantic_tokens_builder: Arc::new(RefCell::new(Box::new(SemanticTokensBuilder::new(
                "builder".to_string(),
            )))),
            hints: Arc::new(RefCell::new(Box::default())),
            doc_symbols: Arc::new(RefCell::new(Box::default())),
            // refcache:Arc::new(RefCell::new(BTreeMap::new())),
            local_refs: Arc::new(RefCell::new(BTreeMap::new())),
            glob_refs: Arc::new(RefCell::new(BTreeMap::new())),
            refs_map: Arc::new(RefCell::new(BTreeMap::new())),
            impls: FxHashMap::default(),
        }
    }
    pub fn new_child(&self) -> Self {
        Mod {
            name: self.name.clone(),
            path: self.path.clone(),
            types: FxHashMap::default(),
            submods: self.submods.clone(),
            global_table: FxHashMap::default(),
            methods: self.methods.clone(),
            defs: self.defs.clone(),
            // refs: self.refs.clone(),
            sig_helps: self.sig_helps.clone(),
            hovers: self.hovers.clone(),
            completions: self.completions.clone(),
            completion_gened: self.completion_gened.clone(),
            semantic_tokens_builder: self.semantic_tokens_builder.clone(),
            hints: self.hints.clone(),
            doc_symbols: self.doc_symbols.clone(),
            // refcache:self.refcache.clone(),
            local_refs: self.local_refs.clone(),
            glob_refs: self.glob_refs.clone(),
            refs_map: self.refs_map.clone(),
            impls: self.impls.clone(),
        }
    }
    pub fn get_refs(&self, name: &str, db: &dyn Db, set: &mut FxHashSet<String>) {
        if set.contains(&self.path) {
            return;
        }
        set.insert(self.path.clone());
        self.push_refs(name, db);
        for m in self.submods.values() {
            m.get_refs(name, db, set);
        }
    }
    pub fn push_refs(&self, name: &str, db: &dyn Db) {
        if let Some(res) = self.refs_map.borrow().get(name) {
            PLReferences::push(db, res.borrow().clone());
        }
    }
    pub fn get_global_symbol(&self, name: &str) -> Option<&GlobalVar> {
        self.global_table.get(name)
    }
    pub fn add_global_symbol(
        &mut self,
        name: String,
        tp: Arc<RefCell<PLType>>,
        range: Range,
        // refs: Arc<RwVec<Location>>,
    ) -> Result<(), PLDiag> {
        if self.global_table.contains_key(&name) {
            return Err(range.new_err(ErrorCode::UNDEFINED_TYPE));
        }
        self.global_table.insert(
            name,
            GlobalVar {
                tp,
                range,
                // loc: refs,
            },
        );
        Ok(())
    }
    pub fn get_type(&self, name: &str) -> Option<Arc<RefCell<PLType>>> {
        let v = self.types.get(name);
        if let Some(pv) = v {
            return Some(pv.clone());
        }
        if let Some(x) = PriType::try_from_str(name) {
            return Some(Arc::new(RefCell::new(PLType::PRIMITIVE(x))));
        }
        if name == "void" {
            return Some(Arc::new(RefCell::new(PLType::VOID)));
        }
        None
    }

    /// 获取llvm名称
    ///
    /// module路径+类型名
    pub fn get_full_name(&self, name: &str) -> String {
        if name == "main" {
            return name.to_string();
        }
        format!("{}..{}", self.path, name)
    }
    pub fn get_short_name(&self, name: &str) -> String {
        if name == "main" {
            return name.to_string();
        }
        let re = name.split_once("..");
        if let Some((_, v)) = re {
            return v.to_string();
        }
        name.to_string()
    }
    pub fn get_methods_completions(&self, full_name: &str, pub_only: bool) -> Vec<CompletionItem> {
        let mut completions = Vec::new();
        let mut f = |name: &String, v: &FNType| {
            if pub_only && !v.is_modified_by(TokenType::PUB) {
                return;
            }
            completions.push(CompletionItem {
                kind: Some(CompletionItemKind::METHOD),
                label: name.clone(),
                detail: Some("method".to_string()),
                insert_text: Some(v.gen_snippet()),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                command: Some(Command::new(
                    "trigger help".to_string(),
                    "editor.action.triggerParameterHints".to_string(),
                    None,
                )),
                ..Default::default()
            });
        };
        for (_, m) in &self.submods {
            if m.methods.get(full_name).is_none() {
                continue;
            }
            for (name, v) in m.methods.get(full_name).unwrap() {
                f(name, v);
            }
        }
        if self.methods.get(full_name).is_none() {
            return completions;
        }
        for (name, v) in self.methods.get(full_name).unwrap() {
            f(name, v);
        }
        completions
    }

    pub fn find_method(&self, full_name: &str, mthd: &str) -> Option<FNType> {
        if let Some(m) = self.methods.get(full_name) {
            if let Some(v) = m.get(mthd) {
                return Some(v.clone());
            }
        }
        for (_, m) in &self.submods {
            if let Some(v) = m.find_method(full_name, mthd) {
                return Some(v);
            }
        }
        None
    }

    pub fn add_method(&mut self, tp: &STType, mthd: &str, fntp: FNType) -> Result<(), ()> {
        let full_name = tp.get_st_full_name();
        if let Some(m) = self.methods.get_mut(&full_name) {
            if m.get(mthd).is_some() {
                // duplicate method
                return Err(());
            }
            m.insert(mthd.to_string(), fntp);
        } else {
            let mut m = FxHashMap::default();
            m.insert(mthd.to_string(), fntp);
            self.methods.insert(full_name, m);
        }
        Ok(())
    }

    pub fn get_ns_completions_pri(&self, vmap: &mut FxHashMap<String, CompletionItem>) {
        for (k, _) in self.submods.iter() {
            vmap.insert(
                k.to_string(),
                CompletionItem {
                    label: k.to_string(),
                    kind: Some(CompletionItemKind::MODULE),
                    ..Default::default()
                },
            );
        }
    }
    pub fn get_ns_completions(&self) -> Vec<CompletionItem> {
        let mut m = FxHashMap::default();
        self.get_ns_completions_pri(&mut m);
        let cm = m.values().cloned().collect();
        cm
    }

    pub fn add_impl(&mut self, stname: &str, trait_tp_name: &str) {
        let full_name = format!("{}..{}", self.path, stname);
        if let Some(m) = self.impls.get_mut(&full_name) {
            m.insert(format!("{}..{}", self.path, trait_tp_name));
        } else {
            let mut m = FxHashSet::default();
            m.insert(format!("{}..{}", self.path, trait_tp_name));
            self.impls.insert(full_name, m);
        }
    }
}

fn get_ns_path_completions_pri(path: &str, vmap: &mut FxHashMap<String, CompletionItem>) {
    let dirs = PathBuf::from(path).read_dir();
    if dirs.is_err() {
        return;
    }
    for k in PathBuf::from(path).read_dir().unwrap() {
        if let Ok(d) = k {
            let buf = d.path();
            if !buf.is_dir() && buf.extension().is_some() && buf.extension().unwrap() != "pi" {
                continue;
            }
            let buf = PathBuf::from(d.path().file_stem().unwrap().to_str().unwrap());
            let x = buf.file_name().unwrap().to_str().unwrap();
            vmap.insert(
                x.to_string(),
                CompletionItem {
                    label: x.to_string(),
                    kind: Some(CompletionItemKind::MODULE),
                    ..Default::default()
                },
            );
        }
    }
}

pub fn get_ns_path_completions(path: &str) -> Vec<CompletionItem> {
    let mut m = FxHashMap::default();
    get_ns_path_completions_pri(path, &mut m);
    let cm = m.values().cloned().collect();
    cm
}
