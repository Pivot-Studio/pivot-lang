use super::accumulators::PLReferences;
use super::ctx::Ctx;
use super::diag::{ErrorCode, PLDiag};

use super::node::macro_nodes::MacroNode;
use super::pltype::{PLType, FNValue};
use super::pltype::PriType;

use super::range::Range;

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

use super::pltype::TraitMthdImpl;

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
    pub hints: Arc<RefCell<Vec<InlayHint>>>,
    pub doc_symbols: Arc<RefCell<Vec<DocumentSymbol>>>,
    pub impls: FxHashMap<String, FxHashSet<String>>,
    pub macros: FxHashMap<String, Arc<MacroNode>>,
    pub trait_mthd_table: TraitMthdImpl,
}

pub type MutVec<T> = RefCell<Vec<T>>;

#[derive(Debug, Clone, PartialEq)]
pub struct CompletionItemWrapper(pub CompletionItem);

impl Eq for CompletionItemWrapper {}
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
    pub fn get_macro_completions(&self, vmap: &mut FxHashMap<String, CompletionItem>) {
        for (k, _) in self.macros.iter() {
            vmap.insert(
                k.to_string(),
                CompletionItem {
                    label: k.to_string(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    insert_text: Some(format!("{}!()", k)),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                },
            );
        }
    }

    /// import all public symbols from another module
    ///
    /// used to implements `use xxx::*;`
    pub fn import_all_public_symbols_from(&mut self, other: &Self) {
        for (k, v) in other.types.iter() {
            match &*v.borrow() {
                PLType::Fn(_) | PLType::Struct(_) | PLType::Trait(_) | PLType::Union(_) => {
                    self.types.insert(k.to_string(), v.clone());
                }
                _ => (),
            }
        }
        for (k, v) in other.macros.iter() {
            self.macros.insert(k.to_string(), v.clone());
        }
    }

    pub fn new(name: String, path: String) -> Self {
        Self {
            name,
            path,
            types: FxHashMap::default(),
            submods: FxHashMap::default(),
            global_table: FxHashMap::default(),
            defs: Arc::new(RefCell::new(BTreeMap::new())),
            // refs: Arc::new(RefCell::new(BTreeMap::new())),
            sig_helps: Arc::new(RefCell::new(BTreeMap::new())),
            hovers: Arc::new(RefCell::new(BTreeMap::new())),
            completions: Arc::new(RefCell::new(vec![])),
            completion_gened: Arc::new(RefCell::new(Gened(false))),
            semantic_tokens_builder: Arc::new(RefCell::new(Box::new(SemanticTokensBuilder::new(
                "builder".to_string(),
            )))),
            hints: Arc::new(RefCell::new(vec![])),
            doc_symbols: Arc::new(RefCell::new(vec![])),
            // refcache:Arc::new(RefCell::new(BTreeMap::new())),
            local_refs: Arc::new(RefCell::new(BTreeMap::new())),
            glob_refs: Arc::new(RefCell::new(BTreeMap::new())),
            refs_map: Arc::new(RefCell::new(BTreeMap::new())),
            impls: FxHashMap::default(),
            macros: FxHashMap::default(),
            trait_mthd_table: Default::default(),
        }
    }
    pub fn new_child(&self) -> Self {
        Mod {
            name: self.name.clone(),
            path: self.path.clone(),
            types: FxHashMap::default(),
            submods: self.submods.clone(),
            global_table: FxHashMap::default(),
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
            impls: FxHashMap::default(),
            macros: FxHashMap::default(),
            trait_mthd_table: self.trait_mthd_table.clone(),
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
    pub fn add_to_global_mthd_table(
        &self,
        st_name: &str,
        mthd_name: &str,
        fntp: Arc<RefCell<FNValue>>,
    ) {
        let mut m = self.trait_mthd_table.borrow_mut();
        m.entry(st_name.to_string())
            .or_insert_with(Default::default)
            .insert(mthd_name.to_owned(), fntp);
    }

    pub fn find_global_method(&self, name: &str, mthd: &str) -> Option<Arc<RefCell<FNValue>>> {
        let mut m = self.trait_mthd_table.borrow_mut();
        let table = m.get_mut(name);
        if let Some(table) = table {
            if let Some(fntp) = table.get(mthd) {
                return Some(fntp.clone());
            }
        }
        None
    }

    pub fn get_global_mthd_completions(
        &self,
        name: &str,
        set: &mut FxHashMap<String, CompletionItem>,
    ) {
        let mut m = self.trait_mthd_table.borrow_mut();
        let table = m.get_mut(name);
        if let Some(table) = table {
            table.iter().for_each(|(k, v)| {
                set.insert(
                    k.clone(),
                    CompletionItem {
                        kind: Some(CompletionItemKind::METHOD),
                        label: k.clone(),
                        detail: Some("method".to_string()),
                        insert_text: Some(v.borrow().gen_snippet()),
                        insert_text_format: Some(InsertTextFormat::SNIPPET),
                        command: Some(Command::new(
                            "trigger help".to_string(),
                            "editor.action.triggerParameterHints".to_string(),
                            None,
                        )),
                        ..Default::default()
                    },
                );
            });
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

    pub fn add_macro(&mut self, node: &MacroNode) {
        self.macros
            .insert(node.id.name.clone(), Arc::new(node.clone()));
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
    pub fn get_type(&self, name: &str, range: Range, ctx: &Ctx) -> Option<Arc<RefCell<PLType>>> {
        let v = self.types.get(name);
        if let Some(pv) = v {
            ctx.set_if_refs_tp(pv.clone(), range);
            ctx.send_if_go_to_def(
                range,
                pv.borrow().get_range().unwrap_or(range),
                self.path.clone(),
            );
            return Some(pv.clone());
        }
        if let Some(x) = PriType::try_from_str(name) {
            return Some(Arc::new(RefCell::new(PLType::Primitive(x))));
        }
        if name == "void" {
            return Some(Arc::new(RefCell::new(PLType::Void)));
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
    pub fn get_pltp_completions_list(&self) -> Vec<CompletionItem> {
        let mut m = FxHashMap::<String, CompletionItem>::default();
        self.get_pltp_completions(&mut m, &|_| true, &FxHashMap::default(), false);
        m.into_values().collect()
    }
    pub fn get_pltp_completions(
        &self,
        vmap: &mut FxHashMap<String, CompletionItem>,
        filter: &impl Fn(&PLType) -> bool,
        generic_tps: &FxHashMap<String, Arc<RefCell<PLType>>>,
        need_snippet: bool,
    ) {
        for (k, f) in self.types.iter().chain(generic_tps.iter()) {
            let mut insert_text = None;
            let mut command = None;
            if !filter(&f.borrow()) {
                continue;
            }
            let tp = match &*f.clone().borrow() {
                PLType::Fn(f) => {
                    if need_snippet {
                        insert_text = Some(f.gen_snippet());
                        command = Some(Command::new(
                            "trigger help".to_string(),
                            "editor.action.triggerParameterHints".to_string(),
                            None,
                        ));
                    }
                    CompletionItemKind::FUNCTION
                }
                PLType::Struct(_) => CompletionItemKind::STRUCT,
                PLType::Trait(_) => CompletionItemKind::INTERFACE,
                PLType::Arr(_) => unreachable!(),
                PLType::Primitive(_) => CompletionItemKind::KEYWORD,
                PLType::Generic(_) => CompletionItemKind::TYPE_PARAMETER,
                PLType::Void => CompletionItemKind::KEYWORD,
                PLType::Pointer(_) => unreachable!(),
                PLType::PlaceHolder(_) => continue,
                PLType::Union(_) => CompletionItemKind::ENUM,
                PLType::Closure(_) => unreachable!(),
            };
            if k.starts_with('|') {
                // skip method
                continue;
            }
            vmap.insert(
                k.to_string(),
                CompletionItem {
                    label: k.to_string(),
                    kind: Some(tp),
                    insert_text,
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    command,
                    ..Default::default()
                },
            );
        }
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
        self.impls
            .entry(stname.to_string())
            .or_insert_with(Default::default)
            .insert(trait_tp_name.to_string());
    }
}

fn get_ns_path_completions_pri(path: &str, vmap: &mut FxHashMap<String, CompletionItem>) {
    let dirs = PathBuf::from(path).read_dir();
    if dirs.is_err() {
        return;
    }
    for d in PathBuf::from(path).read_dir().unwrap().flatten() {
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

pub fn get_ns_path_completions(path: &str) -> Vec<CompletionItem> {
    let mut m = FxHashMap::default();
    get_ns_path_completions_pri(path, &mut m);
    let cm = m.values().cloned().collect();
    cm
}
