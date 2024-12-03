use super::accumulators::PLReferences;
use super::ctx::{Ctx, GenericCache};
use super::diag::{ErrorCode, PLDiag};

use super::node::macro_nodes::MacroNode;
use super::pltype::PriType;
use super::pltype::{FNValue, PLType};

use super::range::Range;
use super::traits::CustomType;

use crate::lsp::semantic_tokens::SemanticTokensBuilder;
use crate::Db;

use indexmap::IndexMap;
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
use salsa::Accumulator;
use ustr::{ustr, Ustr};

use std::cell::{Ref, RefCell, RefMut};
use std::collections::BTreeMap;

use std::path::{Path, PathBuf};

use std::sync::atomic::AtomicI32;
use std::sync::Arc;

use super::pltype::TraitMthdImpl;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalVar {
    pub tp: Arc<RefCell<PLType>>,
    pub mangled_name: Ustr,
    pub range: Range,
    pub is_extern: bool, // pub loc: Arc<RwVec<Location>>,
}

unsafe impl Sync for GlobalVar {}
unsafe impl Send for GlobalVar {}

pub static G_COUNTER: AtomicI32 = AtomicI32::new(0);

pub type ImplMap = FxHashMap<Ustr, FxHashMap<Ustr, IndexMap<Ustr, Arc<RefCell<PLType>>>>>;

/// # Mod
/// Mod represents a module in pivot-lang, which is a single pi file as well.
/// It carries more information than NodeEnum, such as the functions/variables from another dependency.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Mod {
    /// mod name
    pub name: Ustr,
    /// file path of the module
    pub path: Ustr,
    /// func and types
    pub types: FxHashMap<Ustr, GlobalType>,
    /// sub modules used by the current module
    pub submods: FxHashMap<Ustr, Arc<Mod>>,
    /// global variable table which is visiable for the current module
    /// it is consisted by the variables defined in this module or exported variables outside
    pub global_table: FxHashMap<Ustr, GlobalVar>,
    pub defs: LSPRangeMap<Range, LSPDef>,
    pub local_refs: LSPRangeMap<Range, Arc<MutVec<Location>>>, // hold local vars
    pub glob_refs: LSPRangeMap<Range, Ustr>,                   // hold global refs
    pub refs_map: LSPRangeMap<Ustr, Arc<MutVec<Location>>>,
    pub sig_helps: LSPRangeMap<Range, SignatureHelp>,
    pub hovers: LSPRangeMap<Range, Hover>,
    pub completions: Arc<RefCell<Vec<CompletionItemWrapper>>>,

    /// whether the completation is generated already
    pub completion_gened: Arc<RefCell<Gened>>,
    pub semantic_tokens_builder: Arc<RefCell<Box<SemanticTokensBuilder>>>, // semantic token builder
    pub hints: Arc<RefCell<Vec<InlayHint>>>,
    pub doc_symbols: Arc<RefCell<Vec<DocumentSymbol>>>,
    pub impls: Arc<RefCell<ImplMap>>,
    pub macros: FxHashMap<Ustr, Arc<MacroNode>>,
    pub trait_mthd_table: TraitMthdImpl,
    pub generic_infer: GenericCache,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalType {
    pub is_extern: bool,
    pub re_export: bool,
    pub typ: Arc<RefCell<PLType>>,
}

impl GlobalType {
    pub fn borrow(&self) -> Ref<'_, PLType> {
        self.typ.borrow()
    }
    pub fn borrow_mut(&self) -> RefMut<'_, PLType> {
        self.typ.borrow_mut()
    }
    pub fn new(tp: PLType) -> Self {
        Self {
            is_extern: false,
            re_export: false,
            typ: Arc::new(RefCell::new(tp)),
        }
    }
    pub fn visibal_outside(&self) -> bool {
        (self.borrow().is_pub() && !self.is_extern) || (self.is_extern && self.re_export)
    }
    pub fn expect_pub(&self, ctx: &Ctx, range: Range) -> Result<(), PLDiag> {
        if self.is_extern && !self.re_export {
            return Err(range
                .new_err(ErrorCode::TRY_TO_EXPORT_NON_REEXPORT_SYMBOL)
                .add_to_ctx(ctx));
        }
        self.borrow().expect_pub(ctx, range)
    }
}

impl From<PLType> for GlobalType {
    fn from(tp: PLType) -> Self {
        Self::new(tp)
    }
}

impl From<Arc<RefCell<PLType>>> for GlobalType {
    fn from(tp: Arc<RefCell<PLType>>) -> Self {
        Self {
            is_extern: false,
            re_export: false,
            typ: tp,
        }
    }
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
    pub fn get_macro_completions(&self, vmap: &mut FxHashMap<Ustr, CompletionItem>) {
        for (k, _) in self.macros.iter() {
            vmap.insert(
                *k,
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

    /// import all symbols from another module
    ///
    /// used to implements builtin module
    pub fn import_all_symbols_from(&mut self, other: &Self) {
        for (k, v) in other.types.iter() {
            match &*v.borrow() {
                PLType::Fn(_) | PLType::Struct(_) | PLType::Trait(_) | PLType::Union(_) => {
                    self.types.insert(
                        *k,
                        GlobalType {
                            typ: v.typ.clone(),
                            is_extern: true,
                            re_export: false,
                        },
                    );
                }
                _ => (),
            }
        }
        for (k, v) in other.macros.iter() {
            self.macros.insert(*k, v.clone());
        }
    }

    pub fn new(path: Ustr, generic_infer: GenericCache) -> Self {
        let name = ustr(
            Path::new(Path::new(path.as_str()).file_stem().unwrap())
                .file_name()
                .take()
                .unwrap()
                .to_str()
                .unwrap(),
        );
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
            impls: Arc::new(RefCell::new(FxHashMap::default())),
            macros: FxHashMap::default(),
            trait_mthd_table: Default::default(),
            generic_infer,
        }
    }
    pub fn new_child(&self) -> Self {
        Mod {
            name: self.name,
            path: self.path,
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
            impls: self.impls.clone(),
            macros: FxHashMap::default(),
            trait_mthd_table: self.trait_mthd_table.clone(),
            generic_infer: self.generic_infer.clone(),
        }
    }
    pub fn get_refs(&self, name: &Ustr, db: &dyn Db, set: &mut FxHashSet<Ustr>) {
        if set.contains(&self.path) {
            return;
        }
        set.insert(self.path);
        self.push_refs(name, db);
        for m in self.submods.values() {
            m.get_refs(name, db, set);
        }
    }
    pub fn add_to_global_mthd_table(
        &self,
        st_name: &Ustr,
        mthd_name: &Ustr,
        fntp: Arc<RefCell<FNValue>>,
    ) {
        let mut m = self.trait_mthd_table.borrow_mut();
        m.entry(*st_name).or_default().insert(*mthd_name, fntp);
    }

    pub fn find_global_method(&self, name: &Ustr, mthd: &Ustr) -> Option<Arc<RefCell<FNValue>>> {
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
        name: &Ustr,
        set: &mut FxHashMap<Ustr, CompletionItem>,
    ) {
        let mut m = self.trait_mthd_table.borrow_mut();
        let table = m.get_mut(name);
        if let Some(table) = table {
            table.iter().for_each(|(k, v)| {
                set.insert(
                    *k,
                    CompletionItem {
                        kind: Some(CompletionItemKind::METHOD),
                        label: k.to_string(),
                        detail: Some("method".to_string()),
                        insert_text: Some(v.borrow().gen_snippet().to_string()),
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
    pub fn push_refs(&self, name: &Ustr, db: &dyn Db) {
        if let Some(res) = self.refs_map.borrow().get(name) {
            PLReferences(res.borrow().clone()).accumulate(db);
        }
    }
    pub fn get_global_symbol(&self, name: &Ustr) -> Option<&GlobalVar> {
        self.global_table.get(name)
    }

    pub fn add_macro(&mut self, node: &MacroNode) {
        self.macros.insert(node.id.name, Arc::new(node.clone()));
    }
    pub fn add_global_symbol(
        &mut self,
        name: Ustr,
        tp: Arc<RefCell<PLType>>,
        range: Range,
        is_extern: bool,
    ) -> Result<(), PLDiag> {
        if self.global_table.contains_key(&name) {
            return Err(range.new_err(ErrorCode::REDEFINE_SYMBOL));
        }
        self.global_table.insert(
            name,
            GlobalVar {
                tp,
                range,
                is_extern, // loc: refs,
                mangled_name: self.get_full_name(ustr(&format!(
                    "{}@{}",
                    name,
                    G_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
                ))),
            },
        );
        Ok(())
    }

    /// # get_type_inner
    ///
    /// it checks whether the name refers to a primary type or void,
    /// and then queries the types to try to find a custom type.
    ///
    /// None is returned if no sasified type matches the name.
    fn get_type_inner(&self, name: &Ustr, range: Range, ctx: &Ctx) -> Option<GlobalType> {
        if let Some(x) = PriType::try_from_str(name) {
            return Some(PLType::Primitive(x).into());
        }
        if *name == ustr("void") {
            return Some(PLType::Void.into());
        }
        let v = self.types.get(name);
        if let Some(pv) = v {
            if range != Default::default() {
                ctx.set_if_refs_tp(pv.typ.clone(), range);
                ctx.send_if_go_to_def(
                    range,
                    pv.borrow().get_range().unwrap_or(range),
                    pv.borrow().get_path().unwrap_or(self.path),
                );
            }
            return Some(pv.clone());
        }
        None
    }

    /// # get_type_walker
    ///
    /// it inspects the generic types, and triggers [get_type_inner] to find among primary types and types.
    fn get_type_walker(&self, name: &Ustr, range: Range, ctx: &Ctx) -> Result<GlobalType, PLDiag> {
        if let Some(pv) = ctx.generic_types.get(name) {
            ctx.set_if_refs_tp(pv.clone(), range);
            ctx.send_if_go_to_def(
                range,
                pv.borrow().get_range().unwrap_or(range),
                pv.borrow().get_path().unwrap_or(self.path),
            );
            return Ok(pv.clone().into());
        }
        if let Some(pv) = self.get_type_inner(name, range, ctx) {
            return Ok(pv);
        }
        Err(range.new_err(ErrorCode::UNDEFINED_TYPE))
    }

    /// # get_type
    ///
    /// It tries to find a type associated to the name from current context and all its ancestor contexts until a matched type is found,
    /// otherwise it returns an error.
    ///
    /// It supports both simple type and generic types during finding. The generic argument lists won't be respected during querying.
    pub fn get_type(&self, name: &Ustr, range: Range, ctx: &Ctx) -> Result<GlobalType, PLDiag> {
        if let Ok(re) = self.get_type_walker(name, range, ctx) {
            return Ok(re);
        }
        if name.contains('<') {
            // generic
            // name<i64> ctx --name-> name --name<i64>--> name<i64>
            let st_name = ustr(name.split('<').collect::<Vec<_>>()[0]);
            let st_with_generic = self.get_type_walker(&st_name, range, ctx)?;
            match &*st_with_generic.borrow() {
                PLType::Struct(st) | PLType::Trait(st) => {
                    if let Some(res) = ctx.get_infer_result(st, *name) {
                        if let PLType::Struct(s) = &*res.clone().borrow() {
                            if s.fields.len() == st.fields.len() {
                                return Ok(res.into());
                            }
                        }
                    }
                }
                PLType::Union(st) => {
                    if let Some(res) = ctx.get_infer_result(st, *name) {
                        return Ok(res.into());
                    }
                }
                _ => (),
            };
        }
        Err(range.new_err(ErrorCode::UNDEFINED_TYPE))
    }

    /// 获取llvm名称
    ///
    /// module路径+类型名
    pub fn get_full_name(&self, name: Ustr) -> Ustr {
        if name == "main" {
            return name;
        }
        format!("{}..{}", self.path, name).into()
    }
    pub fn get_short_name(&self, name: &Ustr) -> Ustr {
        if *name == "main" {
            return *name;
        }
        let re = name.split_once("..");
        if let Some((_, v)) = re {
            return v.into();
        }
        *name
    }
    pub fn get_pltp_completions_list(&self) -> Vec<CompletionItem> {
        let mut m = FxHashMap::<Ustr, CompletionItem>::default();
        self.get_pltp_completions(&mut m, &|_| true, &FxHashMap::default(), false);
        m.into_values().collect()
    }
    pub fn get_pltp_completions(
        &self,
        vmap: &mut FxHashMap<Ustr, CompletionItem>,
        filter: &impl Fn(&GlobalType) -> bool,
        generic_tps: &FxHashMap<Ustr, Arc<RefCell<PLType>>>,
        need_snippet: bool,
    ) {
        for (k, f) in self
            .types
            .iter()
            .map(|(k, v)| (k, v.clone()))
            .chain(generic_tps.iter().map(|(k, v)| (k, v.clone().into())))
        {
            let mut insert_text = None;
            let mut command = None;
            if !filter(&f) {
                continue;
            }
            let tp = &*f.borrow();
            let tp = match completion_kind(tp, need_snippet, &mut insert_text, &mut command) {
                Some(value) => value,
                None => continue,
            };
            if k.starts_with('|') {
                // skip method
                continue;
            }
            vmap.insert(
                *k,
                CompletionItem {
                    label: k.to_string(),
                    kind: Some(tp),
                    insert_text: insert_text.map(|x| x.to_string()),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    command,
                    ..Default::default()
                },
            );
        }
    }

    pub fn get_ns_completions_pri(&self, vmap: &mut FxHashMap<Ustr, CompletionItem>) {
        for (k, _) in self.submods.iter() {
            vmap.insert(
                *k,
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

    pub fn add_impl(
        &self,
        stname: &Ustr,
        trait_tp_name: &Ustr,
        generic_map: IndexMap<Ustr, Arc<RefCell<PLType>>>,
    ) {
        self.impls
            .borrow_mut()
            .entry(*stname)
            .or_default()
            .insert(*trait_tp_name, generic_map);
    }
    pub fn search_mod<TP: CustomType>(&self, u: &TP, m: &Ustr) -> Option<Arc<Self>> {
        self.submods
            .get(m)
            .and_then(|x| {
                if x.path != u.get_path() {
                    return None;
                }
                Some(x.clone())
            })
            .or_else(|| {
                for (_, v) in self.submods.iter() {
                    let re = v.search_mod(u, m);
                    if re.is_some() {
                        return re;
                    }
                }
                None
            })
    }
}

/// # completion_kind
///
/// get completion kind from pltype
fn completion_kind(
    tp: &PLType,
    need_snippet: bool,
    insert_text: &mut Option<Ustr>,
    command: &mut Option<Command>,
) -> Option<CompletionItemKind> {
    let tp = match tp {
        PLType::Fn(f) => {
            if need_snippet {
                *insert_text = Some(f.gen_snippet());
                *command = Some(Command::new(
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
        PLType::PlaceHolder(_) => return None,
        PLType::Union(_) => CompletionItemKind::ENUM,
        PLType::Closure(_) => unreachable!(),
        PLType::Unknown => unreachable!(),
        PLType::PartialInferred(p) => {
            return completion_kind(&p.borrow(), need_snippet, insert_text, command);
        }
    };
    Some(tp)
}

fn get_ns_path_completions_pri(path: &Ustr, vmap: &mut FxHashMap<Ustr, CompletionItem>) {
    let dirs = PathBuf::from(path.as_str()).read_dir();
    if dirs.is_err() {
        return;
    }
    for d in PathBuf::from(path.as_str()).read_dir().unwrap().flatten() {
        let buf = d.path();
        if !buf.is_dir() && buf.extension().is_some() && buf.extension().unwrap() != "pi" {
            continue;
        }
        let buf = PathBuf::from(d.path().file_stem().unwrap().to_str().unwrap());
        let x = buf.file_name().unwrap().to_str().unwrap();
        vmap.insert(
            x.into(),
            CompletionItem {
                label: x.to_string(),
                kind: Some(CompletionItemKind::MODULE),
                ..Default::default()
            },
        );
    }
}

pub fn get_ns_path_completions(path: &Ustr) -> Vec<CompletionItem> {
    let mut m = FxHashMap::default();
    get_ns_path_completions_pri(path, &mut m);
    let cm = m.values().cloned().collect();
    cm
}
