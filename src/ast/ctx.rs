use super::builder::BlockHandle;
use super::builder::ValueHandle;
use super::diag::ErrorCode;
use super::diag::PLDiag;

use super::node::NodeEnum;
use super::node::PLValue;
use super::plmod::CompletionItemWrapper;
use super::plmod::GlobalVar;
use super::plmod::LSPDef;
use super::plmod::Mod;
use super::plmod::MutVec;
use super::pltype::add_primitive_types;
use super::pltype::FNType;
use super::pltype::Field;
use super::pltype::PLType;

use super::pltype::STType;
use super::range::Pos;
use super::range::Range;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::lsp::semantic_tokens::type_index;

use crate::utils::read_config::Config;
use crate::Db;

use lsp_types::Command;
use lsp_types::CompletionItem;
use lsp_types::CompletionItemKind;

use lsp_types::Hover;
use lsp_types::HoverContents;
use lsp_types::InlayHint;
use lsp_types::InlayHintKind;
use lsp_types::InsertTextFormat;
use lsp_types::Location;
use lsp_types::MarkedString;
use lsp_types::ParameterInformation;
use lsp_types::ParameterLabel;
use lsp_types::SemanticTokenType;
use lsp_types::SignatureHelp;
use lsp_types::SignatureInformation;
use lsp_types::Url;
use rustc_hash::FxHashMap;
use rustc_hash::FxHashSet;
use std::cell::RefCell;

use std::path::Path;

use std::path::PathBuf;
use std::sync::Arc;
/// # Ctx
/// Context for code generation
pub struct Ctx<'a> {
    pub generic_types: FxHashMap<String, Arc<RefCell<PLType>>>,
    pub need_highlight: usize,
    pub plmod: Mod,
    pub father: Option<&'a Ctx<'a>>, // father context, for symbol lookup
    pub function: Option<ValueHandle>, // current function
    pub init_func: Option<ValueHandle>, //init function,call first in main
    pub roots: RefCell<Vec<ValueHandle>>,
    pub block: Option<BlockHandle>,          // current block
    pub continue_block: Option<BlockHandle>, // the block to jump when continue
    pub break_block: Option<BlockHandle>,    // the block to jump to when break
    pub return_block: Option<(BlockHandle, Option<ValueHandle>)>, // the block to jump to when return and value
    pub errs: &'a RefCell<FxHashSet<PLDiag>>,                     // diagnostic list
    pub edit_pos: Option<Pos>,                                    // lsp params
    pub table: FxHashMap<
        String,
        (
            ValueHandle,
            Arc<RefCell<PLType>>,
            Range,
            Arc<MutVec<Location>>,
        ),
    >, // variable table
    pub config: Config,                                           // config
    pub usegc: bool,
    pub db: &'a dyn Db,
    pub rettp: Option<Arc<RefCell<PLType>>>,
}

impl<'a, 'ctx> Ctx<'a> {
    pub fn new(
        src_file_path: &'a str,
        errs: &'a RefCell<FxHashSet<PLDiag>>,
        edit_pos: Option<Pos>,
        config: Config,
        db: &'a dyn Db,
    ) -> Ctx<'a> {
        let f = Path::new(Path::new(src_file_path).file_stem().unwrap())
            .file_name()
            .take()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();
        let mut ctx = Ctx {
            need_highlight: 0,
            generic_types: FxHashMap::default(),
            plmod: Mod::new(f, src_file_path.to_string()),
            father: None,
            init_func: None,
            function: None,
            errs,
            edit_pos,
            table: FxHashMap::default(),
            config,
            usegc: true,
            db,
            block: None,
            continue_block: None,
            break_block: None,
            return_block: None,
            roots: RefCell::new(Vec::new()),
            rettp: None,
        };
        add_primitive_types(&mut ctx);
        ctx
    }
    pub fn new_child(&'a self, start: Pos, builder: &'a BuilderEnum<'a, 'ctx>) -> Ctx<'a> {
        let mut ctx = Ctx {
            need_highlight: self.need_highlight,
            generic_types: FxHashMap::default(),
            plmod: self.plmod.new_child(),
            father: Some(self),
            errs: self.errs,
            edit_pos: self.edit_pos.clone(),
            table: FxHashMap::default(),
            config: self.config.clone(),
            usegc: self.usegc,
            db: self.db.clone(),
            block: self.block,
            continue_block: self.continue_block,
            break_block: self.break_block,
            return_block: self.return_block,
            roots: RefCell::new(Vec::new()),
            rettp: self.rettp.clone(),
            init_func: self.init_func.clone(),
            function: self.function.clone(),
        };
        add_primitive_types(&mut ctx);
        builder.new_subscope(start);
        ctx
    }
    pub fn set_init_fn<'b>(&'b mut self, builder: &'b BuilderEnum<'a, 'ctx>) {
        self.function = Some(builder.add_function(
            &self.plmod.get_full_name("__init_global"),
            &vec![],
            PLType::VOID,
            self,
        ));
        self.init_func = self.function;
        builder.append_basic_block(self.init_func.unwrap(), "alloc");
        let entry = builder.append_basic_block(self.init_func.unwrap(), "entry");
        self.position_at_end(entry, builder);
    }
    pub fn clear_init_fn<'b>(&'b self, builder: &'b BuilderEnum<'a, 'ctx>) {
        let alloc = builder.get_first_basic_block(self.init_func.unwrap());
        let entry = builder.get_last_basic_block(self.init_func.unwrap());
        builder.delete_block(alloc);
        builder.delete_block(entry);
        builder.append_basic_block(self.init_func.unwrap(), "alloc");
        builder.append_basic_block(self.init_func.unwrap(), "entry");
    }
    pub fn init_fn_ret<'b>(&'b mut self, builder: &'b BuilderEnum<'a, 'ctx>) {
        let alloc = builder.get_first_basic_block(self.init_func.unwrap());
        let entry = builder.get_last_basic_block(self.init_func.unwrap());
        self.position_at_end(alloc, builder);
        builder.rm_curr_debug_location();
        builder.build_unconditional_branch(entry);
        self.position_at_end(entry, builder);
        builder.build_return(None);
    }
    pub fn add_method(&mut self, tp: &STType, mthd: &str, fntp: FNType, range: Range) {
        if self.plmod.add_method(tp, mthd, fntp).is_err() {
            self.add_diag(range.new_err(ErrorCode::DUPLICATE_METHOD));
        }
    }
    /// # get_symbol
    /// search in current and all father symbol tables
    pub fn get_symbol<'b>(
        &'b self,
        name: &str,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Option<(
        ValueHandle,
        Arc<RefCell<PLType>>,
        Range,
        Option<Arc<MutVec<Location>>>,
        bool,
    )> {
        let v = self.table.get(name);
        if let Some((h, pltype, range, refs)) = v {
            return Some((*h, pltype.clone(), range.clone(), Some(refs.clone()), false));
        }
        if let Some(father) = self.father {
            return father.get_symbol(name, builder);
        }
        if let Some(GlobalVar { tp: pltype, range }) = self.plmod.get_global_symbol(name) {
            return Some((
                builder
                    .get_global_var_handle(&self.plmod.get_full_name(name))
                    .unwrap(),
                pltype.clone(),
                range.clone(),
                None,
                true,
            ));
        }
        None
    }

    pub fn add_symbol(
        &mut self,
        name: String,
        pv: ValueHandle,
        pltype: Arc<RefCell<PLType>>,
        range: Range,
        is_const: bool,
    ) -> Result<(), PLDiag> {
        if self.table.contains_key(&name) {
            return Err(self.add_diag(range.new_err(ErrorCode::REDECLARATION)));
        }
        if is_const {
            self.set_glob_refs(&self.plmod.get_full_name(&name), range);
            self.plmod.add_global_symbol(name, pltype.clone(), range)?;
        } else {
            let refs = Arc::new(RefCell::new(vec![]));
            self.table
                .insert(name, (pv, pltype.clone(), range, refs.clone()));
            self.set_if_refs(refs, range);
        }
        self.send_if_go_to_def(range, range, self.plmod.path.clone());
        Ok(())
    }

    pub fn get_type(&self, name: &str, range: Range) -> Result<Arc<RefCell<PLType>>, PLDiag> {
        if let Some(pv) = self.generic_types.get(name) {
            return Ok(pv.clone());
        }
        if let Some(pv) = self.plmod.types.get(name) {
            return Ok(pv.clone());
        }
        if let Some(father) = self.father {
            let re = father.get_type(name, range);
            return re;
        }
        Err(range.new_err(ErrorCode::UNDEFINED_TYPE))
    }
    /// 用来获取外部模块的全局变量
    /// 如果没在当前module的全局变量表中找到，将会生成一个
    /// 该全局变量的声明
    pub fn get_or_add_global<'b>(
        &'b mut self,
        name: &str,
        pltype: Arc<RefCell<PLType>>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> ValueHandle {
        builder.get_or_add_global(name, pltype, self)
    }
    pub fn init_global<'b>(&'b mut self, builder: &'b BuilderEnum<'a, 'ctx>) {
        let mut set: FxHashSet<String> = FxHashSet::default();
        for (_, sub) in &self.plmod.clone().submods {
            self.init_global_walk(&sub, &mut set, builder);
        }

        builder.rm_curr_debug_location();
        builder.build_call(
            builder
                .get_function(&self.plmod.get_full_name("__init_global"))
                .unwrap(),
            &[],
        );
    }
    fn init_global_walk<'b>(
        &'b mut self,
        m: &Mod,
        set: &mut FxHashSet<String>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) {
        let name = m.get_full_name("__init_global");
        if set.contains(&name) {
            return;
        }
        for (_, sub) in &m.submods {
            self.init_global_walk(sub, set, builder);
        }
        let f = builder.add_function(&name, &[], PLType::VOID, self);
        builder.rm_curr_debug_location();
        builder.build_call(f, &[]);
        set.insert(name);
    }

    pub fn add_type(
        &mut self,
        name: String,
        pltype: Arc<RefCell<PLType>>,
        range: Range,
    ) -> Result<(), PLDiag> {
        if let PLType::GENERIC(g) = &*pltype.borrow() {
            if g.curpltype.is_some() {
                let cur = g.curpltype.as_ref().unwrap();
                return self.add_type(
                    cur.borrow().get_name(),
                    cur.clone(),
                    cur.borrow().get_range().unwrap(),
                );
            }
            unreachable!()
        }
        if self.plmod.types.contains_key(&name) {
            return Err(self.add_diag(range.new_err(ErrorCode::REDEFINE_TYPE)));
        }
        self.send_if_go_to_def(range, range, self.plmod.path.clone());
        self.plmod.types.insert(name, pltype.clone());
        Ok(())
    }
    pub fn add_type_without_check(&mut self, pltype: Arc<RefCell<PLType>>) {
        if let PLType::GENERIC(g) = &*pltype.borrow() {
            if g.curpltype.is_some() {
                return self.add_type_without_check(g.curpltype.as_ref().unwrap().clone());
            }
            return;
        }
        let name = pltype.borrow().get_name();
        if self.plmod.types.contains_key(&name) {
            return;
        }
        self.plmod.types.insert(name, pltype.clone());
    }
    pub fn add_generic_type(&mut self, name: String, pltype: Arc<RefCell<PLType>>, range: Range) {
        self.send_if_go_to_def(range, range, self.plmod.path.clone());
        self.generic_types.insert(name, pltype.clone());
    }
    pub fn move_generic_types(&mut self) -> FxHashMap<String, Arc<RefCell<PLType>>> {
        self.generic_types.clone()
    }
    pub fn reset_generic_types(&mut self, mp: FxHashMap<String, Arc<RefCell<PLType>>>) {
        self.generic_types = mp
    }
    pub fn add_doc_symbols(&mut self, pltype: Arc<RefCell<PLType>>) {
        match &*RefCell::borrow(&pltype) {
            PLType::FN(f) => {
                if !f.method {
                    self.plmod.doc_symbols.borrow_mut().push(f.get_doc_symbol())
                }
            }
            PLType::STRUCT(st) => self
                .plmod
                .doc_symbols
                .borrow_mut()
                .push(st.get_doc_symbol()),
            _ => {}
        }
    }

    pub fn add_diag(&self, dia: PLDiag) -> PLDiag {
        let dia2 = dia.clone();
        self.errs.borrow_mut().insert(dia);
        dia2
    }
    // load type* to type
    pub fn try_load2var<'b>(
        &'b mut self,
        range: Range,
        v: PLValue,
        tp: Arc<RefCell<PLType>>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<(ValueHandle, Arc<RefCell<PLType>>), PLDiag> {
        let v = v.value;
        builder.try_load2var(range, v, tp, self)
    }
    pub fn if_completion(
        &self,
        range: Range,
        get_completions: impl FnOnce() -> Vec<CompletionItem>,
    ) {
        if let Some(pos) = self.edit_pos {
            if pos.is_in(range) && !self.plmod.completion_gened.borrow().is_true() {
                let comps = get_completions();
                let comps = comps.iter().map(|x| CompletionItemWrapper(x.clone()));
                self.plmod.completions.borrow_mut().truncate(0);
                self.plmod.completions.borrow_mut().extend(comps);
                self.plmod.completion_gened.borrow_mut().set_true();
            }
        }
    }

    fn set_mod(&mut self, plmod: Mod) -> Mod {
        let m = self.plmod.clone();
        self.plmod = plmod;
        m
    }
    pub fn run_in_st_mod_mut<'b, T, F: FnMut(&mut Ctx<'a>, &mut STType) -> Result<T, PLDiag>>(
        &'b mut self,
        st: &mut STType,
        mut f: F,
    ) -> Result<T, PLDiag> {
        let p = PathBuf::from(&st.path);
        let mut oldm = None;
        if st.path != self.plmod.path {
            let s = p.file_name().unwrap().to_str().unwrap();
            let m = s.split(".").next().unwrap();
            let m = self.plmod.submods.get(m).unwrap();
            oldm = Some(self.set_mod(m.clone()));
        }
        let res = f(self, st);
        if let Some(m) = oldm {
            self.set_mod(m);
        }
        res
    }
    pub fn run_in_st_mod<'b, T, F: FnMut(&mut Ctx<'a>, &STType) -> Result<T, PLDiag>>(
        &'b mut self,
        st: &STType,
        mut f: F,
    ) -> Result<T, PLDiag> {
        let p = PathBuf::from(&st.path);
        let mut oldm = None;
        if st.path != self.plmod.path {
            let s = p.file_name().unwrap().to_str().unwrap();
            let m = s.split(".").next().unwrap();
            let m = self.plmod.submods.get(m).unwrap();
            oldm = Some(self.set_mod(m.clone()));
        }
        let res = f(self, st);
        if let Some(m) = oldm {
            self.set_mod(m);
        }
        res
    }

    pub fn run_in_fn_mod_mut<'b, T, F: FnMut(&mut Ctx<'a>, &mut FNType) -> Result<T, PLDiag>>(
        &'b mut self,
        fntype: &mut FNType,
        mut f: F,
    ) -> Result<T, PLDiag> {
        let p = PathBuf::from(&fntype.path);
        let mut oldm = None;
        if fntype.path != self.plmod.path {
            let s = p.file_name().unwrap().to_str().unwrap();
            let m = s.split(".").next().unwrap();
            let m = self.plmod.submods.get(m).unwrap();
            oldm = Some(self.set_mod(m.clone()));
        }
        let res = f(self, fntype);
        if let Some(m) = oldm {
            self.set_mod(m);
        }
        res
    }

    pub fn run_in_fn_mod<'b, T, F: FnMut(&mut Ctx<'a>, &FNType) -> Result<T, PLDiag>>(
        &'b mut self,
        fntype: &FNType,
        mut f: F,
    ) -> Result<T, PLDiag> {
        let p = PathBuf::from(&fntype.path);
        let mut oldm = None;
        if fntype.path != self.plmod.path {
            let s = p.file_name().unwrap().to_str().unwrap();
            let m = s.split(".").next().unwrap();
            let m = self.plmod.submods.get(m).unwrap();
            oldm = Some(self.set_mod(m.clone()));
        }
        let res = f(self, fntype);
        if let Some(m) = oldm {
            self.set_mod(m);
        }
        res
    }

    pub fn get_file_url(&self) -> Url {
        Url::from_file_path(self.plmod.path.clone()).unwrap()
    }

    pub fn get_location(&self, range: Range) -> Location {
        Location::new(self.get_file_url(), range.to_diag_range())
    }

    pub fn set_if_refs_tp(&self, tp: Arc<RefCell<PLType>>, range: Range) {
        tp.borrow().if_refs(|tp| {
            let name = tp.get_full_elm_name();
            self.set_glob_refs(&name, range)
        })
    }

    pub fn set_field_refs(&self, pltype: Arc<RefCell<PLType>>, f: &Field, range: Range) {
        self.set_glob_refs(
            &format!("{}..{}", &pltype.borrow().get_full_elm_name(), f.name),
            range,
        );
    }

    pub fn set_glob_refs(&self, name: &str, range: Range) {
        self.plmod
            .glob_refs
            .borrow_mut()
            .insert(range, name.to_string());
        let mut rm = self.plmod.refs_map.borrow_mut();
        if let Some(refsmap) = rm.get(name) {
            refsmap.borrow_mut().push(self.get_location(range));
        } else {
            let v = RefCell::new(vec![]);
            v.borrow_mut().push(self.get_location(range));
            rm.insert(name.to_string(), Arc::new(v));
        }
    }

    pub fn set_if_sig(&self, range: Range, name: String, params: &[String], n: u32) {
        self.plmod.sig_helps.borrow_mut().insert(
            range,
            SignatureHelp {
                signatures: vec![SignatureInformation {
                    label: name,
                    documentation: None,
                    parameters: Some(
                        params
                            .iter()
                            .map(|s| ParameterInformation {
                                label: ParameterLabel::Simple(s.clone()),
                                documentation: None,
                            })
                            .collect(),
                    ),
                    active_parameter: Some(n),
                }],
                active_signature: None,
                active_parameter: None,
            },
        );
    }

    pub fn set_if_refs(&self, refs: Arc<MutVec<Location>>, range: Range) {
        refs.borrow_mut().push(self.get_location(range));
        self.plmod.local_refs.borrow_mut().insert(range, refs);
    }

    pub fn send_if_go_to_def(&self, range: Range, destrange: Range, file: String) {
        self.plmod.defs.borrow_mut().insert(
            range,
            LSPDef::Scalar(Location {
                uri: Url::from_file_path(file.clone()).unwrap(),
                range: destrange.to_diag_range(),
            }),
        );
    }

    pub fn get_completions(&self) -> Vec<CompletionItem> {
        let mut m = FxHashMap::default();
        self.get_pltp_completions(&mut m);
        self.plmod.get_ns_completions_pri(&mut m);
        self.get_var_completions(&mut m);
        self.get_keyword_completions(&mut m);

        let cm = m.iter().map(|(_, v)| v.clone()).collect();
        cm
    }

    pub fn get_completions_in_ns(&self, ns: &str) -> Vec<CompletionItem> {
        let mut m = FxHashMap::default();
        self.get_const_completions_in_ns(ns, &mut m);
        self.get_type_completions_in_ns(ns, &mut m);

        let cm = m.iter().map(|(_, v)| v.clone()).collect();
        cm
    }

    fn get_const_completions_in_ns(&self, ns: &str, m: &mut FxHashMap<String, CompletionItem>) {
        let ns = self.plmod.submods.get(ns);
        if let Some(ns) = ns {
            for (k, v) in ns.global_table.iter() {
                let mut item = CompletionItem {
                    label: k.to_string(),
                    kind: Some(CompletionItemKind::CONSTANT),
                    ..Default::default()
                };
                item.detail = Some(v.tp.borrow().get_name());
                m.insert(k.clone(), item);
            }
        }
    }

    fn get_type_completions_in_ns(&self, ns: &str, m: &mut FxHashMap<String, CompletionItem>) {
        let ns = self.plmod.submods.get(ns);
        if let Some(ns) = ns {
            for (k, v) in ns.types.iter() {
                let mut insert_text = None;
                let mut command = None;
                let tp = match &*v.clone().borrow() {
                    PLType::STRUCT(_) => CompletionItemKind::STRUCT,
                    PLType::FN(f) => {
                        insert_text = Some(f.gen_snippet());
                        command = Some(Command::new(
                            "trigger help".to_string(),
                            "editor.action.triggerParameterHints".to_string(),
                            None,
                        ));
                        CompletionItemKind::FUNCTION
                    }
                    _ => continue, // skip completion for primary types
                };
                let mut item = CompletionItem {
                    label: k.to_string(),
                    kind: Some(tp),
                    insert_text,
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    command,
                    ..Default::default()
                };
                item.detail = Some(k.to_string());
                m.insert(k.clone(), item);
            }
        }
    }

    pub fn get_type_completions(&self) -> Vec<CompletionItem> {
        let mut m = FxHashMap::default();
        self.get_tp_completions(&mut m);
        self.plmod.get_ns_completions_pri(&mut m);
        m.iter().map(|(_, v)| v.clone()).collect()
    }

    fn get_tp_completions(&self, m: &mut FxHashMap<String, CompletionItem>) {
        for (k, f) in self.plmod.types.iter() {
            let tp = match &*f.borrow() {
                PLType::FN(_) => continue,
                PLType::ARR(_) => continue,
                PLType::PLACEHOLDER(_) => CompletionItemKind::STRUCT,
                PLType::GENERIC(_) => CompletionItemKind::TYPE_PARAMETER,
                PLType::STRUCT(_) => CompletionItemKind::STRUCT,
                PLType::TRAIT(_) => CompletionItemKind::INTERFACE,
                PLType::PRIMITIVE(_) => CompletionItemKind::KEYWORD,
                PLType::VOID => CompletionItemKind::KEYWORD,
                PLType::POINTER(_) => todo!(),
            };
            m.insert(
                k.to_string(),
                CompletionItem {
                    label: k.to_string(),
                    kind: Some(tp),
                    ..Default::default()
                },
            );
        }
        if let Some(father) = self.father {
            father.get_tp_completions(m);
        }
    }

    fn get_var_completions(&self, vmap: &mut FxHashMap<String, CompletionItem>) {
        for (k, _) in self.table.iter() {
            vmap.insert(
                k.to_string(),
                CompletionItem {
                    label: k.to_string(),
                    kind: Some(CompletionItemKind::VARIABLE),
                    ..Default::default()
                },
            );
        }
        if let Some(father) = self.father {
            father.get_var_completions(vmap);
        }
    }

    fn get_pltp_completions(&self, vmap: &mut FxHashMap<String, CompletionItem>) {
        for (k, f) in self.plmod.types.iter() {
            let mut insert_text = None;
            let mut command = None;
            let tp = match &*f.clone().borrow() {
                PLType::FN(f) => {
                    insert_text = Some(f.gen_snippet());
                    command = Some(Command::new(
                        "trigger help".to_string(),
                        "editor.action.triggerParameterHints".to_string(),
                        None,
                    ));
                    CompletionItemKind::FUNCTION
                }
                PLType::STRUCT(_) => CompletionItemKind::STRUCT,
                PLType::TRAIT(_) => CompletionItemKind::INTERFACE,
                PLType::ARR(_) => CompletionItemKind::KEYWORD,
                PLType::PRIMITIVE(_) => CompletionItemKind::KEYWORD,
                PLType::GENERIC(_) => CompletionItemKind::STRUCT,
                PLType::VOID => CompletionItemKind::KEYWORD,
                PLType::POINTER(_) => todo!(),
                PLType::PLACEHOLDER(_) => CompletionItemKind::STRUCT,
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
        if let Some(father) = self.father {
            father.get_pltp_completions(vmap);
        }
    }

    pub fn push_semantic_token(&self, range: Range, tp: SemanticTokenType, modifiers: u32) {
        if self.need_highlight != 0 {
            return;
        }
        self.plmod.semantic_tokens_builder.borrow_mut().push(
            range.to_diag_range(),
            type_index(tp),
            modifiers,
        )
    }
    pub fn push_type_hints(&self, range: Range, pltype: Arc<RefCell<PLType>>) {
        if self.need_highlight != 0 {
            return;
        }
        let hint = InlayHint {
            position: range.to_diag_range().end,
            label: lsp_types::InlayHintLabel::String(
                ": ".to_string() + &pltype.borrow().get_name(),
            ),
            kind: Some(InlayHintKind::TYPE),
            text_edits: None,
            tooltip: None,
            padding_left: None,
            padding_right: None,
            data: None,
        };
        self.plmod.hints.borrow_mut().push(hint);
    }
    pub fn push_param_hint(&self, range: Range, name: String) {
        if self.need_highlight != 0 {
            return;
        }
        let hint = InlayHint {
            position: range.to_diag_range().start,
            label: lsp_types::InlayHintLabel::String(name + ": "),
            kind: Some(InlayHintKind::TYPE),
            text_edits: None,
            tooltip: None,
            padding_left: None,
            padding_right: None,
            data: None,
        };
        self.plmod.hints.borrow_mut().push(hint);
    }
    pub fn position_at_end<'b>(
        &'b mut self,
        block: BlockHandle,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) {
        self.block = Some(block);
        builder.position_at_end_block(block);
    }
    fn get_keyword_completions(&self, vmap: &mut FxHashMap<String, CompletionItem>) {
        let keywords = vec![
            "if", "else", "while", "for", "return", "struct", "let", "true", "false",
        ];
        let loopkeys = vec!["break", "continue"];
        let toplevel = vec!["fn", "struct", "const", "use", "impl", "trait"];
        if self.father.is_none() {
            for k in toplevel {
                vmap.insert(
                    k.to_string(),
                    CompletionItem {
                        label: k.to_string(),
                        kind: Some(CompletionItemKind::KEYWORD),
                        ..Default::default()
                    },
                );
            }
        } else {
            for k in keywords {
                vmap.insert(
                    k.to_string(),
                    CompletionItem {
                        label: k.to_string(),
                        kind: Some(CompletionItemKind::KEYWORD),
                        ..Default::default()
                    },
                );
            }
        }
        if self.break_block.is_some() && self.continue_block.is_some() {
            for k in loopkeys {
                vmap.insert(
                    k.to_string(),
                    CompletionItem {
                        label: k.to_string(),
                        kind: Some(CompletionItemKind::KEYWORD),
                        ..Default::default()
                    },
                );
            }
        }
    }

    pub fn save_if_comment_doc_hover(&self, range: Range, docs: Option<Vec<Box<NodeEnum>>>) {
        if self.need_highlight != 0 {
            return;
        }
        let mut content = vec![];
        let mut string = String::new();
        if let Some(docs) = docs {
            for doc in docs {
                if let NodeEnum::Comment(c) = *doc {
                    string.push_str(&c.comment);
                    string.push('\n');
                }
            }
        }
        content.push(MarkedString::String(string));
        self.save_if_hover(range, HoverContents::Array(content))
    }

    pub fn save_if_hover(&self, range: Range, value: HoverContents) {
        if self.need_highlight != 0 {
            return;
        }
        self.plmod.hovers.borrow_mut().insert(
            range,
            Hover {
                range: None,
                contents: value,
            },
        );
    }
    /// # auto_deref
    /// 自动解引用，有几层解几层
    pub fn auto_deref<'b>(
        &'b self,
        tp: Arc<RefCell<PLType>>,
        value: ValueHandle,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> (Arc<RefCell<PLType>>, ValueHandle) {
        let mut tp = tp;
        let mut value = value;
        loop {
            match &*RefCell::borrow(&tp.clone()) {
                PLType::POINTER(p) => {
                    tp = p.clone();
                    value = builder.build_load(value, "load");
                }
                _ => break,
            }
        }
        (tp, value)
    }

    /// # auto_deref_tp
    /// 自动解pltype引用，有几层解几层
    pub fn auto_deref_tp(&self, tp: Arc<RefCell<PLType>>) -> Arc<RefCell<PLType>> {
        let mut tp = tp;
        loop {
            match &*RefCell::borrow(&tp.clone()) {
                PLType::POINTER(p) => {
                    tp = p.clone();
                }
                _ => break,
            }
        }
        tp
    }
}
