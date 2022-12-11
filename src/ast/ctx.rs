use crate::lsp::semantic_tokens::type_index;
use crate::lsp::semantic_tokens::SemanticTokensBuilder;
use crate::utils::read_config::Config;
use crate::Db;
use colored::Colorize;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::debug_info::*;
use inkwell::module::FlagBehavior;
use inkwell::module::Linkage;
use inkwell::module::Module;
use inkwell::targets::TargetMachine;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use inkwell::values::PointerValue;
use inkwell::values::{AnyValueEnum, BasicMetadataValueEnum};
use lsp_types::Command;
use lsp_types::CompletionItem;
use lsp_types::CompletionItemKind;
use lsp_types::Diagnostic;
use lsp_types::DiagnosticSeverity;
use lsp_types::DocumentSymbol;
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
use std::collections::BTreeMap;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;

use super::builder::BlockHandle;
use super::builder::LLVMBuilder;
use super::builder::ValueHandle;
use super::compiler::get_target_machine;
use super::diag::{ErrorCode, WarnCode};
use super::diag::{ERR_MSG, WARN_MSG};
use super::node::pkg::ExternIdNode;
use super::node::types::TypeNameNode;
use super::node::NodeEnum;
use super::node::PLValue;
use super::pltype::add_primitive_types;
use super::pltype::FNType;
use super::pltype::PLType;
use super::pltype::PriType;
use super::pltype::STType;
use super::range::Pos;
use super::range::Range;
/// # Ctx
/// Context for code generation
pub struct Ctx<'a, 'ctx> {
    pub generic_types: FxHashMap<String, Rc<RefCell<PLType>>>,
    pub need_highlight: bool,
    pub plmod: Mod,
    pub father: Option<&'a Ctx<'a, 'ctx>>, // father context, for symbol lookup
    pub llbuilder: RefCell<LLVMBuilder<'a, 'ctx>>,
    // pub context: &'ctx Context,            // llvm context
    // pub builder: &'a Builder<'ctx>,        // llvm builder
    // pub module: &'a Module<'ctx>,          // llvm module
    // pub dibuilder: &'a DebugInfoBuilder<'ctx>, // debug info builder
    // pub diunit: &'a DICompileUnit<'ctx>,   // debug info unit
    // pub function: Option<FunctionValue<'ctx>>, // current function
    // pub block: Option<BasicBlock<'ctx>>,   // current block
    // pub continue_block: Option<BasicBlock<'ctx>>, // the block to jump when continue
    // pub break_block: Option<BasicBlock<'ctx>>, // the block to jump to when break
    // pub return_block: Option<(BasicBlock<'ctx>, Option<PointerValue<'ctx>>)>, // the block to jump to when return and value
    // pub targetmachine: &'a TargetMachine, // might be used in debug info
    // pub discope: DIScope<'ctx>,           // debug info scope
    // pub nodebug_builder: &'a Builder<'ctx>, // builder without debug info
    pub roots: RefCell<Vec<ValueHandle>>,
    pub block: Option<BlockHandle>,          // current block
    pub continue_block: Option<BlockHandle>, // the block to jump when continue
    pub break_block: Option<BlockHandle>,    // the block to jump to when break
    pub return_block: Option<(BlockHandle, Option<ValueHandle>)>, // the block to jump to when return and value
    pub errs: &'a RefCell<Vec<PLDiag>>,                           // diagnostic list
    pub edit_pos: Option<Pos>,                                    // lsp params
    pub table: FxHashMap<
        String,
        (
            ValueHandle,
            Rc<RefCell<PLType>>,
            Range,
            Rc<RefCell<Vec<Location>>>,
        ),
    >, // variable table
    pub config: Config,                                           // config
    pub usegc: bool,
    pub db: &'a dyn Db,
    pub rettp: Option<Rc<RefCell<PLType>>>,
}

pub struct MemberType<'ctx> {
    pub ditype: DIDerivedType<'ctx>,
    pub offset: u64,
    pub scope: DIScope<'ctx>,
    pub line: u32,
    pub name: String,
    pub di_file: DIFile<'ctx>,
    pub ptr_depth: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalVar {
    pub tp: Rc<RefCell<PLType>>,
    pub range: Range,
    pub loc: Rc<RefCell<Vec<Location>>>,
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
    pub types: FxHashMap<String, Rc<RefCell<PLType>>>,
    /// sub mods
    pub submods: FxHashMap<String, Mod>,
    /// global variable table
    pub global_table: FxHashMap<String, GlobalVar>,
    /// structs methods
    pub methods: FxHashMap<String, FxHashMap<String, FNType>>,
    pub defs: LSPRangeMap<Range, LSPDef>,
    pub refs: LSPRangeMap<Range, Rc<RefCell<Vec<Location>>>>,
    pub sig_helps: LSPRangeMap<Range, SignatureHelp>,
    pub hovers: LSPRangeMap<Range, Hover>,
    pub completions: Rc<RefCell<Vec<CompletionItemWrapper>>>,
    pub completion_gened: Rc<RefCell<Gened>>,
    pub semantic_tokens_builder: Rc<RefCell<Box<SemanticTokensBuilder>>>, // semantic token builder
    pub hints: Rc<RefCell<Box<Vec<InlayHint>>>>,
    pub doc_symbols: Rc<RefCell<Box<Vec<DocumentSymbol>>>>,
    // pub hints: Rc<RefCell<Box<Vec<InlayHint>>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompletionItemWrapper(CompletionItem);

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

type LSPRangeMap<T, V> = Rc<RefCell<BTreeMap<T, V>>>;

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
            defs: Rc::new(RefCell::new(BTreeMap::new())),
            refs: Rc::new(RefCell::new(BTreeMap::new())),
            sig_helps: Rc::new(RefCell::new(BTreeMap::new())),
            hovers: Rc::new(RefCell::new(BTreeMap::new())),
            completions: Rc::new(RefCell::new(vec![])),
            completion_gened: Rc::new(RefCell::new(Gened(false))),
            semantic_tokens_builder: Rc::new(RefCell::new(Box::new(SemanticTokensBuilder::new(
                "builder".to_string(),
            )))),
            hints: Rc::new(RefCell::new(Box::new(vec![]))),
            doc_symbols: Rc::new(RefCell::new(Box::new(vec![]))),
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
            refs: self.refs.clone(),
            sig_helps: self.sig_helps.clone(),
            hovers: self.hovers.clone(),
            completions: self.completions.clone(),
            completion_gened: self.completion_gened.clone(),
            semantic_tokens_builder: self.semantic_tokens_builder.clone(),
            hints: self.hints.clone(),
            doc_symbols: self.doc_symbols.clone(),
        }
    }
    pub fn get_global_symbol(&self, name: &str) -> Option<&GlobalVar> {
        self.global_table.get(name)
    }
    pub fn add_global_symbol(
        &mut self,
        name: String,
        tp: Rc<RefCell<PLType>>,
        range: Range,
        refs: Rc<RefCell<Vec<Location>>>,
    ) -> Result<(), PLDiag> {
        if self.global_table.contains_key(&name) {
            return Err(PLDiag::new_error(range, ErrorCode::UNDEFINED_TYPE));
        }
        self.global_table.insert(
            name,
            GlobalVar {
                tp,
                range,
                loc: refs,
            },
        );
        Ok(())
    }
    pub fn get_type(&self, name: &str) -> Option<Rc<RefCell<PLType>>> {
        let v = self.types.get(name);
        if let Some(pv) = v {
            return Some(pv.clone());
        }
        if let Some(x) = PriType::try_from_str(name) {
            return Some(Rc::new(RefCell::new(PLType::PRIMITIVE(x))));
        }
        if name == "void" {
            return Some(Rc::new(RefCell::new(PLType::VOID)));
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
    pub fn get_methods_completions(&self, full_name: &str) -> Vec<CompletionItem> {
        let mut completions = Vec::new();
        let mut f = |name: &String, v: &FNType| {
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

    fn add_method(&mut self, tp: &STType, mthd: &str, fntp: FNType) -> Result<(), ()> {
        let full_name = tp.get_st_full_name();
        if let Some(m) = self.methods.get_mut(&full_name) {
            if let Some(_) = m.get(mthd) {
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

    fn get_ns_completions_pri(&self, vmap: &mut FxHashMap<String, CompletionItem>) {
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
        let cm = m.iter().map(|(_, v)| v.clone()).collect();
        cm
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
    let cm = m.iter().map(|(_, v)| v.clone()).collect();
    cm
}

/// # PLDiag
/// Diagnostic for pivot-lang
/// TODO: warning, info
#[derive(Debug, Clone)]
pub enum PLDiag {
    Error(Err),
    Warn(Warn),
}

/// # Err
/// Error for pivot-lang
#[derive(Debug, Clone)]
pub struct Err {
    pub diag: Diagnostic,
}
#[derive(Debug, Clone)]
pub struct Warn {
    pub diag: Diagnostic,
}

const PL_DIAG_SOURCE: &str = "plsp";

impl PLDiag {
    pub fn print(&self, path: &str) {
        match self {
            PLDiag::Error(s) => {
                let err = format!(
                    "{}\n\t{}",
                    format!(
                        "{}:{}:{}",
                        path,
                        s.diag.range.start.line + 1,
                        s.diag.range.start.character + 1
                    )
                    .red(),
                    format!("{}", s.diag.message.blue().bold()),
                );
                log::error!("{}", err);
            }
            PLDiag::Warn(s) => {
                let err = format!(
                    "{}\n\t{}",
                    format!(
                        "{}:{}:{}",
                        path,
                        s.diag.range.start.line + 1,
                        s.diag.range.start.character + 1
                    )
                    .yellow(),
                    format!("{}", s.diag.message.blue().bold()),
                );
                log::warn!("{}", err);
            }
        }
    }
    pub fn is_err(&self) -> bool {
        if let PLDiag::Error(_) = self {
            true
        } else {
            false
        }
    }
    pub fn get_diagnostic(&self) -> Diagnostic {
        match self {
            PLDiag::Error(e) => e.diag.clone(),
            PLDiag::Warn(w) => w.diag.clone(),
        }
    }
    pub fn new_error(range: Range, code: ErrorCode) -> Self {
        let diag = Diagnostic::new_with_code_number(
            range.to_diag_range(),
            DiagnosticSeverity::ERROR,
            code as i32,
            Some(PL_DIAG_SOURCE.to_string()),
            ERR_MSG[&code].to_string(),
        );
        PLDiag::Error(Err { diag })
    }
    pub fn new_warn(range: Range, code: WarnCode) -> Self {
        let diag = Diagnostic::new_with_code_number(
            range.to_diag_range(),
            DiagnosticSeverity::WARNING,
            code as i32,
            Some(PL_DIAG_SOURCE.to_string()),
            WARN_MSG[&code].to_string(),
        );
        PLDiag::Warn(Warn { diag })
    }
}

pub fn create_ctx_info<'ctx>(
    context: &'ctx Context,
    dir: &str,
    file: &str,
) -> (
    Module<'ctx>,
    Builder<'ctx>,
    DebugInfoBuilder<'ctx>,
    DICompileUnit<'ctx>,
    TargetMachine,
    Builder<'ctx>,
) {
    let builder = context.create_builder();
    let module = context.create_module("main");
    let (dibuilder, compile_unit) = module.create_debug_info_builder(
        true,
        DWARFSourceLanguage::C,
        file,
        dir,
        "plc frontend",
        false,
        "",
        0,
        "",
        DWARFEmissionKind::Full,
        0,
        false,
        true,
        "",
        "",
    );

    let metav = context.metadata_node(&[BasicMetadataValueEnum::IntValue(
        context.i32_type().const_int(3, false),
    )]);
    module.add_metadata_flag("Debug Info Version", FlagBehavior::Warning, metav);
    if cfg!(target_os = "windows") {
        let metacv = context.metadata_node(&[BasicMetadataValueEnum::IntValue(
            context.i32_type().const_int(1, false),
        )]);
        module.add_metadata_flag("CodeView", FlagBehavior::Warning, metacv); // TODO: is this needed for windows debug?
    }
    let tm = get_target_machine(inkwell::OptimizationLevel::None);
    (
        module,
        builder,
        dibuilder,
        compile_unit,
        tm,
        context.create_builder(),
    )
}

impl<'a, 'ctx> Ctx<'a, 'ctx> {
    pub fn new(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
        dibuilder: &'a DebugInfoBuilder<'ctx>,
        diunit: &'a DICompileUnit<'ctx>,
        tm: &'a TargetMachine,
        nodbg_builder: &'a Builder<'ctx>,
        src_file_path: &'a str,
        errs: &'a RefCell<Vec<PLDiag>>,
        edit_pos: Option<Pos>,
        config: Config,
        db: &'a dyn Db,
    ) -> Ctx<'a, 'ctx> {
        let f = Path::new(Path::new(src_file_path).file_stem().unwrap())
            .file_name()
            .take()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();
        let mut ctx = Ctx {
            need_highlight: true,
            generic_types: FxHashMap::default(),
            plmod: Mod::new(f, src_file_path.to_string()),
            father: None,
            llbuilder: RefCell::new(LLVMBuilder::new(
                context,
                module,
                builder,
                dibuilder,
                diunit,
                tm,
                nodbg_builder,
            )),
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
    pub fn new_child(&'a self, start: Pos) -> Ctx<'a, 'ctx> {
        let mut ctx = Ctx {
            need_highlight: self.need_highlight,
            generic_types: FxHashMap::default(),
            plmod: self.plmod.new_child(),
            father: Some(self),
            llbuilder: RefCell::new(self.llbuilder.borrow().new_child()),
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
        };
        add_primitive_types(&mut ctx);
        ctx
    }
    pub fn tmp_child_ctx(&'a self) -> Ctx<'a, 'ctx> {
        let mut ctx = Ctx {
            need_highlight: self.need_highlight,
            generic_types: FxHashMap::default(),
            plmod: self.plmod.new_child(),
            father: Some(self),
            llbuilder: RefCell::new(self.llbuilder.borrow().new_child()),
            errs: self.errs,
            edit_pos: self.edit_pos.clone(),
            table: FxHashMap::default(),
            config: self.config.clone(),
            usegc: self.usegc,
            db: self.db,
            block: self.block,
            continue_block: self.continue_block,
            break_block: self.break_block,
            return_block: self.return_block,
            roots: RefCell::new(Vec::new()),
            rettp: self.rettp.clone(),
        };
        add_primitive_types(&mut ctx);
        ctx
    }
    pub fn set_init_fn(&self) {
        self.llbuilder.borrow_mut().set_init_fn(&self);
    }
    pub fn clear_init_fn(&self) {
        self.llbuilder.borrow_mut().clear_init_fn();
    }
    pub fn add_method(&mut self, tp: &STType, mthd: &str, fntp: FNType, range: Range) {
        if self.plmod.add_method(tp, mthd, fntp).is_err() {
            self.add_err(range, ErrorCode::DUPLICATE_METHOD);
        }
    }
    pub fn init_fn_ret(&self) {
        self.llbuilder.borrow_mut().init_fn_ret();
    }
    /// # get_symbol
    /// search in current and all father symbol tables
    pub fn get_symbol(
        &self,
        name: &str,
    ) -> Option<(
        ValueHandle,
        Rc<RefCell<PLType>>,
        Range,
        Rc<RefCell<Vec<Location>>>,
        bool,
    )> {
        let v = self.table.get(name);
        if let Some((h, pltype, range, refs)) = v {
            return Some((*h, pltype.clone(), range.clone(), refs.clone(), false));
        }
        if let Some(father) = self.father {
            return father.get_symbol(name);
        }
        if let Some(GlobalVar {
            tp: pltype,
            range,
            loc: refs,
        }) = self.plmod.get_global_symbol(name)
        {
            return Some((
                self.llbuilder
                    .borrow()
                    .get_global_var_handle(&self.plmod.get_full_name(name))
                    .unwrap(),
                pltype.clone(),
                range.clone(),
                refs.clone(),
                true,
            ));
        }
        None
    }

    pub fn add_symbol(
        &mut self,
        name: String,
        pv: ValueHandle,
        pltype: Rc<RefCell<PLType>>,
        range: Range,
        is_const: bool,
    ) -> Result<(), PLDiag> {
        if self.table.contains_key(&name) {
            return Err(self.add_err(range, ErrorCode::REDECLARATION));
        }
        let refs = Rc::new(RefCell::new(vec![]));
        if is_const {
            self.plmod
                .add_global_symbol(name, pltype.clone(), range, refs.clone())?;
        } else {
            self.table
                .insert(name, (pv, pltype.clone(), range, refs.clone()));
        }
        self.send_if_go_to_def(range, range, self.plmod.path.clone());
        self.set_if_refs(refs, range);
        Ok(())
    }

    pub fn get_type(&self, name: &str, range: Range) -> Result<Rc<RefCell<PLType>>, PLDiag> {
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
        Err(PLDiag::new_error(range, ErrorCode::UNDEFINED_TYPE))
    }
    /// 用来获取外部模块的全局变量
    /// 如果没在当前module的全局变量表中找到，将会生成一个
    /// 该全局变量的声明
    pub fn get_or_add_global(&mut self, name: &str, pltype: Rc<RefCell<PLType>>) -> ValueHandle {
        self.llbuilder
            .borrow()
            .get_or_add_global(name, pltype, self)
    }
    pub fn init_global(&mut self) {
        let mut set: FxHashSet<String> = FxHashSet::default();
        for (_, sub) in &self.plmod.clone().submods {
            self.init_global_walk(&sub, &mut set);
        }
        let a: &[ValueHandle] = &[];
        self.llbuilder.borrow().build_call(
            self.llbuilder
                .borrow()
                .get_function(&self.plmod.get_full_name("__init_global"))
                .unwrap(),
            false,
            a.iter(),
        );
    }
    fn init_global_walk(&mut self, m: &Mod, set: &mut FxHashSet<String>) {
        let name = m.get_full_name("__init_global");
        if set.contains(&name) {
            return;
        }
        for (_, sub) in &m.submods {
            self.init_global_walk(sub, set);
        }
        let f = self
            .llbuilder
            .borrow()
            .add_function(&name, &[], PLType::VOID, self);
        let a: &[ValueHandle] = &[];
        self.llbuilder.borrow().build_call(f, false, a.iter());
        set.insert(name);
    }

    pub fn add_type(
        &mut self,
        name: String,
        pltype: Rc<RefCell<PLType>>,
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
            return Err(self.add_err(range, ErrorCode::REDEFINE_TYPE));
        }
        self.send_if_go_to_def(range, range, self.plmod.path.clone());
        self.plmod.types.insert(name, pltype.clone());
        Ok(())
    }
    pub fn add_type_without_check(&mut self, pltype: Rc<RefCell<PLType>>) {
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
    pub fn add_generic_type(&mut self, name: String, pltype: Rc<RefCell<PLType>>, range: Range) {
        self.send_if_go_to_def(range, range, self.plmod.path.clone());
        self.generic_types.insert(name, pltype.clone());
    }
    pub fn move_generic_types(&mut self) -> FxHashMap<String, Rc<RefCell<PLType>>> {
        self.generic_types.clone()
    }
    pub fn reset_generic_types(&mut self, mp: FxHashMap<String, Rc<RefCell<PLType>>>) {
        self.generic_types = mp
    }
    pub fn add_doc_symbols(&mut self, pltype: Rc<RefCell<PLType>>) {
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

    pub fn add_err(&self, range: Range, code: ErrorCode) -> PLDiag {
        let dia = PLDiag::new_error(range, code);
        self.errs.borrow_mut().push(dia.clone());
        dia
    }
    pub fn add_warn(&self, range: Range, code: WarnCode) -> PLDiag {
        let dia = PLDiag::new_warn(range, code);
        self.errs.borrow_mut().push(dia.clone());
        dia
    }

    pub fn add_diag(&mut self, dia: PLDiag) {
        self.errs.borrow_mut().push(dia);
    }
    // load type* to type
    pub fn try_load2var(
        &mut self,
        range: Range,
        v: PLValue,
        tp: Rc<RefCell<PLType>>,
    ) -> Result<(ValueHandle, Rc<RefCell<PLType>>), PLDiag> {
        let v = v.value;
        self.llbuilder.borrow().try_load2var(range, v, tp, self)
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

    pub fn get_file_url(&self) -> Url {
        Url::from_file_path(self.plmod.path.clone()).unwrap()
    }

    pub fn get_location(&self, range: Range) -> Location {
        Location::new(self.get_file_url(), range.to_diag_range())
    }

    pub fn set_if_refs_tp(&self, tp: Rc<RefCell<PLType>>, range: Range) {
        if let Some(tprefs) = tp.borrow().get_refs() {
            tprefs.borrow_mut().push(self.get_location(range));
            self.plmod.refs.borrow_mut().insert(range, tprefs.clone());
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

    pub fn set_if_refs(&self, refs: Rc<RefCell<Vec<Location>>>, range: Range) {
        refs.borrow_mut().push(self.get_location(range));
        self.plmod.refs.borrow_mut().insert(range, refs.clone());
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
        // self.get_var_completions_in_ns(ns, &mut m);
        // self.get_pltp_completions_in_ns(ns, &mut m);
        // self.get_keyword_completions_in_ns(ns, &mut m);
        // self.plmod.get_ns_completions_pri_in_ns(ns, &mut m);
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
            let tp = match *RefCell::borrow(&f) {
                PLType::FN(_) => continue,
                PLType::ARR(_) => continue,
                PLType::PLACEHOLDER(_) => CompletionItemKind::STRUCT,
                PLType::GENERIC(_) => CompletionItemKind::STRUCT,
                PLType::STRUCT(_) => CompletionItemKind::STRUCT,
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
        if !self.need_highlight {
            return;
        }
        self.plmod.semantic_tokens_builder.borrow_mut().push(
            range.to_diag_range(),
            type_index(tp),
            modifiers,
        )
    }
    pub fn push_type_hints(&self, range: Range, pltype: Rc<RefCell<PLType>>) {
        if !self.need_highlight {
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
        if !self.need_highlight {
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
    pub fn position_at_end(&mut self, block: BlockHandle) {
        self.block = Some(block);
        self.llbuilder.borrow().position_at_end_block(block);
    }
    fn get_keyword_completions(&self, vmap: &mut FxHashMap<String, CompletionItem>) {
        let keywords = vec![
            "if", "else", "while", "for", "return", "struct", "let", "true", "false",
        ];
        let loopkeys = vec!["break", "continue"];
        let toplevel = vec!["fn", "struct", "const", "use", "impl"];
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
        if !self.need_highlight {
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
        if !self.need_highlight {
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
    pub fn auto_deref(
        &self,
        tp: Rc<RefCell<PLType>>,
        value: ValueHandle,
    ) -> (Rc<RefCell<PLType>>, ValueHandle) {
        let mut tp = tp;
        let mut value = value;
        loop {
            match &*RefCell::borrow(&tp.clone()) {
                PLType::POINTER(p) => {
                    tp = p.clone();
                    value = self.llbuilder.borrow().build_load(value, "load");
                }
                _ => break,
            }
        }
        (tp, value)
    }

    /// # auto_deref_tp
    /// 自动解pltype引用，有几层解几层
    pub fn auto_deref_tp(&self, tp: Rc<RefCell<PLType>>) -> Rc<RefCell<PLType>> {
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
