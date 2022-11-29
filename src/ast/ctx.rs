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
use lsp_types::GotoDefinitionResponse;
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
use std::cell::Cell;
use std::cell::RefCell;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;

use super::accumulators::PLSignatureHelp;
use super::compiler::get_target_machine;
use super::compiler::ActionType;
use super::diag::{ErrorCode, WarnCode};
use super::diag::{ERR_MSG, WARN_MSG};
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
    pub context: &'ctx Context,            // llvm context
    pub builder: &'a Builder<'ctx>,        // llvm builder
    pub module: &'a Module<'ctx>,          // llvm module
    pub dibuilder: &'a DebugInfoBuilder<'ctx>, // debug info builder
    pub diunit: &'a DICompileUnit<'ctx>,   // debug info unit
    pub function: Option<FunctionValue<'ctx>>, // current function
    pub block: Option<BasicBlock<'ctx>>,   // current block
    pub continue_block: Option<BasicBlock<'ctx>>, // the block to jump when continue
    pub break_block: Option<BasicBlock<'ctx>>, // the block to jump to when break
    pub return_block: Option<(BasicBlock<'ctx>, Option<PointerValue<'ctx>>)>, // the block to jump to when return and value
    pub targetmachine: &'a TargetMachine, // might be used in debug info
    pub discope: DIScope<'ctx>,           // debug info scope
    pub nodebug_builder: &'a Builder<'ctx>, // builder without debug info
    pub errs: &'a RefCell<Vec<PLDiag>>,   // diagnostic list
    pub action: Option<ActionType>,       // lsp sender
    pub lspparams: Option<(Pos, Option<String>, ActionType)>, // lsp params
    pub refs: Rc<Cell<Option<Rc<RefCell<Vec<Location>>>>>>, // hold the find references result (thank you, Rust!)
    pub ditypes_placeholder: Rc<RefCell<FxHashMap<String, RefCell<Vec<MemberType<'ctx>>>>>>, // hold the generated debug info type place holder
    pub ditypes: Rc<RefCell<FxHashMap<String, DIType<'ctx>>>>, // hold the generated debug info type
    pub hints: Rc<RefCell<Box<Vec<InlayHint>>>>,
    pub doc_symbols: Rc<RefCell<Box<Vec<DocumentSymbol>>>>,
    pub semantic_tokens_builder: Rc<RefCell<Box<SemanticTokensBuilder>>>, // semantic token builder
    pub goto_def: Rc<Cell<Option<GotoDefinitionResponse>>>, // hold the goto definition result
    pub completion_items: Rc<Cell<Vec<CompletionItem>>>,    // hold the completion items
    pub hover: Rc<Cell<Option<Hover>>>,                     // hold the hover result
    pub init_func: Option<FunctionValue<'ctx>>,             //init function,call first in main
    pub table: FxHashMap<
        String,
        (
            PointerValue<'ctx>,
            Rc<RefCell<PLType>>,
            Range,
            Rc<RefCell<Vec<Location>>>,
        ),
    >, // variable table
    pub config: Config,                                     // config
    pub roots: RefCell<Vec<BasicValueEnum<'ctx>>>,
    pub usegc: bool,
    pub db: &'a dyn Db,
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
        sender: Option<ActionType>,
        completion: Option<(Pos, Option<String>, ActionType)>,
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
            context,
            module,
            builder,
            function: None,
            block: None,
            continue_block: None,
            break_block: None,
            return_block: None,
            dibuilder,
            diunit,
            targetmachine: tm,
            discope: diunit.get_file().as_debug_info_scope(),
            nodebug_builder: nodbg_builder,
            errs,
            action: sender,
            lspparams: completion,
            refs: Rc::new(Cell::new(None)),
            hints: Rc::new(RefCell::new(Box::new(vec![]))),
            doc_symbols: Rc::new(RefCell::new(Box::new(vec![]))),
            semantic_tokens_builder: Rc::new(RefCell::new(Box::new(SemanticTokensBuilder::new(
                src_file_path.to_string(),
            )))),
            goto_def: Rc::new(Cell::new(None)),
            completion_items: Rc::new(Cell::new(Vec::new())),
            hover: Rc::new(Cell::new(None)),
            init_func: None,
            table: FxHashMap::default(),
            config,
            roots: RefCell::new(Vec::new()),
            usegc: true,
            ditypes_placeholder: Rc::new(RefCell::new(FxHashMap::default())),
            ditypes: Rc::new(RefCell::new(FxHashMap::default())),
            db,
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
            context: self.context,
            builder: self.builder,
            module: self.module,
            function: self.function,
            block: self.block,
            continue_block: self.continue_block,
            break_block: self.break_block,
            return_block: self.return_block,
            dibuilder: self.dibuilder,
            diunit: self.diunit,
            targetmachine: self.targetmachine,
            discope: self
                .dibuilder
                .create_lexical_block(
                    self.discope,
                    self.diunit.get_file(),
                    start.line as u32,
                    start.column as u32,
                )
                .as_debug_info_scope(),
            nodebug_builder: self.nodebug_builder,
            errs: self.errs,
            action: self.action,
            lspparams: self.lspparams.clone(),
            refs: self.refs.clone(),
            hints: self.hints.clone(),
            doc_symbols: self.doc_symbols.clone(),
            semantic_tokens_builder: self.semantic_tokens_builder.clone(),
            goto_def: self.goto_def.clone(),
            completion_items: self.completion_items.clone(),
            hover: self.hover.clone(),
            init_func: self.init_func,
            table: FxHashMap::default(),
            config: self.config.clone(),
            roots: RefCell::new(Vec::new()),
            usegc: self.usegc,
            ditypes_placeholder: self.ditypes_placeholder.clone(),
            ditypes: self.ditypes.clone(),
            db: self.db.clone(),
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
            context: self.context,
            builder: self.builder,
            module: self.module,
            function: self.function,
            block: self.block,
            continue_block: self.continue_block,
            break_block: self.break_block,
            return_block: self.return_block,
            dibuilder: self.dibuilder,
            diunit: self.diunit,
            targetmachine: self.targetmachine,
            discope: self.discope.clone(),
            nodebug_builder: self.nodebug_builder,
            errs: self.errs,
            action: self.action,
            lspparams: self.lspparams.clone(),
            refs: self.refs.clone(),
            hints: self.hints.clone(),
            doc_symbols: self.doc_symbols.clone(),
            semantic_tokens_builder: self.semantic_tokens_builder.clone(),
            goto_def: self.goto_def.clone(),
            completion_items: self.completion_items.clone(),
            hover: self.hover.clone(),
            init_func: self.init_func,
            table: FxHashMap::default(),
            config: self.config.clone(),
            roots: RefCell::new(Vec::new()),
            usegc: self.usegc,
            ditypes_placeholder: self.ditypes_placeholder.clone(),
            ditypes: self.ditypes.clone(),
            db: self.db,
        };
        add_primitive_types(&mut ctx);
        ctx
    }
    pub fn set_init_fn(&mut self) {
        self.function = Some(self.module.add_function(
            &self.plmod.get_full_name("__init_global"),
            self.context.void_type().fn_type(&vec![], false),
            None,
        ));
        self.init_func = self.function;
        self.context
            .append_basic_block(self.init_func.unwrap(), "alloc");
        let entry = self
            .context
            .append_basic_block(self.init_func.unwrap(), "entry");
        self.position_at_end(entry);
    }
    pub fn clear_init_fn(&mut self) {
        let alloc = self.init_func.unwrap().get_first_basic_block().unwrap();
        let entry = self.init_func.unwrap().get_last_basic_block().unwrap();
        unsafe {
            entry.delete().unwrap();
            alloc.delete().unwrap();
        }
        self.context
            .append_basic_block(self.init_func.unwrap(), "alloc");
        self.context
            .append_basic_block(self.init_func.unwrap(), "entry");
    }
    pub fn add_method(&mut self, tp: &STType, mthd: &str, fntp: FNType, range: Range) {
        if self.plmod.add_method(tp, mthd, fntp).is_err() {
            self.add_err(range, ErrorCode::DUPLICATE_METHOD);
        }
    }
    pub fn init_fn_ret(&mut self) {
        let alloc = self.init_func.unwrap().get_first_basic_block().unwrap();
        let entry = self.init_func.unwrap().get_last_basic_block().unwrap();
        self.position_at_end(alloc);
        self.nodebug_builder.build_unconditional_branch(entry);
        self.position_at_end(entry);
        self.nodebug_builder.build_return(None);
    }
    /// # get_symbol
    /// search in current and all father symbol tables
    pub fn get_symbol(
        &self,
        name: &str,
    ) -> Option<(
        PointerValue<'ctx>,
        Rc<RefCell<PLType>>,
        Range,
        Rc<RefCell<Vec<Location>>>,
        bool,
    )> {
        let v = self.table.get(name);
        if let Some((pv, pltype, range, refs)) = v {
            return Some((
                pv.clone(),
                pltype.clone(),
                range.clone(),
                refs.clone(),
                false,
            ));
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
                self.module
                    .get_global(&self.plmod.get_full_name(name))
                    .unwrap()
                    .as_pointer_value(),
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
        pv: PointerValue<'ctx>,
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
    pub fn get_or_add_global(
        &mut self,
        name: &str,
        pltype: Rc<RefCell<PLType>>,
    ) -> PointerValue<'ctx> {
        let global = self.module.get_global(name);
        if global.is_none() {
            let global = self
                .module
                .add_global(pltype.borrow().get_basic_type(self), None, name);
            global.set_linkage(Linkage::External);
            return global.as_pointer_value();
        }
        global.unwrap().as_pointer_value()
    }
    pub fn init_global(&mut self) {
        let mut set: FxHashSet<String> = FxHashSet::default();
        for (_, sub) in &self.plmod.clone().submods {
            self.init_global_walk(&sub, &mut set);
        }
        self.nodebug_builder.build_call(
            self.module
                .get_function(&self.plmod.get_full_name("__init_global"))
                .unwrap(),
            &[],
            "",
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
            .module
            .add_function(&name, self.context.void_type().fn_type(&[], false), None);
        self.nodebug_builder.build_call(f, &[], "");
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
        let range = pltype.borrow().get_range().unwrap();
        self.send_if_go_to_def(range, range, self.plmod.path.clone());
        self.plmod.types.insert(name, pltype.clone());
    }
    pub fn add_generic_type(
        &mut self,
        name: String,
        pltype: Rc<RefCell<PLType>>,
        range: Range,
    ) -> Result<(), PLDiag> {
        self.send_if_go_to_def(range, range, self.plmod.path.clone());
        self.generic_types.insert(name, pltype.clone());
        Ok(())
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
                    self.doc_symbols.borrow_mut().push(f.get_doc_symbol())
                }
            }
            PLType::STRUCT(st) => self.doc_symbols.borrow_mut().push(st.get_doc_symbol()),
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
        v: PLValue<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, PLDiag> {
        let v = v.value;
        if !v.is_pointer_value() {
            return Ok(match v {
                AnyValueEnum::ArrayValue(v) => v.into(),
                AnyValueEnum::IntValue(v) => v.into(),
                AnyValueEnum::FloatValue(v) => v.into(),
                AnyValueEnum::PointerValue(v) => v.into(),
                AnyValueEnum::StructValue(v) => v.into(),
                AnyValueEnum::VectorValue(v) => v.into(),
                _ => return Err(self.add_err(range, ErrorCode::EXPECT_VALUE)),
            });
        } else {
            Ok(self.builder.build_load(v.into_pointer_value(), "loadtmp"))
        }
    }
    pub fn if_completion(&mut self, c: impl FnOnce(&mut Ctx, &(Pos, Option<String>))) {
        if let Some(tp) = self.action {
            if let Some(comp) = &self.lspparams {
                if tp == ActionType::Completion {
                    let v = self.completion_items.take();
                    if v.is_empty() {
                        c(self, &(comp.0, comp.1.clone()));
                    } else {
                        self.completion_items.set(v);
                    }
                }
            }
        }
    }
    pub fn if_completion_no_mut(&self, c: impl FnOnce(&Ctx, &(Pos, Option<String>))) {
        if let Some(tp) = self.action {
            if let Some(comp) = &self.lspparams {
                if tp == ActionType::Completion {
                    let v = self.completion_items.take();
                    if v.is_empty() {
                        c(self, &(comp.0, comp.1.clone()));
                    } else {
                        self.completion_items.set(v);
                    }
                }
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
        if let Some(_) = self.action {
            if let Some(comp) = &self.lspparams {
                let tprefs = RefCell::borrow(&tp).get_refs();
                if let Some(tprefs) = tprefs {
                    tprefs.borrow_mut().push(self.get_location(range));
                    if comp.0.is_in(range) {
                        self.refs.set(Some(tprefs));
                    }
                }
            }
        }
    }

    pub fn set_if_sig(&self, range: Range, name: String, params: &[String], n: u32) {
        if let Some(act) = self.action {
            if act != ActionType::SignatureHelp {
                return;
            }
            if let Some(comp) = &self.lspparams {
                if comp.0.is_in(range) {
                    PLSignatureHelp::push(
                        self.db,
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
            }
        }
    }

    pub fn set_if_refs(&self, refs: Rc<RefCell<Vec<Location>>>, range: Range) {
        if let Some(act) = self.action {
            if act == ActionType::FindReferences {
                if let Some(comp) = &self.lspparams {
                    refs.borrow_mut().push(self.get_location(range));
                    if comp.0.is_in(range) {
                        self.refs.set(Some(refs));
                    }
                }
            }
        }
    }

    pub fn send_if_go_to_def(&self, range: Range, destrange: Range, file: String) {
        if let Some(_) = self.action {
            if let Some(comp) = &self.lspparams {
                if comp.2 == ActionType::GotoDef {
                    let v = self.goto_def.take();
                    if v.is_none() && comp.0.is_in(range) {
                        let resp = GotoDefinitionResponse::Scalar(Location {
                            uri: Url::from_file_path(file).unwrap(),
                            range: destrange.to_diag_range(),
                        });
                        self.goto_def.set(Some(resp));
                        // self.action = None // set sender to None so it won't be sent again
                    } else {
                        self.goto_def.set(v);
                    }
                }
            }
        }
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
        self.semantic_tokens_builder.borrow_mut().push(
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
        self.hints.borrow_mut().push(hint);
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
        self.hints.borrow_mut().push(hint);
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
        if let Some(act) = self.action {
            if let Some(comp) = &self.lspparams {
                if act == ActionType::Hover {
                    if comp.0.is_in(range) {
                        let hover = Hover {
                            contents: value,
                            range: None,
                        };
                        self.hover.set(Some(hover));
                    }
                }
            }
        }
    }
    /// # auto_deref
    /// 自动解引用，有几层解几层
    pub fn auto_deref(
        &self,
        tp: Rc<RefCell<PLType>>,
        value: PointerValue<'ctx>,
    ) -> (Rc<RefCell<PLType>>, PointerValue<'ctx>) {
        let mut tp = tp;
        let mut value = value;
        loop {
            match &*RefCell::borrow(&tp.clone()) {
                PLType::POINTER(p) => {
                    tp = p.clone();
                    value = self.builder.build_load(value, "load").into_pointer_value();
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
