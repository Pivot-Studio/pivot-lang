use crate::ast::node::Value;
use crate::lsp::semantic_tokens::type_index;
use crate::lsp::semantic_tokens::SemanticTokensBuilder;
use colored::Colorize;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::debug_info::*;
use inkwell::module::FlagBehavior;
use inkwell::module::Linkage;
use inkwell::module::Module;
use inkwell::targets::TargetMachine;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::types::FunctionType;
use inkwell::types::StructType;
use inkwell::types::VoidType;
use inkwell::values::BasicMetadataValueEnum;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use inkwell::values::PointerValue;
use lsp_types::CompletionItem;
use lsp_types::CompletionItemKind;
use lsp_types::Diagnostic;
use lsp_types::DiagnosticSeverity;
use lsp_types::GotoDefinitionResponse;
use lsp_types::Hover;
use lsp_types::HoverContents;
use lsp_types::InsertTextFormat;
use lsp_types::Location;
use lsp_types::MarkedString;
use lsp_types::SemanticTokenType;
use lsp_types::Url;
use rustc_hash::FxHashMap;
use std::cell::Cell;
use std::cell::RefCell;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;

use super::compiler::get_target_machine;
use super::compiler::ActionType;
use super::diag::{ErrorCode, WarnCode};
use super::diag::{ERR_MSG, WARN_MSG};
use super::node::function::FuncTypeNode;
use super::node::types::TypeNameNode;
use super::node::NodeEnum;
use super::range::Pos;
use super::range::Range;
// TODO: match all case
// const DW_ATE_UTF: u32 = 0x10;
const DW_ATE_BOOLEAN: u32 = 0x02;
const DW_ATE_FLOAT: u32 = 0x04;
const DW_ATE_SIGNED: u32 = 0x05;
const DW_TAG_REFERENCE_TYPE: u32 = 16;
// const DW_ATE_UNSIGNED: u32 = 0x07;
fn get_dw_ate_encoding(basetype: &BasicTypeEnum) -> u32 {
    match basetype {
        BasicTypeEnum::FloatType(_) => DW_ATE_FLOAT,
        BasicTypeEnum::IntType(i) => match i.get_bit_width() {
            8 => DW_ATE_BOOLEAN,
            64 => DW_ATE_SIGNED,
            _ => todo!(),
        },
        _ => todo!(),
    }
}

/// # Ctx
/// Context for code generation
pub struct Ctx<'a, 'ctx> {
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
    pub semantic_tokens_builder: Rc<RefCell<Box<SemanticTokensBuilder>>>, // semantic token builder
    pub goto_def: Rc<Cell<Option<GotoDefinitionResponse>>>, // hold the goto definition result
    pub completion_items: Rc<Cell<Vec<CompletionItem>>>,    // hold the completion items
    pub hover: Rc<Cell<Option<Hover>>>,                     // hold the hover result
    pub init_func: Option<FunctionValue<'ctx>>,             //init function,call first in main
    pub table: FxHashMap<
        String,
        (
            PointerValue<'ctx>,
            String,
            Range,
            Rc<RefCell<Vec<Location>>>,
        ),
    >, // variable table
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalVar {
    pub tp: String,
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
    pub types: FxHashMap<String, PLType>,
    /// sub mods
    pub submods: FxHashMap<String, Mod>,
    // global variable table
    pub global_table: FxHashMap<String, GlobalVar>,
}

impl Mod {
    pub fn new(name: String, path: String) -> Self {
        Self {
            name,
            path,
            types: FxHashMap::default(),
            submods: FxHashMap::default(),
            global_table: FxHashMap::default(),
        }
    }
    pub fn new_child(&self) -> Self {
        Mod {
            name: self.name.clone(),
            path: self.path.clone(),
            types: FxHashMap::default(),
            submods: self.submods.clone(),
            global_table: FxHashMap::default(),
        }
    }
    pub fn get_global_symbol(&self, name: &str) -> Option<&GlobalVar> {
        self.global_table.get(name)
    }
    pub fn add_global_symbol(
        &mut self,
        name: String,
        tp: String,
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
    pub fn get_type(&self, name: &str) -> Option<PLType> {
        let v = self.types.get(name);
        if let Some(pv) = v {
            return Some(pv.clone());
        }
        if let Some(x) = PriType::try_from_str(name) {
            return Some(PLType::PRIMITIVE(x));
        }
        if name == "void" {
            return Some(PLType::VOID);
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
                    "error at {}\n\t{}",
                    format!(
                        "{}:{}:{}",
                        path,
                        s.diag.range.start.line + 1,
                        s.diag.range.start.character + 1
                    )
                    .red(),
                    format!("{}", s.diag.message.blue().bold()),
                );
                println!("{}", err);
            }
            PLDiag::Warn(s) => {
                let err = format!(
                    "warn at {}\n\t{}",
                    format!(
                        "{}:{}:{}",
                        path,
                        s.diag.range.start.line + 1,
                        s.diag.range.start.character + 1
                    )
                    .yellow(),
                    format!("{}", s.diag.message.blue().bold()),
                );
                println!("{}", err);
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

/// # PLType
/// Type for pivot-lang
/// including primitive type, struct type, function type, void type
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PLType {
    FN(FNType),
    STRUCT(STType),
    PRIMITIVE(PriType),
    VOID,
    NAMESPACE(Mod),
}

fn pri_ty<'ctx>(s: &str, ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
    match s {
        "i32" => ctx.i32_type().as_basic_type_enum(),
        "i64" => ctx.i64_type().as_basic_type_enum(),
        "i8" | "bool" => ctx.i8_type().as_basic_type_enum(),
        "i16" => ctx.i16_type().as_basic_type_enum(),
        "i1" => ctx.bool_type().as_basic_type_enum(),
        "f32" => ctx.f32_type().as_basic_type_enum(),
        "f64" => ctx.f64_type().as_basic_type_enum(),
        _ => panic!("unknown type {}", s),
    }
}

/// # PriType
/// Primitive type for pivot-lang
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PriType {
    // basetype: BasicTypeEnum<'ctx>,
    id: String,
}
impl PriType {
    pub fn get_basic_type<'a, 'ctx>(&self, ctx: &Ctx<'a, 'ctx>) -> BasicTypeEnum<'ctx> {
        pri_ty(&self.id, ctx.context)
    }
    pub fn try_from_str(s: &str) -> Option<Self> {
        match s {
            "i32" | "i64" | "i8" | "bool" | "i16" | "i1" | "f32" | "f64" => {
                Some(PriType { id: s.to_string() })
            }
            _ => None,
        }
    }
}

impl PLType {
    /// # get_refs
    /// get the references of the type
    /// used in find references
    /// void type and primitive types has no references
    pub fn get_refs(&self) -> Option<Rc<RefCell<Vec<Location>>>> {
        match self {
            PLType::FN(f) => Some(f.refs.clone()),
            PLType::STRUCT(s) => Some(s.refs.clone()),
            PLType::PRIMITIVE(_) => None,
            PLType::VOID => None,
            PLType::NAMESPACE(_) => None,
        }
    }

    /// # get_basic_type
    /// get the basic type of the type
    /// used in code generation
    /// may panic if the type is void type
    pub fn get_basic_type<'a, 'ctx>(&self, ctx: &Ctx<'a, 'ctx>) -> BasicTypeEnum<'ctx> {
        self.get_basic_type_op(ctx).unwrap()
    }

    /// # get_range
    /// get the defination range of the type
    pub fn get_range(&self) -> Option<Range> {
        match self {
            PLType::FN(f) => Some(f.range.clone()),
            PLType::STRUCT(s) => Some(s.range.clone()),
            PLType::PRIMITIVE(_) => None,
            PLType::VOID => None,
            PLType::NAMESPACE(_) => None,
        }
    }

    /// # get_basic_type_op
    /// get the basic type of the type
    /// used in code generation
    pub fn get_basic_type_op<'a, 'ctx>(&self, ctx: &Ctx<'a, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        match self {
            PLType::FN(f) => Some(
                f.get_value(ctx, &ctx.plmod)
                    .get_type()
                    .ptr_type(inkwell::AddressSpace::Global)
                    .as_basic_type_enum(),
            ),
            PLType::STRUCT(s) => Some(s.struct_type(&ctx).as_basic_type_enum()),
            PLType::PRIMITIVE(t) => Some(t.get_basic_type(ctx)),
            PLType::VOID => None,
            PLType::NAMESPACE(_) => None,
        }
    }

    /// # get_ret_type
    /// get the return type, which is void type or primitive type
    pub fn get_ret_type<'a, 'ctx>(&self, ctx: &Ctx<'a, 'ctx>) -> RetTypeEnum<'ctx> {
        match self {
            PLType::VOID => RetTypeEnum::VOID(ctx.context.void_type()),
            _ => RetTypeEnum::BASIC(self.get_basic_type(ctx)),
        }
    }
    pub fn is_void(&self) -> bool {
        if let PLType::VOID = self {
            true
        } else {
            false
        }
    }

    /// # get_ditype
    /// get the debug info type of the pltype
    pub fn get_ditype<'a, 'ctx>(&self, ctx: &Ctx<'a, 'ctx>) -> Option<DIType<'ctx>> {
        let td = ctx.targetmachine.get_target_data();
        match self {
            PLType::FN(_) => None,
            PLType::STRUCT(x) => {
                let mut offset = 0;
                let m = x
                    .ordered_fields
                    .iter()
                    .map(|v| {
                        let (tp, off) = v.get_di_type(ctx, offset);
                        offset = off;
                        tp
                    })
                    .collect::<Vec<_>>();
                return Some(
                    ctx.dibuilder
                        .create_struct_type(
                            ctx.discope,
                            &x.name,
                            ctx.diunit.get_file(),
                            x.range.start.line as u32 + 1,
                            td.get_bit_size(&x.struct_type(ctx)),
                            td.get_abi_alignment(&x.struct_type(ctx)),
                            DIFlags::PUBLIC,
                            None,
                            &m,
                            0,
                            None,
                            &x.name,
                        )
                        .as_type(),
                );
            }
            PLType::PRIMITIVE(pt) => {
                return Some(
                    ctx.dibuilder
                        .create_basic_type(
                            &pt.id,
                            td.get_bit_size(&self.get_basic_type(ctx)),
                            get_dw_ate_encoding(&self.get_basic_type(ctx)),
                            DIFlags::PUBLIC,
                        )
                        .unwrap()
                        .as_type(),
                );
            }
            PLType::VOID => None,
            PLType::NAMESPACE(_) => None,
        }
    }
    pub fn get_di_ref_type<'a, 'ctx>(
        &self,
        ctx: &Ctx<'a, 'ctx>,
        ditype: Option<DIType<'ctx>>,
    ) -> Option<DIDerivedType<'ctx>> {
        if ditype.is_none() {
            return None;
        }
        Some(
            ctx.dibuilder
                .create_reference_type(ditype.unwrap(), DW_TAG_REFERENCE_TYPE),
        )
    }
}

pub enum RetTypeEnum<'ctx> {
    VOID(VoidType<'ctx>),
    BASIC(BasicTypeEnum<'ctx>),
}

impl<'ctx> RetTypeEnum<'ctx> {
    pub fn fn_type(
        &self,
        param_types: &[BasicMetadataTypeEnum<'ctx>],
        is_var_args: bool,
    ) -> FunctionType<'ctx> {
        match self {
            RetTypeEnum::VOID(t) => t.fn_type(param_types, is_var_args),
            RetTypeEnum::BASIC(t) => t.fn_type(param_types, is_var_args),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub index: u32,
    pub tp: PLType,
    pub typename: Box<TypeNameNode>,
    pub name: String,
    pub range: Range,
    pub is_ref: bool,
    pub refs: Rc<RefCell<Vec<Location>>>,
}

impl Field {
    pub fn get_di_type<'a, 'ctx>(&self, ctx: &Ctx<'a, 'ctx>, offset: u64) -> (DIType<'ctx>, u64) {
        let pltype = ctx
            .get_type(&self.typename.id, self.typename.range)
            .unwrap();
        let di_type = pltype.get_ditype(ctx);
        let debug_type = if self.is_ref {
            pltype
                .clone()
                .get_di_ref_type(ctx, di_type.clone())
                .unwrap()
                .as_type()
        } else {
            di_type.unwrap()
        };
        (
            ctx.dibuilder
                .create_member_type(
                    ctx.discope,
                    &self.name,
                    ctx.diunit.get_file(),
                    self.typename.range.start.line as u32,
                    debug_type.get_size_in_bits(),
                    debug_type.get_align_in_bits(),
                    offset + debug_type.get_offset_in_bits(),
                    DIFlags::PUBLIC,
                    debug_type,
                )
                .as_type(),
            offset + debug_type.get_size_in_bits(),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FNType {
    pub name: String,
    pub fntype: FuncTypeNode,
    pub ret_pltype: Option<String>,
    pub range: Range,
    pub refs: Rc<RefCell<Vec<Location>>>,
    pub is_ref: bool,
    pub doc: Vec<Box<NodeEnum>>,
}

impl FNType {
    /// try get function value from module
    ///
    /// if not found, create a declaration
    pub fn get_value<'a, 'ctx>(&self, ctx: &Ctx<'a, 'ctx>, m: &Mod) -> FunctionValue<'ctx> {
        if let Some(v) = ctx.module.get_function(&m.get_full_name(&self.name)) {
            return v;
        }
        if let Some(v) = ctx.module.get_function(&self.name) {
            return v;
        }
        let ret_pltype = ctx
            .get_type(&self.ret_pltype.as_ref().unwrap(), self.range)
            .unwrap();
        let mut param_types = vec![];
        for param in self.fntype.paralist.iter() {
            let pltype = m.get_type(&param.tp.id).unwrap();
            param_types.push(pltype.get_basic_type(ctx).into());
        }
        let fn_type = ret_pltype.get_ret_type(ctx).fn_type(&param_types, false);
        let fn_value = ctx.module.add_function(
            &m.get_full_name(&self.name),
            fn_type,
            Some(Linkage::External),
        );
        fn_value
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct STType {
    pub name: String,
    pub fields: FxHashMap<String, Field>,
    pub ordered_fields: Vec<Field>,
    pub range: Range,
    pub refs: Rc<RefCell<Vec<Location>>>,
    pub doc: Vec<Box<NodeEnum>>,
}

impl STType {
    pub fn struct_type<'a, 'ctx>(&'a self, ctx: &Ctx<'a, 'ctx>) -> StructType<'ctx> {
        let st = ctx
            .module
            .get_struct_type(&ctx.plmod.get_full_name(&self.name));
        if let Some(st) = st {
            return st;
        }
        let st = ctx
            .context
            .opaque_struct_type(&ctx.plmod.get_full_name(&self.name));
        st.set_body(
            &self
                .ordered_fields
                .clone()
                .into_iter()
                .map(|order_field| {
                    if order_field.is_ref {
                        order_field
                            .tp
                            .get_basic_type(&ctx)
                            .ptr_type(inkwell::AddressSpace::Generic)
                            .as_basic_type_enum()
                    } else {
                        order_field.tp.get_basic_type(&ctx)
                    }
                })
                .collect::<Vec<_>>(),
            false,
        );
        st
    }
    pub fn get_completions(&self) -> Vec<CompletionItem> {
        let mut completions = Vec::new();
        for (name, _) in &self.fields {
            completions.push(CompletionItem {
                kind: Some(CompletionItemKind::FIELD),
                label: name.clone(),
                detail: Some("field".to_string()),
                insert_text: Some(name.clone()),
                insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
                ..Default::default()
            });
        }
        completions
    }
}

fn add_primitive_types<'a, 'ctx>(ctx: &mut Ctx<'a, 'ctx>) {
    let pltype_i64 = PLType::PRIMITIVE(PriType {
        id: "i64".to_string(),
    });
    ctx.plmod
        .types
        .insert("i64".to_string(), pltype_i64.clone());

    let pltype_f64 = PLType::PRIMITIVE(PriType {
        id: "f64".to_string(),
    });
    ctx.plmod
        .types
        .insert("f64".to_string(), pltype_f64.clone());

    let pltype_bool = PLType::PRIMITIVE(PriType {
        id: "bool".to_string(),
    });
    ctx.plmod
        .types
        .insert("bool".to_string(), pltype_bool.clone());

    let pltype_void = PLType::VOID;
    ctx.plmod
        .types
        .insert("void".to_string(), pltype_void.clone());
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
    // let metacv = context.metadata_node(&[BasicMetadataValueEnum::IntValue(
    //     context.i32_type().const_int(1, false),
    // )]);
    module.add_metadata_flag("Debug Info Version", FlagBehavior::Warning, metav);
    // module.add_metadata_flag("CodeView", FlagBehavior::Warning, metacv); // TODO: is this needed for windows debug?
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
    ) -> Ctx<'a, 'ctx> {
        let f = Path::new(Path::new(src_file_path).file_stem().unwrap())
            .file_name()
            .take()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();
        let mut ctx = Ctx {
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
            semantic_tokens_builder: Rc::new(RefCell::new(Box::new(SemanticTokensBuilder::new(
                src_file_path.to_string(),
            )))),
            goto_def: Rc::new(Cell::new(None)),
            completion_items: Rc::new(Cell::new(Vec::new())),
            hover: Rc::new(Cell::new(None)),
            init_func: None,
            table: FxHashMap::default(),
        };
        add_primitive_types(&mut ctx);
        ctx
    }
    pub fn new_child(&'a self, start: Pos) -> Ctx<'a, 'ctx> {
        let mut ctx = Ctx {
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
            semantic_tokens_builder: self.semantic_tokens_builder.clone(),
            goto_def: self.goto_def.clone(),
            completion_items: self.completion_items.clone(),
            hover: self.hover.clone(),
            init_func: self.init_func,
            table: FxHashMap::default(),
        };
        add_primitive_types(&mut ctx);
        ctx
    }

    /// # get_symbol
    /// search in current and all father symbol tables
    pub fn get_symbol(
        &self,
        name: &str,
    ) -> Option<(
        PointerValue<'ctx>,
        String,
        Range,
        Rc<RefCell<Vec<Location>>>,
        bool,
    )> {
        let v = self.table.get(name);
        if let Some((pv, pltype, range, refs)) = v {
            return Some((
                pv.clone(),
                pltype.to_string(),
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
                self.module.get_global(name).unwrap().as_pointer_value(),
                pltype.to_string(),
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
        tp: String,
        range: Range,
        is_const: bool,
    ) -> Result<(), PLDiag> {
        if self.table.contains_key(&name) {
            return Err(self.add_err(range, ErrorCode::REDECLARATION));
        }
        let refs = Rc::new(RefCell::new(vec![]));
        if is_const {
            self.plmod
                .add_global_symbol(name, tp, range, refs.clone())?;
        } else {
            self.table.insert(name, (pv, tp, range, refs.clone()));
        }
        self.send_if_go_to_def(range, range, self.plmod.path.clone());
        self.set_if_refs(refs, range);
        Ok(())
    }

    pub fn get_type(&self, name: &str, range: Range) -> Result<&PLType, PLDiag> {
        let v = self.plmod.types.get(name);
        if let Some(pv) = v {
            return Ok(pv);
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
    pub fn get_or_add_global(&self, name: &str, m: &Mod, tp: &str) -> PointerValue<'ctx> {
        let global = self.module.get_global(name);
        if global.is_none() {
            let pltype = m.get_type(tp).unwrap();
            let global = self
                .module
                .add_global(pltype.get_basic_type(self), None, name);
            global.set_linkage(Linkage::External);
            return global.as_pointer_value();
        }
        global.unwrap().as_pointer_value()
    }

    pub fn add_type(&mut self, name: String, tp: PLType, range: Range) -> Result<(), PLDiag> {
        if self.plmod.types.contains_key(&name) {
            return Err(self.add_err(range, ErrorCode::REDEFINE_TYPE));
        }
        self.send_if_go_to_def(range, range, self.plmod.path.clone());
        self.plmod.types.insert(name, tp.clone());
        Ok(())
    }

    pub fn add_err(&mut self, range: Range, code: ErrorCode) -> PLDiag {
        let dia = PLDiag::new_error(range, code);
        self.errs.borrow_mut().push(dia.clone());
        dia
    }
    pub fn add_warn(&mut self, range: Range, code: WarnCode) -> PLDiag {
        let dia = PLDiag::new_warn(range, code);
        self.errs.borrow_mut().push(dia.clone());
        dia
    }

    pub fn add_diag(&mut self, dia: PLDiag) {
        self.errs.borrow_mut().push(dia);
    }
    // load type** and type* to type
    pub fn try_load2var(&mut self, v: Value<'ctx>) -> Value<'ctx> {
        let basic_value_op = v.as_basic_value_enum_op();
        if basic_value_op.is_none() {
            return Value::None;
        }
        match basic_value_op.unwrap() {
            BasicValueEnum::IntValue(v) => match v.get_type().get_bit_width() {
                8 => Value::BoolValue(v),
                64 => Value::IntValue(v),
                _ => todo!(),
            },
            BasicValueEnum::FloatValue(v) => Value::FloatValue(v),
            BasicValueEnum::PointerValue(ptr2v) => {
                let loadv = self.builder.build_load(ptr2v, "loadtmp");
                self.try_load2var(Value::LoadValue(loadv))
            }
            _ => v,
        }
    }
    // load type** and type* to type*
    pub fn try_load2ptr(&mut self, v: Value<'ctx>) -> Value<'ctx> {
        match v.as_basic_value_enum() {
            BasicValueEnum::PointerValue(ptr2value) => {
                if ptr2value.get_type().get_element_type().is_pointer_type() {
                    let value = self.builder.build_load(ptr2value, "loadtmp");
                    Value::VarValue(value.into_pointer_value())
                } else {
                    Value::VarValue(ptr2value)
                }
            }
            _ => v,
        }
    }

    pub fn if_completion(&mut self, c: impl FnOnce(&mut Ctx, &(Pos, Option<String>))) {
        if let Some(tp) = self.action {
            if let Some(comp) = &self.lspparams {
                if tp == ActionType::Completion {
                    c(self, &(comp.0, comp.1.clone()));
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

    pub fn set_if_refs_tp(&self, tp: &PLType, range: Range) {
        if let Some(_) = self.action {
            if let Some(comp) = &self.lspparams {
                let tprefs = tp.get_refs();
                if let Some(tprefs) = tprefs {
                    tprefs.borrow_mut().push(self.get_location(range));
                    if comp.0.is_in(range) {
                        self.refs.set(Some(tprefs));
                    }
                }
            }
        }
    }

    pub fn set_if_refs(&self, refs: Rc<RefCell<Vec<Location>>>, range: Range) {
        if let Some(_) = self.action {
            if let Some(comp) = &self.lspparams {
                refs.borrow_mut().push(self.get_location(range));
                if comp.0.is_in(range) {
                    self.refs.set(Some(refs));
                }
            }
        }
    }

    pub fn send_if_go_to_def(&mut self, range: Range, destrange: Range, file: String) {
        if let Some(_) = self.action {
            if let Some(comp) = &self.lspparams {
                if comp.2 == ActionType::GotoDef {
                    if comp.0.is_in(range) {
                        let resp = GotoDefinitionResponse::Scalar(Location {
                            uri: Url::from_file_path(file).unwrap(),
                            range: destrange.to_diag_range(),
                        });
                        self.goto_def.set(Some(resp));
                        self.action = None // set sender to None so it won't be sent again
                    }
                }
            }
        }
    }

    pub fn get_completions(&self) -> Vec<CompletionItem> {
        let mut m = FxHashMap::default();
        self.get_var_completions(&mut m);
        self.get_pltp_completions(&mut m);
        self.get_keyword_completions(&mut m);
        self.plmod.get_ns_completions_pri(&mut m);

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
                item.detail = Some(v.tp.to_string());
                m.insert(k.clone(), item);
            }
        }
    }

    fn get_type_completions_in_ns(&self, ns: &str, m: &mut FxHashMap<String, CompletionItem>) {
        let ns = self.plmod.submods.get(ns);
        if let Some(ns) = ns {
            for (k, v) in ns.types.iter() {
                let tp = match v {
                    PLType::STRUCT(_) => CompletionItemKind::STRUCT,
                    PLType::FN(_) => CompletionItemKind::FUNCTION,
                    _ => continue, // skip completion for primary types
                };
                let mut item = CompletionItem {
                    label: k.to_string(),
                    kind: Some(tp),
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
            let tp = match f {
                PLType::FN(_) => continue,
                PLType::STRUCT(_) => CompletionItemKind::STRUCT,
                PLType::PRIMITIVE(_) => CompletionItemKind::KEYWORD,
                PLType::VOID => CompletionItemKind::KEYWORD,
                PLType::NAMESPACE(_) => todo!(),
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
            let tp = match f {
                PLType::FN(_) => CompletionItemKind::FUNCTION,
                PLType::STRUCT(_) => CompletionItemKind::STRUCT,
                PLType::PRIMITIVE(_) => CompletionItemKind::KEYWORD,
                PLType::VOID => CompletionItemKind::KEYWORD,
                PLType::NAMESPACE(_) => todo!(),
            };
            vmap.insert(
                k.to_string(),
                CompletionItem {
                    label: k.to_string(),
                    kind: Some(tp),
                    ..Default::default()
                },
            );
        }
        if let Some(father) = self.father {
            father.get_pltp_completions(vmap);
        }
    }

    pub fn push_semantic_token(&self, range: Range, tp: SemanticTokenType, modifiers: u32) {
        self.semantic_tokens_builder.borrow_mut().push(
            range.to_diag_range(),
            type_index(tp),
            modifiers,
        )
    }

    fn get_keyword_completions(&self, vmap: &mut FxHashMap<String, CompletionItem>) {
        let keywords = vec![
            "if", "else", "while", "for", "return", "struct", "let", "true", "false",
        ];
        let loopkeys = vec!["break", "continue"];
        let toplevel = vec!["fn", "struct", "const", "use"];
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
}
