use crate::lsp::semantic_tokens::type_index;
use crate::lsp::semantic_tokens::SemanticTokensBuilder;
use crate::utils::read_config::Config;
use colored::Colorize;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::debug_info::*;
use inkwell::module::FlagBehavior;
use inkwell::module::Linkage;
use inkwell::module::Module;
use inkwell::targets::TargetMachine;
use inkwell::types::ArrayType;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::types::FunctionType;
use inkwell::types::StructType;
use inkwell::types::VoidType;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use inkwell::values::PointerValue;
use inkwell::values::{AnyValueEnum, BasicMetadataValueEnum};
use inkwell::AddressSpace;
use lsp_types::CompletionItem;
use lsp_types::CompletionItemKind;
use lsp_types::Diagnostic;
use lsp_types::DiagnosticSeverity;
use lsp_types::GotoDefinitionResponse;
use lsp_types::Hover;
use lsp_types::HoverContents;
use lsp_types::InlayHint;
use lsp_types::InlayHintKind;
use lsp_types::InsertTextFormat;
use lsp_types::Location;
use lsp_types::MarkedString;
use lsp_types::SemanticTokenType;
use lsp_types::Url;
use rustc_hash::FxHashMap;
use rustc_hash::FxHashSet;
use std::cell::Cell;
use std::cell::RefCell;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;

use super::compiler::get_target_machine;
use super::compiler::ActionType;
use super::diag::{ErrorCode, WarnCode};
use super::diag::{ERR_MSG, WARN_MSG};
use super::node::NodeEnum;
use super::node::PLValue;
use super::node::TypeNode;
use super::node::TypeNodeEnum;
use super::range::Pos;
use super::range::Range;
// TODO: match all case
// const DW_ATE_UTF: u32 = 0x10;
const DW_ATE_BOOLEAN: u32 = 0x02;
const DW_ATE_FLOAT: u32 = 0x04;
const DW_ATE_SIGNED: u32 = 0x05;
const DW_ATE_UNSIGNED: u32 = 0x07;
const DW_TAG_REFERENCE_TYPE: u32 = 16;
fn get_dw_ate_encoding<'a, 'ctx>(pritp: &PriType) -> u32 {
    match pritp {
        PriType::I8 | PriType::I16 | PriType::I32 | PriType::I64 | PriType::I128 => DW_ATE_SIGNED,
        PriType::U8 | PriType::U16 | PriType::U32 | PriType::U64 | PriType::U128 => DW_ATE_UNSIGNED,
        PriType::F32 | PriType::F64 => DW_ATE_FLOAT,
        PriType::BOOL => DW_ATE_BOOLEAN,
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
    pub ditypes_placeholder: Rc<RefCell<FxHashMap<String, RefCell<Vec<MemberType<'ctx>>>>>>, // hold the generated debug info type place holder
    pub ditypes: Rc<RefCell<FxHashMap<String, DIType<'ctx>>>>, // hold the generated debug info type
    pub hints: Rc<RefCell<Box<Vec<InlayHint>>>>,
    pub semantic_tokens_builder: Rc<RefCell<Box<SemanticTokensBuilder>>>, // semantic token builder
    pub goto_def: Rc<Cell<Option<GotoDefinitionResponse>>>, // hold the goto definition result
    pub completion_items: Rc<Cell<Vec<CompletionItem>>>,    // hold the completion items
    pub hover: Rc<Cell<Option<Hover>>>,                     // hold the hover result
    pub init_func: Option<FunctionValue<'ctx>>,             //init function,call first in main
    pub table: FxHashMap<
        String,
        (
            PointerValue<'ctx>,
            PLType,
            Range,
            Rc<RefCell<Vec<Location>>>,
        ),
    >, // variable table
    pub config: Config,                                     // config
    pub roots: RefCell<Vec<BasicValueEnum<'ctx>>>,
    pub usegc: bool,
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
    pub tp: PLType,
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
        tp: PLType,
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

    pub fn replace_type(&mut self, name: &str, tp: PLType) {
        self.types.insert(name.to_string(), tp);
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
    ARR(ARRType),
    PRIMITIVE(PriType),
    VOID,
    POINTER(Box<PLType>),
}

/// # PriType
/// Primitive type for pivot-lang
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PriType {
    // basetype: BasicTypeEnum<'ctx>,
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
    BOOL,
}
impl PriType {
    pub fn get_basic_type<'a, 'ctx>(&self, ctx: &Ctx<'a, 'ctx>) -> BasicTypeEnum<'ctx> {
        match self {
            PriType::I8 => ctx.context.i8_type().into(),
            PriType::I16 => ctx.context.i16_type().into(),
            PriType::I32 => ctx.context.i32_type().into(),
            PriType::I64 => ctx.context.i64_type().as_basic_type_enum(),
            PriType::I128 => ctx.context.i128_type().as_basic_type_enum(),
            PriType::U8 => ctx.context.i8_type().as_basic_type_enum(),
            PriType::U16 => ctx.context.i16_type().as_basic_type_enum(),
            PriType::U32 => ctx.context.i32_type().as_basic_type_enum(),
            PriType::U64 => ctx.context.i64_type().as_basic_type_enum(),
            PriType::U128 => ctx.context.i128_type().as_basic_type_enum(),
            PriType::F32 => ctx.context.f32_type().as_basic_type_enum(),
            PriType::F64 => ctx.context.f64_type().as_basic_type_enum(),
            PriType::BOOL => ctx.context.i8_type().as_basic_type_enum(),
        }
    }
    pub fn get_name(&self) -> String {
        match self {
            PriType::I8 => "i8".to_string(),
            PriType::I16 => "i16".to_string(),
            PriType::I32 => "i32".to_string(),
            PriType::I64 => String::from("i64"),
            PriType::I128 => String::from("i128"),
            PriType::U8 => String::from("u8"),
            PriType::U16 => String::from("u16"),
            PriType::U32 => String::from("u32"),
            PriType::U64 => String::from("u64"),
            PriType::U128 => String::from("u128"),
            PriType::F32 => String::from("f32"),
            PriType::F64 => String::from("f64"),
            PriType::BOOL => String::from("bool"),
        }
    }
    pub fn try_from_str(str: &str) -> Option<Self> {
        match str {
            "i8" => Some(PriType::I8),
            "i16" => Some(PriType::I16),
            "i32" => Some(PriType::I32),
            "i64" => Some(PriType::I64),
            "i128" => Some(PriType::I128),
            "u8" => Some(PriType::U8),
            "u16" => Some(PriType::U16),
            "u32" => Some(PriType::U32),
            "u64" => Some(PriType::U64),
            "u128" => Some(PriType::U128),
            "f32" => Some(PriType::F32),
            "f64" => Some(PriType::F64),
            "bool" => Some(PriType::BOOL),
            _ => None,
        }
    }
}

impl PLType {
    pub fn is(self, pri_type: PriType) -> bool {
        if let PLType::PRIMITIVE(pri) = self {
            pri == pri_type
        } else {
            false
        }
    }
    /// # get_refs
    /// get the references of the type
    /// used in find references
    /// void type and primitive types has no references
    pub fn get_refs(&self) -> Option<Rc<RefCell<Vec<Location>>>> {
        match self {
            PLType::FN(f) => Some(f.refs.clone()),
            PLType::STRUCT(s) => Some(s.refs.clone()),
            PLType::ARR(_) => None,
            PLType::PRIMITIVE(_) => None,
            PLType::VOID => None,
            PLType::POINTER(_) => None,
        }
    }

    /// # get_basic_type
    /// get the basic type of the type
    /// used in code generation
    /// may panic if the type is void type
    pub fn get_basic_type<'a, 'ctx>(&self, ctx: &Ctx<'a, 'ctx>) -> BasicTypeEnum<'ctx> {
        self.get_basic_type_op(ctx).unwrap()
    }

    pub fn get_name<'a, 'ctx>(&self) -> String {
        match self {
            PLType::FN(fu) => fu.name.clone(),
            PLType::STRUCT(st) => st.name.clone(),
            PLType::PRIMITIVE(pri) => pri.get_name(),
            PLType::ARR(arr) => format!("[{} * {}]", arr.element_type.get_name(), arr.size),
            PLType::VOID => "void".to_string(),
            PLType::POINTER(p) => "&".to_string() + &p.get_name(),
        }
    }

    pub fn get_full_elm_name<'a, 'ctx>(&self) -> String {
        match self {
            PLType::FN(fu) => fu.name.clone(),
            PLType::STRUCT(st) => st.get_st_full_name(),
            PLType::PRIMITIVE(pri) => pri.get_name(),
            PLType::ARR(arr) => {
                format!("[{} * {}]", arr.element_type.get_full_elm_name(), arr.size)
            }
            PLType::VOID => "void".to_string(),
            PLType::POINTER(p) => p.get_full_elm_name(),
        }
    }
    pub fn get_ptr_depth(&self) -> usize {
        match self {
            PLType::POINTER(p) => p.get_ptr_depth() + 1,
            _ => 0,
        }
    }

    /// # get_range
    /// get the defination range of the type
    pub fn get_range(&self) -> Option<Range> {
        match self {
            PLType::FN(f) => Some(f.range.clone()),
            PLType::STRUCT(s) => Some(s.range.clone()),
            PLType::ARR(_) => None,
            PLType::PRIMITIVE(_) => None,
            PLType::VOID => None,
            PLType::POINTER(_) => None,
        }
    }

    /// # get_basic_type_op
    /// get the basic type of the type
    /// used in code generation
    pub fn get_basic_type_op<'a, 'ctx>(&self, ctx: &Ctx<'a, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        match self {
            PLType::FN(f) => Some(
                f.get_or_insert_fn(ctx)
                    .get_type()
                    .ptr_type(inkwell::AddressSpace::Global)
                    .as_basic_type_enum(),
            ),
            PLType::STRUCT(s) => Some(s.struct_type(&ctx).as_basic_type_enum()),
            PLType::ARR(a) => Some(a.arr_type(ctx).as_basic_type_enum()),
            PLType::PRIMITIVE(t) => Some(t.get_basic_type(ctx)),
            PLType::VOID => None,
            PLType::POINTER(p) => Some(
                p.get_basic_type(ctx)
                    .ptr_type(AddressSpace::Generic)
                    .as_basic_type_enum(),
            ),
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
            PLType::ARR(arr) => {
                let elemdi = arr.element_type.get_ditype(ctx)?;
                let etp = &arr.element_type.get_basic_type(ctx);
                let size = td.get_bit_size(etp) * arr.size as u64;
                let align = td.get_preferred_alignment(etp);
                Some(
                    ctx.dibuilder
                        .create_array_type(elemdi, size, align, &[(0..arr.size as i64)])
                        .as_type(),
                )
            }
            PLType::STRUCT(x) => {
                if ctx.ditypes.borrow().contains_key(&x.get_st_full_name()) {
                    return Some(
                        ctx.ditypes
                            .borrow()
                            .get(&x.get_st_full_name())
                            .unwrap()
                            .clone(),
                    );
                }
                let mut offset = 0;
                ctx.ditypes_placeholder
                    .borrow_mut()
                    .insert(x.get_st_full_name(), RefCell::new(vec![]));
                let m = x
                    .ordered_fields
                    .iter()
                    .map(|v| {
                        let (tp, off) = v.get_di_type(ctx, offset);
                        offset = off;
                        tp
                    })
                    .collect::<Vec<_>>();
                let sttp = x.struct_type(ctx);
                let st = ctx
                    .dibuilder
                    .create_struct_type(
                        ctx.discope,
                        &x.name,
                        ctx.diunit.get_file(),
                        x.range.start.line as u32 + 1,
                        td.get_bit_size(&sttp),
                        td.get_abi_alignment(&sttp),
                        DIFlags::PUBLIC,
                        None,
                        &m,
                        0,
                        None,
                        &x.name,
                    )
                    .as_type();
                let members = ctx
                    .ditypes_placeholder
                    .borrow_mut()
                    .remove(&x.get_st_full_name())
                    .unwrap();
                for m in members.borrow().iter() {
                    let mut elemdi = st;
                    for _ in 0..m.ptr_depth {
                        elemdi = ctx
                            .dibuilder
                            .create_pointer_type(
                                "",
                                elemdi,
                                td.get_bit_size(
                                    &sttp.ptr_type(AddressSpace::Generic).as_basic_type_enum(),
                                ),
                                td.get_preferred_alignment(
                                    &sttp.ptr_type(AddressSpace::Generic).as_basic_type_enum(),
                                ),
                                AddressSpace::Generic,
                            )
                            .as_type();
                    }

                    let realtp = ctx.dibuilder.create_member_type(
                        m.scope,
                        &m.name,
                        m.di_file,
                        m.line,
                        elemdi.get_size_in_bits(),
                        elemdi.get_align_in_bits(),
                        m.offset,
                        DIFlags::PUBLIC,
                        elemdi,
                    );
                    unsafe {
                        ctx.dibuilder
                            .replace_placeholder_derived_type(m.ditype, realtp);
                    }
                }
                ctx.ditypes.borrow_mut().insert(x.get_st_full_name(), st);
                return Some(st);
            }
            PLType::PRIMITIVE(pt) => {
                return Some(
                    ctx.dibuilder
                        .create_basic_type(
                            &pt.get_name(),
                            td.get_bit_size(&self.get_basic_type(ctx)),
                            get_dw_ate_encoding(pt),
                            0,
                        )
                        .unwrap()
                        .as_type(),
                );
            }
            PLType::VOID => None,
            PLType::POINTER(p) => {
                let elemdi = p.get_ditype(ctx)?;
                let etp = &p
                    .get_basic_type(ctx)
                    .ptr_type(AddressSpace::Generic)
                    .as_basic_type_enum();
                let size = td.get_bit_size(etp);
                let align = td.get_preferred_alignment(etp);
                Some(
                    ctx.dibuilder
                        .create_pointer_type("", elemdi, size, align, AddressSpace::Generic)
                        .as_type(),
                )
            }
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
    pub pltype: Box<TypeNodeEnum>,
    pub name: String,
    pub range: Range,
    pub refs: Rc<RefCell<Vec<Location>>>,
}

impl Field {
    pub fn get_di_type<'a, 'ctx>(&self, ctx: &Ctx<'a, 'ctx>, offset: u64) -> (DIType<'ctx>, u64) {
        let pltp = self.pltype.get_type(ctx).unwrap();
        let depth = pltp.get_ptr_depth();
        if let Some(x) = ctx
            .ditypes_placeholder
            .borrow_mut()
            .get(&pltp.get_full_elm_name())
        {
            if !matches!(pltp, PLType::POINTER(_)) {
                ctx.add_err(self.range, ErrorCode::ILLEGAL_SELF_RECURSION);
            }
            let placeholder = unsafe { ctx.dibuilder.create_placeholder_derived_type(ctx.context) };
            let td = ctx.targetmachine.get_target_data();
            let etp = ctx.context.i8_type().ptr_type(AddressSpace::Generic);
            let size = td.get_bit_size(&etp);
            x.borrow_mut().push(MemberType {
                ditype: placeholder,
                offset,
                scope: ctx.discope,
                line: self.range.start.line as u32,
                name: self.name.clone(),
                di_file: ctx.diunit.get_file(),
                ptr_depth: depth,
            });
            return (placeholder.as_type(), offset + size);
        }
        let di_type = pltp.get_ditype(ctx);
        let debug_type = di_type.unwrap();
        (
            ctx.dibuilder
                .create_member_type(
                    ctx.discope,
                    &self.name,
                    ctx.diunit.get_file(),
                    self.range.start.line as u32,
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
    pub name: String,     // name for lsp
    pub llvmname: String, // name in llvm ir
    pub param_pltypes: Vec<PLType>,
    pub param_name: Vec<String>,
    pub ret_pltype: Box<PLType>,
    pub range: Range,
    pub refs: Rc<RefCell<Vec<Location>>>,
    pub doc: Vec<Box<NodeEnum>>,
}

impl TryFrom<PLType> for FNType {
    type Error = ();

    fn try_from(value: PLType) -> Result<Self, Self::Error> {
        match value {
            PLType::FN(x) => Ok(x),
            _ => Err(()),
        }
    }
}

impl FNType {
    /// try get function value from module
    ///
    /// if not found, create a declaration
    pub fn get_or_insert_fn<'a, 'ctx>(&self, ctx: &Ctx<'a, 'ctx>) -> FunctionValue<'ctx> {
        if let Some(v) = ctx.module.get_function(&self.llvmname) {
            return v;
        }
        let mut param_types = vec![];
        for param_pltype in self.param_pltypes.iter() {
            param_types.push(param_pltype.get_basic_type(ctx).into());
        }
        let fn_type = self
            .ret_pltype
            .get_ret_type(ctx)
            .fn_type(&param_types, false);
        let fn_value = ctx
            .module
            .add_function(&self.llvmname, fn_type, Some(Linkage::External));
        fn_value
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ARRType {
    pub element_type: Box<PLType>,
    pub size: u32,
}

impl ARRType {
    pub fn arr_type<'a, 'ctx>(&'a self, ctx: &Ctx<'a, 'ctx>) -> ArrayType<'ctx> {
        self.element_type.get_basic_type(ctx).array_type(self.size)
    }
    pub fn get_elem_type<'a, 'ctx>(&'a self) -> Box<PLType> {
        self.element_type.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct STType {
    pub name: String,
    pub path: String,
    pub fields: FxHashMap<String, Field>,
    pub ordered_fields: Vec<Field>,
    pub range: Range,
    pub refs: Rc<RefCell<Vec<Location>>>,
    pub doc: Vec<Box<NodeEnum>>,
    pub methods: FxHashMap<String, FNType>,
}

impl STType {
    pub fn struct_type<'a, 'ctx>(&'a self, ctx: &Ctx<'a, 'ctx>) -> StructType<'ctx> {
        let st = ctx.module.get_struct_type(&self.get_st_full_name());
        if let Some(st) = st {
            return st;
        }
        let st = ctx.context.opaque_struct_type(&self.get_st_full_name());
        st.set_body(
            &self
                .ordered_fields
                .clone()
                .into_iter()
                .map(|order_field| {
                    order_field
                        .pltype
                        .get_type(ctx)
                        .unwrap()
                        .get_basic_type(&ctx)
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
        for (name, _) in &self.methods {
            completions.push(CompletionItem {
                kind: Some(CompletionItemKind::METHOD),
                label: name.clone(),
                detail: Some("method".to_string()),
                insert_text: Some(name.clone()),
                insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
                ..Default::default()
            });
        }
        completions
    }
    pub fn get_st_full_name(&self) -> String {
        format!("{}..{}", self.path, self.name)
    }
}

fn add_primitive_types<'a, 'ctx>(ctx: &mut Ctx<'a, 'ctx>) {
    let pltype_i128 = PLType::PRIMITIVE(PriType::I128);
    ctx.plmod
        .types
        .insert("i128".to_string(), pltype_i128.clone());

    let pltype_i64 = PLType::PRIMITIVE(PriType::I64);
    ctx.plmod
        .types
        .insert("i64".to_string(), pltype_i64.clone());

    let pltype_i32 = PLType::PRIMITIVE(PriType::I32);
    ctx.plmod
        .types
        .insert("i32".to_string(), pltype_i32.clone());

    let pltype_i16 = PLType::PRIMITIVE(PriType::I16);
    ctx.plmod
        .types
        .insert("i16".to_string(), pltype_i16.clone());

    let pltype_i8 = PLType::PRIMITIVE(PriType::I8);
    ctx.plmod.types.insert("i8".to_string(), pltype_i8.clone());

    let pltype_u128 = PLType::PRIMITIVE(PriType::U128);
    ctx.plmod
        .types
        .insert("u128".to_string(), pltype_u128.clone());

    let pltype_u64 = PLType::PRIMITIVE(PriType::U64);
    ctx.plmod
        .types
        .insert("u64".to_string(), pltype_u64.clone());

    let pltype_u32 = PLType::PRIMITIVE(PriType::U32);
    ctx.plmod
        .types
        .insert("u32".to_string(), pltype_u32.clone());

    let pltype_u16 = PLType::PRIMITIVE(PriType::U16);
    ctx.plmod
        .types
        .insert("u16".to_string(), pltype_u16.clone());

    let pltype_u8 = PLType::PRIMITIVE(PriType::U8);
    ctx.plmod.types.insert("u8".to_string(), pltype_u8.clone());

    let pltype_f64 = PLType::PRIMITIVE(PriType::F64);
    ctx.plmod
        .types
        .insert("f64".to_string(), pltype_f64.clone());

    let pltype_f32 = PLType::PRIMITIVE(PriType::F32);
    ctx.plmod
        .types
        .insert("f32".to_string(), pltype_f32.clone());

    let pltype_bool = PLType::PRIMITIVE(PriType::BOOL);
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
        config: Config,
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
            hints: Rc::new(RefCell::new(Box::new(vec![]))),
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
            hints: self.hints.clone(),
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
        PLType,
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
        tp: PLType,
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
    pub fn get_or_add_global(&self, name: &str, m: &Mod, pltype: PLType) -> PointerValue<'ctx> {
        let global = self.module.get_global(&m.get_full_name(name));
        if global.is_none() {
            let global =
                self.module
                    .add_global(pltype.get_basic_type(self), None, &m.get_full_name(name));
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

    pub fn add_type(&mut self, name: String, tp: PLType, range: Range) -> Result<(), PLDiag> {
        if self.plmod.types.contains_key(&name) {
            return Err(self.add_err(range, ErrorCode::REDEFINE_TYPE));
        }
        self.send_if_go_to_def(range, range, self.plmod.path.clone());
        self.plmod.types.insert(name, tp.clone());
        Ok(())
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
                item.detail = Some(v.tp.get_name());
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
                PLType::ARR(_) => continue,
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
            let tp = match f {
                PLType::FN(_) => CompletionItemKind::FUNCTION,
                PLType::STRUCT(_) => CompletionItemKind::STRUCT,
                PLType::ARR(_) => CompletionItemKind::KEYWORD,
                PLType::PRIMITIVE(_) => CompletionItemKind::KEYWORD,
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
    pub fn push_type_hints(&self, range: Range, pltype: &PLType) {
        let hint = InlayHint {
            position: range.to_diag_range().end,
            label: lsp_types::InlayHintLabel::String(": ".to_string() + &pltype.get_name()),
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
    /// # auto_deref
    /// 自动解引用，有几层解几层
    pub fn auto_deref(
        &self,
        tp: PLType,
        value: PointerValue<'ctx>,
    ) -> (PLType, PointerValue<'ctx>) {
        let mut tp = tp;
        let mut value = value;
        loop {
            match tp {
                PLType::POINTER(p) => {
                    tp = *p;
                    value = self.builder.build_load(value, "load").into_pointer_value();
                }
                _ => break,
            }
        }
        (tp, value)
    }

    /// # auto_deref_tp
    /// 自动解pltype引用，有几层解几层
    pub fn auto_deref_tp(&self, tp: PLType) -> PLType {
        let mut tp = tp;
        loop {
            match tp {
                PLType::POINTER(p) => {
                    tp = *p;
                }
                _ => break,
            }
        }
        tp
    }
}
