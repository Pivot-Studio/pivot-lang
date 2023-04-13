use super::ctx::Ctx;
use super::diag::ErrorCode;
use super::plmod::Mod;
use super::tokens::TokenType;
use crate::add_basic_types;
use crate::ast::builder::IRBuilder;

use crate::ast::builder::BuilderEnum;
use crate::format_label;
use crate::generic_impl;
use crate::if_not_modified_by;
use crate::skip_if_not_modified_by;
use crate::utils::get_hash_code;

use super::diag::PLDiag;

use super::fmt::FmtBuilder;
use super::node::function::FuncDefNode;
use super::node::pkg::ExternIdNode;
use super::node::primary::NumNode;
use super::node::primary::VarNode;
use super::node::types::ArrayTypeNameNode;
use super::node::types::PointerTypeNode;
use super::node::types::TypeNameNode;
use super::node::NodeEnum;
use super::node::Num;
use super::node::TypeNode;
use super::node::TypeNodeEnum;
use super::range::Range;
use immix::ObjectType;
use indexmap::IndexMap;

use inkwell::types::BasicMetadataTypeEnum;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::types::FunctionType;

use inkwell::types::VoidType;

use lsp_types::Command;
use lsp_types::CompletionItem;
use lsp_types::CompletionItemKind;
use lsp_types::DocumentSymbol;
use lsp_types::InsertTextFormat;

use lsp_types::SymbolKind;
use rustc_hash::FxHashMap;
use std::cell::RefCell;

use std::sync::Arc;

/// # PLType
/// Type for pivot-lang
/// including primitive type, struct type, function type, void type
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PLType {
    Fn(FNValue),
    Struct(STType),
    Arr(ARRType),
    Primitive(PriType),
    Void,
    Pointer(Arc<RefCell<PLType>>),
    Generic(GenericType),
    PlaceHolder(PlaceHolderType),
    Trait(STType),
    Union(UnionType),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnionType {
    pub name: String,
    pub generic_map: IndexMap<String, Arc<RefCell<PLType>>>,
    pub sum_types: Vec<Box<TypeNodeEnum>>,
    pub path: String,
    pub modifier: Option<(TokenType, Range)>,
    pub range: Range,
}

impl UnionType {
    pub fn find_method(&self, ctx: &Ctx, method: &str) -> Option<FNValue> {
        ctx.plmod
            .find_method(&self.get_full_name_except_generic(), method)
    }
    pub fn get_full_name_except_generic(&self) -> String {
        let full_name = self.get_full_name();
        full_name.split('<').collect::<Vec<_>>()[0].to_string()
    }
    pub fn get_full_name(&self) -> String {
        format!("{}..{}", self.path, self.name)
    }
    pub fn append_name_with_generic(&self) -> String {
        let typeinfer = self
            .generic_map
            .iter()
            .map(|(_, v)| match &*v.clone().borrow() {
                PLType::Generic(g) => g.curpltype.as_ref().unwrap().borrow().get_name(),
                _ => unreachable!(),
            })
            .collect::<Vec<_>>()
            .join(", ");
        format!("{}<{}>", self.name, typeinfer)
    }
    pub fn gen_code<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<UnionType, PLDiag> {
        let name = self.append_name_with_generic();
        if let Ok(pltype) = ctx.get_type(&name, Default::default()) {
            match &*pltype.borrow() {
                PLType::Union(st) => {
                    return Ok(st.clone());
                }
                _ => unreachable!(),
            }
        }
        let mut res = self.clone();
        res.name = name;
        ctx.add_type_without_check(Arc::new(RefCell::new(PLType::Union(res.clone()))));

        let (tps, errors): (Vec<_>, Vec<_>) = res
            .sum_types
            .iter()
            .map(|t| {
                Ok::<Box<TypeNodeEnum>, PLDiag>(t.get_type(ctx, builder)?.borrow().get_typenode())
            })
            .partition(Result::is_ok);
        if !errors.is_empty() {
            return Err(errors[0].clone().unwrap_err());
        }
        res.sum_types = tps.into_iter().map(Result::unwrap).collect::<Vec<_>>();
        res.generic_map.clear();
        let pltype = ctx.get_type(&res.name, Default::default()).unwrap();
        pltype.replace(PLType::Union(res.clone()));
        Ok(res)
    }
    pub fn has_type<'a, 'ctx, 'b>(
        &self,
        pltype: &PLType,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Option<usize> {
        ctx.run_in_union_mod(self, |ctx, u| {
            Ok(u.sum_types
                .iter()
                .enumerate()
                .find(|(_, t)| &*t.get_type(ctx, builder).unwrap().borrow() == pltype))
            .map(|x| x.map(|(i, _)| i))
        })
        .unwrap()
    }
}
/// # PriType
/// Primitive type for pivot-lang
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::upper_case_acronyms)]
pub enum PriType {
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
    pub fn signed(&self) -> bool {
        matches!(
            self,
            PriType::I8 | PriType::I16 | PriType::I32 | PriType::I64 | PriType::I128
        )
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
fn new_typename_node(name: &str, range: Range) -> Box<TypeNodeEnum> {
    Box::new(TypeNodeEnum::Basic(TypeNameNode {
        id: Some(ExternIdNode {
            ns: vec![],
            id: Box::new(VarNode {
                name: name.to_string(),
                range,
            }),
            complete: true,
            singlecolon: false,
            range,
        }),
        generic_params: None,
        range,
    }))
}
fn new_arrtype_node(typenode: Box<TypeNodeEnum>, size: u64) -> Box<TypeNodeEnum> {
    Box::new(TypeNodeEnum::Array(ArrayTypeNameNode {
        id: typenode,
        size: Box::new(NodeEnum::Num(NumNode {
            value: Num::Int(size),
            range: Default::default(),
        })),
        range: Default::default(),
    }))
}
fn new_ptrtype_node(typenode: Box<TypeNodeEnum>) -> Box<TypeNodeEnum> {
    Box::new(TypeNodeEnum::Pointer(PointerTypeNode {
        elm: typenode,
        range: Default::default(),
    }))
}
pub fn get_type_deep(pltype: Arc<RefCell<PLType>>) -> Arc<RefCell<PLType>> {
    match &*pltype.borrow() {
        PLType::Generic(g) => {
            if g.curpltype.is_some() {
                g.curpltype.as_ref().unwrap().clone()
            } else {
                pltype.clone()
            }
        }
        _ => pltype.clone(),
    }
}

fn expect_pub_err(err: ErrorCode, ctx: &Ctx, range: Range, name: String) -> Result<(), PLDiag> {
    Err(PLDiag::new_error(range, err)
        .add_label(
            range,
            ctx.get_file(),
            format_label!("{} is not public", name),
        )
        .add_help("try add `pub` modifier before it")
        .add_to_ctx(ctx))
}
impl PLType {
    pub fn get_immix_type(&self) -> ObjectType {
        match self {
            PLType::Struct(_) | PLType::Arr(_) => ObjectType::Complex,
            PLType::Pointer(_) => ObjectType::Pointer,
            PLType::Trait(_) => ObjectType::Trait,
            PLType::Union(_) => ObjectType::Trait, // share same layout as trait
            _ => ObjectType::Atomic,
        }
    }

    pub fn get_kind_name(&self) -> String {
        match self {
            PLType::Primitive(_) | PLType::Void => "primitive".to_string(),
            PLType::Pointer(_) => "pointer".to_string(),
            PLType::Arr(_) => "array".to_string(),
            PLType::Struct(_) => "struct".to_string(),
            PLType::Fn(_) => "function".to_string(),
            PLType::PlaceHolder(_) => "placeholder".to_string(),
            PLType::Generic(_) => "generic".to_string(),
            PLType::Trait(_) => "trait".to_string(),
            PLType::Union(_) => "union".to_string(),
        }
    }
    pub fn get_typenode(&self) -> Box<TypeNodeEnum> {
        match self {
            PLType::Struct(st) => new_typename_node(&st.name, st.range),
            PLType::Arr(arr) => {
                new_arrtype_node(arr.get_elem_type().borrow().get_typenode(), arr.size as u64)
            }
            PLType::Primitive(p) => new_typename_node(&p.get_name(), Default::default()),
            PLType::Void => new_typename_node("void", Default::default()),
            PLType::Pointer(p) => new_ptrtype_node(p.borrow().get_typenode()),
            PLType::Generic(g) => {
                if g.curpltype.is_some() {
                    g.curpltype.as_ref().unwrap().borrow().get_typenode()
                } else {
                    new_typename_node(&g.name, Default::default())
                }
            }
            PLType::PlaceHolder(p) => {
                new_typename_node(&p.get_place_holder_name(), Default::default())
            }
            PLType::Trait(t) => new_typename_node(&t.name, t.range),
            PLType::Fn(_) => unreachable!(),
            PLType::Union(u) => new_typename_node(&u.name, u.range),
        }
    }
    pub fn is(&self, pri_type: &PriType) -> bool {
        if let PLType::Primitive(pri) = self {
            pri == pri_type
        } else {
            false
        }
    }
    /// # if_refs
    /// if support find refs
    pub fn if_refs(&self, f: impl FnOnce(&PLType)) {
        match self {
            PLType::Fn(_) | PLType::Struct(_) | PLType::Trait(_) | PLType::Union(_) => f(self),
            PLType::Arr(_) => (),
            PLType::Primitive(_) => (),
            PLType::Void => (),
            PLType::Pointer(_) => (),
            PLType::Generic(_) => (),
            PLType::PlaceHolder(_) => (),
        }
    }

    pub fn get_name(&self) -> String {
        match self {
            PLType::Fn(fu) => fu.name.clone(),
            PLType::Struct(st) => st.name.clone(),
            PLType::Primitive(pri) => pri.get_name(),
            PLType::Arr(arr) => {
                format!("[{} * {}]", arr.element_type.borrow().get_name(), arr.size)
            }
            PLType::Void => "void".to_string(),
            PLType::Pointer(p) => "*".to_string() + &p.borrow().get_name(),
            PLType::Generic(g) => {
                if g.curpltype.is_some() {
                    g.curpltype.as_ref().unwrap().borrow().get_name()
                } else {
                    g.name.clone()
                }
            }
            PLType::PlaceHolder(p) => p.name.clone(),
            PLType::Trait(t) => t.name.clone(),
            PLType::Union(u) => u.name.clone(),
        }
    }
    pub fn get_llvm_name(&self) -> String {
        match self {
            PLType::Fn(fu) => fu.name.clone(),
            PLType::Struct(st) => st.name.clone(),
            PLType::Trait(t) => t.name.clone(),
            PLType::Primitive(pri) => pri.get_name(),
            PLType::Arr(arr) => {
                format!("[{} * {}]", arr.element_type.borrow().get_name(), arr.size)
            }
            PLType::Void => "void".to_string(),
            PLType::Pointer(p) => "*".to_string() + &p.borrow().get_name(),
            PLType::Generic(g) => {
                if g.curpltype.is_some() {
                    g.curpltype.as_ref().unwrap().borrow().get_name()
                } else {
                    g.name.clone()
                }
            }
            PLType::PlaceHolder(p) => p.get_place_holder_name(),
            PLType::Union(u) => u.name.clone(),
        }
    }

    pub fn get_full_elm_name(&self) -> String {
        match self {
            PLType::Generic(g) => g.name.clone(),
            PLType::Fn(fu) => fu.llvmname.clone(),
            PLType::Struct(st) => st.get_st_full_name(),
            PLType::Trait(st) => st.get_st_full_name(),
            PLType::Primitive(pri) => pri.get_name(),
            PLType::Arr(arr) => {
                format!(
                    "[{} * {}]",
                    arr.element_type.borrow().get_full_elm_name(),
                    arr.size
                )
            }
            PLType::Void => "void".to_string(),
            PLType::Pointer(p) => p.borrow().get_full_elm_name(),
            PLType::PlaceHolder(p) => p.name.clone(),
            PLType::Union(u) => u.get_full_name(),
        }
    }
    pub fn get_ptr_depth(&self) -> usize {
        match self {
            PLType::Pointer(p) => p.borrow().get_ptr_depth() + 1,
            _ => 0,
        }
    }

    pub fn expect_pub(&self, ctx: &Ctx, range: Range) -> Result<(), PLDiag> {
        match self {
            PLType::Fn(f) => f.expect_pub(ctx, range),
            PLType::Struct(s) => {
                if s.path == ctx.plmod.path {
                    return Ok(());
                }
                if_not_modified_by!(
                    s.modifier,
                    TokenType::PUB,
                    return expect_pub_err(
                        super::diag::ErrorCode::EXPECT_PUBLIC_STRUCT,
                        ctx,
                        range,
                        s.name.clone()
                    )
                );
                Ok(())
            }
            PLType::Trait(st) => {
                if st.path == ctx.plmod.path {
                    return Ok(());
                }
                if_not_modified_by!(
                    st.modifier,
                    TokenType::PUB,
                    return expect_pub_err(
                        super::diag::ErrorCode::EXPECT_PUBLIC_TRAIT,
                        ctx,
                        range,
                        st.name.clone()
                    )
                );
                Ok(())
            }
            PLType::Union(st) => {
                if st.path == ctx.plmod.path {
                    return Ok(());
                }
                if_not_modified_by!(
                    st.modifier,
                    TokenType::PUB,
                    return expect_pub_err(
                        super::diag::ErrorCode::EXPECT_PUBLIC_UNION,
                        ctx,
                        range,
                        st.name.clone()
                    )
                );
                Ok(())
            }
            _ => Ok(()),
        }
    }

    /// # get_range
    /// get the defination range of the type
    pub fn get_range(&self) -> Option<Range> {
        match self {
            PLType::Generic(g) => Some(g.range),
            PLType::Fn(f) => Some(f.range),
            PLType::Struct(s) => Some(s.range),
            PLType::Arr(_) => None,
            PLType::Primitive(_) => None,
            PLType::Void => None,
            PLType::Pointer(_) => None,
            PLType::PlaceHolder(p) => Some(p.range),
            PLType::Trait(t) => Some(t.range),
            PLType::Union(u) => Some(u.range),
        }
    }

    pub fn is_void(&self) -> bool {
        matches!(self, PLType::Void)
    }
}

pub enum RetTypeEnum<'ctx> {
    Void(VoidType<'ctx>),
    Basic(BasicTypeEnum<'ctx>),
}

impl<'ctx> RetTypeEnum<'ctx> {
    pub fn fn_type(
        &self,
        param_types: &[BasicMetadataTypeEnum<'ctx>],
        is_var_args: bool,
    ) -> FunctionType<'ctx> {
        match self {
            RetTypeEnum::Void(t) => t.fn_type(param_types, is_var_args),
            RetTypeEnum::Basic(t) => t.fn_type(param_types, is_var_args),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub index: u32,
    pub typenode: Box<TypeNodeEnum>,
    pub name: String,
    pub range: Range,
    pub modifier: Option<(TokenType, Range)>,
}

impl Field {
    pub fn get_doc_symbol(&self) -> DocumentSymbol {
        #[allow(deprecated)]
        DocumentSymbol {
            name: self.name.clone(),
            detail: Some(FmtBuilder::generate_node(&self.typenode)),
            kind: SymbolKind::FIELD,
            tags: None,
            deprecated: None,
            range: self.range.to_diag_range(),
            selection_range: self.range.to_diag_range(),
            children: None,
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnType {
    pub param_pltypes: Vec<Box<TypeNodeEnum>>,
    pub ret_pltype: Box<TypeNodeEnum>,
    pub generic: bool,
    pub modifier: Option<(TokenType, Range)>,
    pub method: bool,
    pub generic_map: IndexMap<String, Arc<RefCell<PLType>>>,
    pub generics_size: usize, // the size of generics except the generics from impl node
}
impl FnType {}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FNValue {
    pub name: String,     // name for lsp
    pub llvmname: String, // name in llvm ir
    pub path: String,
    pub param_names: Vec<String>,
    pub range: Range,
    pub doc: Vec<Box<NodeEnum>>,
    pub generic_infer: Arc<RefCell<IndexMap<String, Arc<RefCell<PLType>>>>>,
    pub node: Option<Box<FuncDefNode>>,
    pub fntype: FnType,
}
impl TryFrom<PLType> for FNValue {
    type Error = ();

    fn try_from(value: PLType) -> Result<Self, Self::Error> {
        match value {
            PLType::Fn(x) => Ok(x),
            _ => Err(()),
        }
    }
}
impl FNValue {
    pub fn is_modified_by(&self, modifier: TokenType) -> bool {
        if let Some((t, _)) = self.fntype.modifier {
            t == modifier
        } else {
            false
        }
    }
    pub fn expect_pub(&self, ctx: &Ctx, range: Range) -> Result<(), PLDiag> {
        if ctx.plmod.path == self.path {
            return Ok(());
        }
        if_not_modified_by!(
            self.fntype.modifier,
            TokenType::PUB,
            return expect_pub_err(
                super::diag::ErrorCode::EXPECT_PUBLIC_FUNCTION,
                ctx,
                range,
                self.name.clone()
            )
        );
        Ok(())
    }
    /// 用来比较接口函数与实现函数是否相同
    ///
    /// 忽略第一个参数比较（receiver
    ///
    /// 因为接口函数的第一个参数是*i64，而实现函数的第一个参数是实现类型
    pub fn eq_except_receiver<'a, 'ctx, 'b>(
        &self,
        other: &FNValue,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> bool {
        if self.name.split("::").last().unwrap() != other.name.split("::").last().unwrap() {
            return false;
        }
        if self.fntype.param_pltypes.len() != other.fntype.param_pltypes.len() {
            return false;
        }
        for i in 1..self.fntype.param_pltypes.len() {
            if self.fntype.param_pltypes[i].get_type(ctx, builder)
                != other.fntype.param_pltypes[i].get_type(ctx, builder)
            {
                return false;
            }
        }
        self.fntype.ret_pltype.get_type(ctx, builder)
            == other.fntype.ret_pltype.get_type(ctx, builder)
    }
    pub fn append_name_with_generic(&self, name: String) -> String {
        if self.fntype.need_gen_code() {
            let typeinfer = self
                .fntype
                .generic_map
                .iter()
                .map(|(_, v)| match &*v.clone().borrow() {
                    PLType::Generic(g) => g.curpltype.as_ref().unwrap().borrow().get_llvm_name(),
                    _ => unreachable!(),
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}<{}>", name, typeinfer)
        } else {
            name
        }
    }
    pub fn generic_infer_pltype<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<FNValue, PLDiag> {
        let name = self.append_name_with_generic(self.name.clone());
        if let Some(pltype) = self.generic_infer.borrow().get(&name) {
            if let PLType::Fn(f) = &*pltype.borrow() {
                return Ok(f.clone());
            }
            unreachable!()
        }
        let mut res = self.clone();
        res.llvmname = format!(
            "{}..{}",
            res.llvmname.split("..").collect::<Vec<&str>>()[0],
            name
        );
        res.name = name.clone();
        res.fntype.generic_map.clear();
        res.generic_infer = Arc::new(RefCell::new(IndexMap::default()));
        self.generic_infer
            .borrow_mut()
            .insert(name, Arc::new(RefCell::new(PLType::Fn(res.clone()))));

        let block = ctx.block;
        ctx.need_highlight += 1;
        let f = self.clone();
        if let Some(n) = &mut self.node {
            builder.rm_curr_debug_location();
            n.gen_fntype(ctx, false, builder, f)?;
        } else {
            unreachable!()
        }
        ctx.need_highlight -= 1;
        ctx.position_at_end(block.unwrap(), builder);

        res.fntype.ret_pltype = self
            .fntype
            .ret_pltype
            .get_type(ctx, builder)
            .unwrap()
            .borrow()
            .get_typenode();
        res.fntype.param_pltypes = self
            .fntype
            .param_pltypes
            .iter()
            .map(|p| {
                let np = p.get_type(ctx, builder).unwrap().borrow().get_typenode();
                np
            })
            .collect::<Vec<Box<TypeNodeEnum>>>();
        let pltype = self.generic_infer.borrow().get(&res.name).unwrap().clone();
        pltype.replace(PLType::Fn(res.clone()));
        Ok(res.clone())
    }
    pub fn gen_snippet(&self) -> String {
        let mut name = self.name.clone();
        let mut iter = self.param_names.iter();
        if self.fntype.method {
            iter.next();
            name = name.split("::").last().unwrap().to_string();
        }
        name + "("
            + &iter
                .enumerate()
                .map(|(i, v)| format!("${{{}:{}}}", i + 1, v))
                .collect::<Vec<_>>()
                .join(", ")
            + ")$0"
    }
    pub fn get_doc_symbol(&self) -> DocumentSymbol {
        #[allow(deprecated)]
        DocumentSymbol {
            name: if self.fntype.method {
                self.name.split("::").last().unwrap().to_string()
            } else {
                self.name.clone()
            },
            detail: Some(self.get_signature()),
            kind: if self.fntype.method {
                SymbolKind::METHOD
            } else {
                SymbolKind::FUNCTION
            },
            tags: None,
            deprecated: None,
            range: self.range.to_diag_range(),
            selection_range: self.range.to_diag_range(),
            children: None,
        }
    }
    pub fn get_signature(&self) -> String {
        let mut params = String::new();
        if !self.param_names.is_empty() {
            if !self.fntype.method {
                params += &format!(
                    "{}: {}",
                    self.param_names[0],
                    FmtBuilder::generate_node(&self.fntype.param_pltypes[0])
                );
            }
            for i in 1..self.param_names.len() {
                params += &format!(
                    ", {}: {}",
                    self.param_names[i],
                    FmtBuilder::generate_node(&self.fntype.param_pltypes[i])
                );
            }
        }
        format!(
            "fn ({}) {}",
            params,
            FmtBuilder::generate_node(&self.fntype.ret_pltype)
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ARRType {
    pub element_type: Arc<RefCell<PLType>>,
    pub size: u32,
}

impl ARRType {
    pub fn get_elem_type(&self) -> Arc<RefCell<PLType>> {
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
    pub doc: Vec<Box<NodeEnum>>,
    pub generic_map: IndexMap<String, Arc<RefCell<PLType>>>,
    pub derives: Vec<Arc<RefCell<PLType>>>,
    pub modifier: Option<(TokenType, Range)>,
}

impl STType {
    fn implements(&self, tp: &PLType, plmod: &Mod) -> bool {
        plmod
            .impls
            .get(&self.get_st_full_name())
            .and_then(|v| v.get(&tp.get_full_elm_name()))
            .is_some()
    }
    pub fn implements_trait(&self, tp: &STType, plmod: &Mod) -> bool {
        if self.implements_trait_curr_mod(tp, plmod) {
            return true;
        }
        for plmod in plmod.submods.values() {
            if self.implements_trait(tp, plmod) {
                return true;
            }
        }
        false
    }
    pub fn expect_field_pub(&self, ctx: &Ctx, f: &Field, range: Range) -> Result<(), PLDiag> {
        if self.path == ctx.plmod.path {
            return Ok(());
        }

        if_not_modified_by!(
            f.modifier,
            TokenType::PUB,
            return expect_pub_err(
                super::diag::ErrorCode::EXPECT_PUBLIC_FIELD,
                ctx,
                range,
                f.name.clone()
            )
        );
        Ok(())
    }
    fn implements_trait_curr_mod(&self, tp: &STType, plmod: &Mod) -> bool {
        let re = plmod
            .impls
            .get(&self.get_st_full_name())
            .and_then(|v| v.get(&tp.get_st_full_name()))
            .is_some();
        if !re {
            return re;
        }
        for de in &tp.derives {
            let re = self.implements(&de.borrow(), plmod);
            if !re {
                return re;
            }
        }
        true
    }
    pub fn get_type_code(&self) -> u64 {
        let full_name = self.get_st_full_name();
        get_hash_code(full_name)
    }
    pub fn append_name_with_generic(&self) -> String {
        let typeinfer = self
            .generic_map
            .iter()
            .map(|(_, v)| match &*v.clone().borrow() {
                PLType::Generic(g) => g.curpltype.as_ref().unwrap().borrow().get_name(),
                _ => unreachable!(),
            })
            .collect::<Vec<_>>()
            .join(", ");
        format!("{}<{}>", self.name, typeinfer)
    }
    pub fn gen_code<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<STType, PLDiag> {
        let name = self.append_name_with_generic();
        if let Ok(pltype) = ctx.get_type(&name, Default::default()) {
            match &*pltype.borrow() {
                PLType::Struct(st) => {
                    return Ok(st.clone());
                }
                _ => unreachable!(),
            }
        }
        let mut res = self.clone();
        res.name = name;
        ctx.add_type_without_check(Arc::new(RefCell::new(PLType::Struct(res.clone()))));
        let (tps, errors): (Vec<_>, Vec<_>) = self
            .ordered_fields
            .iter()
            .map(|f| {
                let mut nf = f.clone();
                nf.typenode = f.typenode.get_type(ctx, builder)?.borrow().get_typenode();
                Ok::<Field, PLDiag>(nf)
            })
            .partition(Result::is_ok);
        if !errors.is_empty() {
            return Err(errors.into_iter().map(Result::unwrap_err).next().unwrap());
        }
        res.ordered_fields = tps.into_iter().map(Result::unwrap).collect::<Vec<_>>();
        let mut field_pltps = vec![];
        res.ordered_fields.iter().for_each(|f| {
            field_pltps.push(f.typenode.get_type(ctx, builder).unwrap());
            res.fields.insert(f.name.clone(), f.clone());
        });
        builder.gen_st_visit_function(ctx, &res, &field_pltps);
        res.generic_map.clear();
        let pltype = ctx.get_type(&res.name, Default::default()).unwrap();
        pltype.replace(PLType::Struct(res.clone()));
        Ok(res)
    }
    pub fn get_field_completions(&self, must_pub: bool) -> Vec<CompletionItem> {
        let mut completions = Vec::new();
        for (name, f) in &self.fields {
            if f.index == 0 {
                continue;
            }
            if must_pub {
                skip_if_not_modified_by!(f.modifier, TokenType::PUB);
            }
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
    pub fn get_mthd_completions(&self, ctx: &Ctx) -> Vec<CompletionItem> {
        ctx.plmod
            .get_methods_completions(&self.get_st_full_name(), self.path != ctx.plmod.path)
    }

    pub fn get_completions(&self, ctx: &Ctx) -> Vec<CompletionItem> {
        let mut coms = self.get_field_completions(self.path != ctx.plmod.path);
        coms.extend(self.get_mthd_completions(ctx));
        coms
    }
    pub fn get_trait_completions(&self, ctx: &Ctx) -> Vec<CompletionItem> {
        let mut coms = self.get_trait_field_completions();
        coms.extend(self.get_mthd_completions(ctx));
        coms
    }
    pub fn get_trait_field_completions(&self) -> Vec<CompletionItem> {
        let mut completions = Vec::new();
        for (name, f) in &self.fields {
            if let TypeNodeEnum::Func(func) = &*f.typenode {
                completions.push(CompletionItem {
                    kind: Some(CompletionItemKind::METHOD),
                    label: name.clone(),
                    detail: Some("method".to_string()),
                    insert_text: Some(func.gen_snippet()),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    command: Some(Command::new(
                        "trigger help".to_string(),
                        "editor.action.triggerParameterHints".to_string(),
                        None,
                    )),
                    ..Default::default()
                });
            }
        }
        completions
    }
    pub fn find_method(&self, ctx: &Ctx, method: &str) -> Option<FNValue> {
        ctx.plmod
            .find_method(&self.get_st_full_name_except_generic(), method)
    }
    pub fn get_st_full_name(&self) -> String {
        format!("{}..{}", self.path, self.name)
    }
    pub fn get_st_full_name_except_generic(&self) -> String {
        let full_name = self.get_st_full_name();
        full_name.split('<').collect::<Vec<_>>()[0].to_string()
    }
    pub fn get_doc_symbol(&self) -> DocumentSymbol {
        let children: Vec<DocumentSymbol> = self
            .ordered_fields
            .iter()
            .map(|order_field| order_field.get_doc_symbol())
            .collect();
        #[allow(deprecated)]
        DocumentSymbol {
            name: self.name.clone(),
            detail: None,
            kind: SymbolKind::STRUCT,
            tags: None,
            deprecated: None,
            range: self.range.to_diag_range(),
            selection_range: self.range.to_diag_range(),
            children: Some(children),
        }
    }
}

pub fn add_primitive_types(ctx: &mut Ctx) {
    add_basic_types!(
        ctx.plmod.types,
        i128,
        i64,
        i32,
        i16,
        i8,
        u128,
        u64,
        u32,
        u16,
        u8,
        f64,
        f32,
        bool
    );
    let pltype_void = PLType::Void;
    ctx.plmod
        .types
        .insert("void".to_string(), Arc::new(RefCell::new(pltype_void)));
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericType {
    pub name: String,
    pub range: Range,
    pub curpltype: Option<Arc<RefCell<PLType>>>,
    pub trait_impl: Option<Arc<RefCell<PLType>>>,
}
impl GenericType {
    pub fn set_type(&mut self, pltype: Arc<RefCell<PLType>>) {
        self.curpltype = Some(pltype);
    }
    pub fn clear_type(&mut self) {
        self.curpltype = None;
    }
    pub fn set_place_holder(&mut self, ctx: &mut Ctx) {
        if let Some(trait_impl) = &self.trait_impl {
            self.curpltype = Some(trait_impl.clone());
            return;
        }
        let range = self.range;
        let p = PlaceHolderType {
            name: self.name.clone(),
            range,
        };
        let name_in_map = p.get_place_holder_name();
        let pltype = Arc::new(RefCell::new(PLType::PlaceHolder(p)));
        self.curpltype = Some(pltype.clone());
        ctx.add_type(name_in_map, pltype, range).unwrap();
    }
}
generic_impl!(FnType, STType, UnionType);
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlaceHolderType {
    pub name: String,
    pub range: Range,
}
impl PlaceHolderType {
    fn get_place_holder_name(&self) -> String {
        format!("placeholder_::{}", self.name)
    }
}
