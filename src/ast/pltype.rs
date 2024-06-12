use super::builder::ValueHandle;
use super::ctx::Ctx;
use super::diag::ErrorCode;
use super::node::interface::MultiTraitNode;
use super::node::tuple::TupleTypeNode;
use super::node::types::ClosureTypeNode;
use super::node::types::CustomTypeNode;
use super::plmod::Mod;
use super::plmod::MutVec;
use super::tokens::TokenType;
use super::traits::CustomType;
use crate::add_basic_types;
use crate::ast::builder::IRBuilder;

use crate::ast::builder::BuilderEnum;
use crate::format_label;
use crate::generic_impl;
use crate::if_not_modified_by;
use crate::inference::unknown_arc;
use crate::skip_if_not_modified_by;
use crate::utils::get_hash_code;

use super::diag::PLDiag;

use super::fmt::FmtBuilder;
use super::node::function::FuncDefNode;
use super::node::pkg::ExternIdNode;
use super::node::primary::VarNode;
use super::node::types::ArrayTypeNameNode;
use super::node::types::PointerTypeNode;
use super::node::types::TypeNameNode;
use super::node::NodeEnum;
use super::node::TypeNode;
use super::node::TypeNodeEnum;
use super::range::Range;
#[cfg(feature = "llvm")]
use immix::ObjectType;
use indexmap::IndexMap;

use internal_macro::range;
use linked_hash_map::LinkedHashMap;
use lsp_types::Command;
use lsp_types::CompletionItem;
use lsp_types::CompletionItemKind;
use lsp_types::DocumentSymbol;
use lsp_types::InsertTextFormat;

use lsp_types::Location;
use lsp_types::SymbolKind;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use ustr::ustr;
use ustr::Ustr;

use std::sync::atomic::AtomicI64;
use std::sync::atomic::Ordering;
use std::sync::Arc;

mod tpdocs;
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
    Closure(ClosureType),
    /// # Unknown
    ///
    /// Unknown type means that the type is not inferred
    /// or cannot be inferred
    Unknown,
    /// # PartialInferred
    ///
    /// PartialInfered type is used for type inference
    ///
    /// When a type is wrapped by PartialInfered, it means that the type
    /// is not fully inferred
    /// e.g. it 'contains' Unknown type
    PartialInferred(Arc<RefCell<PLType>>),
}

impl PLType {
    /// # is_complete
    ///
    /// Check if the type is complete
    ///
    /// if the type is Unknown or PartialInferred, it is not complete
    pub fn is_complete(&self) -> bool {
        // Unknow/PartialInfered type are not complete
        !matches!(self, PLType::Unknown | PLType::PartialInferred(_))
    }
    pub fn is_atomic(&self) -> bool {
        match self {
            PLType::Struct(s) => s.is_atomic(),
            PLType::Primitive(_) => true,
            _ => false,
        }
    }
}

impl TraitImplAble for PriType {
    fn get_full_name(&self) -> Ustr {
        self.get_name()
    }
    fn get_full_name_except_generic(&self) -> Ustr {
        self.get_name()
    }
    fn get_mod_path(&self) -> Option<std::borrow::Cow<Ustr>> {
        None
    }
}

#[derive(Debug, Clone, Eq)]
pub struct ClosureType {
    pub arg_types: Vec<Arc<RefCell<PLType>>>,
    pub ret_type: Arc<RefCell<PLType>>,
    pub range: Range,
}

impl PartialEq for ClosureType {
    fn eq(&self, other: &Self) -> bool {
        if self.arg_types.len() != other.arg_types.len() {
            return false;
        }
        for i in 0..self.arg_types.len() {
            if get_type_deep(self.arg_types[i].clone()) != get_type_deep(other.arg_types[i].clone())
            {
                return false;
            }
        }
        get_type_deep(self.ret_type.clone()) == get_type_deep(other.ret_type.clone())
    }
}

impl ClosureType {
    pub fn to_type_node(&self, path: &Ustr) -> TypeNodeEnum {
        TypeNodeEnum::Closure(ClosureTypeNode {
            arg_types: self
                .arg_types
                .iter()
                .map(|t| t.borrow().get_typenode(path))
                .collect(),
            ret_type: self.ret_type.borrow().get_typenode(path),
            range: self.range,
        })
    }
    pub fn get_name(&self) -> Ustr {
        format!(
            "|{}| => {}",
            self.arg_types
                .iter()
                .map(|t| t.borrow().get_name().to_string())
                .collect::<Vec<_>>()
                .join(", "),
            self.ret_type.borrow().get_name()
        )
        .into()
    }
}
impl TraitImplAble for ClosureType {
    fn get_full_name(&self) -> Ustr {
        self.get_name()
    }
    fn get_full_name_except_generic(&self) -> Ustr {
        self.get_name()
    }
    fn get_mod_path(&self) -> Option<std::borrow::Cow<Ustr>> {
        None
    }
}

mod method;

pub use method::*;

macro_rules! impl_mthd {
    ($n:ident) => {
        impl ImplAble for $n {
            fn get_method_table(&self) -> Arc<RefCell<FxHashMap<Ustr, Arc<RefCell<FNValue>>>>>{
                return self.methods.clone();
            }

        }
        impl TraitImplAble for $n {
            fn get_full_name(&self) -> Ustr {
                format!("{}..{}", self.path, self.name).into()
            }
            fn get_full_name_except_generic(&self) -> Ustr {
                let full_name = self.get_full_name();
                full_name.split('<').collect::<Vec<_>>()[0].to_string().into()
            }

            fn get_mod_path(&self) -> Option<std::borrow::Cow<Ustr>> {
                Some(std::borrow::Cow::Borrowed(&self.path))
            }
        }
    };
    ($($n:ident),*) => (
        $(
            impl_mthd!($n);
        )*
    );
    ($($n:ident),*,) => (
        $(
            impl_mthd!($n);
        )*
    );

}

#[range]
#[derive(Debug, Clone, Eq)]
pub struct UnionType {
    pub name: Ustr,
    pub generic_map: IndexMap<Ustr, Arc<RefCell<PLType>>>,
    pub sum_types: Vec<Box<TypeNodeEnum>>,
    pub path: Ustr,
    pub modifier: Option<(TokenType, Range)>,
    pub methods: Arc<RefCell<FxHashMap<Ustr, Arc<RefCell<FNValue>>>>>,
}

impl PartialEq for UnionType {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.generic_map == other.generic_map
            && self.sum_types == other.sum_types
            && self.path == other.path
    }
}

impl_mthd!(UnionType, STType, PlaceHolderType);

impl UnionType {
    pub fn find_method(&self, method: &Ustr) -> Option<Arc<RefCell<FNValue>>> {
        self.methods.borrow().get(method).cloned()
    }
    pub fn append_name_with_generic(&self) -> Ustr {
        let typeinfer = self
            .generic_map
            .iter()
            .map(|(_, v)| match &*v.clone().borrow() {
                PLType::Generic(g) => g
                    .curpltype
                    .as_ref()
                    .unwrap()
                    .borrow()
                    .get_name()
                    .to_string(),
                _ => unreachable!(),
            })
            .collect::<Vec<_>>()
            .join(", ");
        format!("{}<{}>", self.name, typeinfer).into()
    }
    pub fn gen_code<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
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
        res.sum_types = self
            .sum_types
            .iter()
            .map(|t| {
                Ok(t.get_type(ctx, builder, true)?
                    .borrow()
                    .get_typenode(&ctx.get_file()))
            })
            .collect::<Result<Vec<_>, PLDiag>>()?;
        res.generic_map.clear();
        let pltype = ctx.get_type(&res.name, Default::default()).unwrap();
        pltype.typ.replace(PLType::Union(res.clone()));
        // ctx.generic_infer
        // .borrow_mut().entry(self.get_full_name_except_generic()).or_insert(Default::default())
        // .borrow_mut()
        // .insert(res.name.clone(), pltype.tp.clone());
        ctx.add_infer_result(self, res.name, pltype.typ);
        builder.set_di_file(&ctx.get_file());
        Ok(res)
    }
    pub fn has_type<'a, 'b>(
        &self,
        pltype: &PLType,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> Option<usize> {
        ctx.run_in_type_mod(self, |ctx, u| {
            u.sum_types
                .iter()
                .enumerate()
                .find(|(_, t)| {
                    get_type_deep(t.get_type(ctx, builder, true).unwrap_or(unknown_arc()))
                        == get_type_deep(Arc::new(RefCell::new(pltype.clone())))
                })
                .map(|(i, _)| i)
        })
    }

    pub fn get_sum_types<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> Vec<Arc<RefCell<PLType>>> {
        ctx.run_in_type_mod(self, |ctx, u| {
            u.sum_types
                .iter()
                .map(|t| t.get_type(ctx, builder, true).unwrap_or(unknown_arc()))
                .collect()
        })
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
    CHAR,
}
impl PriType {
    pub fn get_name(&self) -> Ustr {
        match self {
            PriType::I8 => "i8".into(),
            PriType::I16 => "i16".into(),
            PriType::I32 => "i32".into(),
            PriType::I64 => "i64".into(),
            PriType::I128 => "i128".into(),
            PriType::U8 => "u8".into(),
            PriType::U16 => "u16".into(),
            PriType::U32 => "u32".into(),
            PriType::U64 => "u64".into(),
            PriType::U128 => "u128".into(),
            PriType::F32 => "f32".into(),
            PriType::F64 => "f64".into(),
            PriType::BOOL => "bool".into(),
            PriType::CHAR => "char".into(),
        }
    }
    pub fn signed(&self) -> bool {
        matches!(
            self,
            PriType::I8 | PriType::I16 | PriType::I32 | PriType::I64 | PriType::I128
        )
    }
    pub fn is_int(&self) -> bool {
        matches!(
            self,
            PriType::I8
                | PriType::I16
                | PriType::I32
                | PriType::I64
                | PriType::I128
                | PriType::U8
                | PriType::U16
                | PriType::U32
                | PriType::U64
                | PriType::U128
                | PriType::BOOL
                | PriType::CHAR
        )
    }

    /// # try_from_str
    ///
    /// try to convert a string to a pivot language type if it's a primary type
    pub fn try_from_str(str: &Ustr) -> Option<Self> {
        match str.as_str() {
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
            "char" => Some(PriType::CHAR),
            _ => None,
        }
    }
}
fn new_typename_node(name: &Ustr, range: Range, ns: &[Ustr]) -> Box<TypeNodeEnum> {
    Box::new(TypeNodeEnum::Basic(TypeNameNode {
        id: Some(ExternIdNode {
            namespace: ns
                .iter()
                .map(|s| {
                    Box::new(VarNode {
                        name: *s,
                        range: Default::default(),
                        id: None,
                    })
                })
                .collect(),
            id: Box::new(VarNode {
                name: *name,
                range,
                id: None,
            }),
            complete: true,
            singlecolon: false,
            range,
        }),
        generic_params: None,
        range,
        generic_infer: None,
    }))
}
fn new_arrtype_node(typenode: Box<TypeNodeEnum>) -> Box<TypeNodeEnum> {
    Box::new(TypeNodeEnum::Array(ArrayTypeNameNode {
        id: typenode,
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
                get_type_deep(g.curpltype.as_ref().unwrap().clone())
            } else {
                pltype.clone()
            }
        }
        PLType::Pointer(p) => Arc::new(RefCell::new(PLType::Pointer(get_type_deep(p.clone())))),
        PLType::Arr(p) => {
            let a = ARRType {
                element_type: get_type_deep(p.element_type.clone()),
                size_handle: p.size_handle,
                generic_map: p.generic_map.clone(),
            };
            Arc::new(RefCell::new(PLType::Arr(a)))
        }
        _ => pltype.clone(),
    }
}

fn expect_pub_err(err: ErrorCode, ctx: &Ctx, range: Range, name: Ustr) -> Result<(), PLDiag> {
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
    pub fn new_i8_ptr() -> PLType {
        PLType::Pointer(Arc::new(RefCell::new(PLType::Primitive(PriType::I8))))
    }
    pub fn new_u8_ptr() -> PLType {
        PLType::Pointer(Arc::new(RefCell::new(PLType::Primitive(PriType::U8))))
    }
    pub fn new_i64() -> PLType {
        PLType::Primitive(PriType::I64)
    }
    #[cfg(feature = "llvm")]
    pub fn get_immix_type(&self) -> ObjectType {
        match self {
            PLType::Struct(s) if s.atomic => ObjectType::Atomic,
            PLType::Struct(_) => ObjectType::Complex,
            PLType::Arr(_) => ObjectType::Complex,
            PLType::Pointer(_) => ObjectType::Pointer,
            PLType::Trait(_) => ObjectType::Trait,
            PLType::Union(_) => ObjectType::Trait, // share same layout as trait
            PLType::Closure(_) => ObjectType::Trait, // share same layout as trait
            PLType::Generic(g) => {
                if g.curpltype.is_some() {
                    g.curpltype.as_ref().unwrap().borrow().get_immix_type()
                } else {
                    ObjectType::Atomic
                }
            }
            _ => ObjectType::Atomic,
        }
    }

    pub fn implements_trait(&self, tp: &STType, ctx: &Ctx) -> bool {
        let name = &self.get_full_elm_name_without_generic();

        match self {
            PLType::Struct(s) => s.implements_trait(tp, ctx),
            PLType::Union(u) => u.implements_trait(tp, ctx),
            PLType::Arr(a) => a.implements_trait(tp, ctx),
            _ => {
                let plmod = if tp.path == ctx.plmod.path {
                    ctx.plmod.clone()
                } else {
                    ctx.db.get_module(tp.path).unwrap_or_else(|| {
                        panic!(
                            "expect module {} exists, trait name: {}",
                            &tp.path, &tp.name
                        )
                    })
                };
                if impl_in_mod(&plmod, name, tp) {
                    return true;
                } else {
                    for m in plmod.submods.values() {
                        if impl_in_mod(m, name, tp) {
                            return true;
                        }
                    }
                }
                false
            }
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            PLType::Primitive(p) => p.is_int(),
            _ => false,
        }
    }

    pub fn get_kind_name(&self) -> Ustr {
        match self {
            PLType::Primitive(_) | PLType::Void => "primitive".into(),
            PLType::Pointer(_) => "pointer".into(),
            PLType::Arr(_) => "array".into(),
            PLType::Struct(_) => "struct".into(),
            PLType::Fn(_) => "function".into(),
            PLType::PlaceHolder(_) => "placeholder".into(),
            PLType::Generic(_) => "generic".into(),
            PLType::Trait(_) => "trait".into(),
            PLType::Union(_) => "union".into(),
            PLType::Closure(_) => "closure".into(),
            PLType::Unknown => "unknown".into(),
            PLType::PartialInferred(_) => "partial".into(),
        }
    }

    fn new_custom_tp_node<T: CustomType>(tp: &T, _path: &Ustr) -> Box<TypeNodeEnum> {
        Box::new(TypeNodeEnum::Custom(CustomTypeNode {
            name: tp.get_name(),
            range: tp.get_range(),
            path: tp.get_path(),
        }))
    }

    pub fn get_typenode(&self, path: &Ustr) -> Box<TypeNodeEnum> {
        match self {
            PLType::Struct(st) => {
                if st.is_tuple {
                    Box::new(TypeNodeEnum::Tuple(TupleTypeNode {
                        types: st
                            .fields
                            .iter()
                            .map(|v| v.1.typenode.clone())
                            .collect::<Vec<_>>(),
                        range: st.range,
                    }))
                } else {
                    Self::new_custom_tp_node(st, path)
                }
            }
            PLType::Arr(arr) => new_arrtype_node(arr.get_elem_type().borrow().get_typenode(path)),
            PLType::Primitive(p) => new_typename_node(&p.get_name(), Default::default(), &[]),
            PLType::Void => new_typename_node(&"void".into(), Default::default(), &[]),
            PLType::Pointer(p) => new_ptrtype_node(p.borrow().get_typenode(path)),
            PLType::Generic(g) => {
                if g.curpltype.is_some() {
                    g.curpltype.as_ref().unwrap().borrow().get_typenode(path)
                } else {
                    new_typename_node(&g.name, Default::default(), &[])
                }
            }
            PLType::PlaceHolder(p) => new_typename_node(&p.name, Default::default(), &[]),
            PLType::Trait(t) => Self::new_custom_tp_node(t, path),
            PLType::Fn(_) => new_typename_node(&"Unknown".into(), Default::default(), &[]),
            PLType::Union(u) => Self::new_custom_tp_node(u, path),
            PLType::Closure(c) => Box::new(c.to_type_node(path)),
            PLType::Unknown => new_typename_node(&"Unknown".into(), Default::default(), &[]),
            PLType::PartialInferred(p) => p.borrow().get_typenode(path),
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
    pub fn if_refs(&self, f: impl FnOnce(&PLType), f_local: impl FnOnce(&GenericType)) {
        match self {
            PLType::Fn(_) | PLType::Struct(_) | PLType::Trait(_) | PLType::Union(_) => f(self),
            PLType::Arr(_) => (),
            PLType::Primitive(_) => (),
            PLType::Void => (),
            PLType::Pointer(_) => (),
            PLType::Generic(g) => f_local(g),
            PLType::PlaceHolder(_) => (),
            PLType::Closure(_) => (),
            PLType::Unknown => (),
            PLType::PartialInferred(p) => p.borrow().if_refs(f, f_local),
        }
    }

    pub fn get_path(&self) -> Option<Ustr> {
        match self {
            PLType::Fn(f) => Some(f.get_path()),
            PLType::Struct(f) | PLType::Trait(f) => Some(f.get_path()),
            PLType::Union(f) => Some(f.get_path()),
            _ => None,
        }
    }

    pub fn get_name(&self) -> Ustr {
        match self {
            PLType::Fn(fu) => fu.name,
            PLType::Struct(st) => {
                if st.is_tuple {
                    if st.name.starts_with('@') {
                        // convert `@Tuple{num}<{tys}>` to `(tys)`
                        let re = regex::Regex::new(r"@Tuple\d+<(.*)>").unwrap();
                        if let Some(captures) = re.captures(&st.name) {
                            if let Some(matched) = captures.get(1) {
                                return ustr(&format!("({})", matched.as_str()));
                            }
                        }
                    }
                    st.name
                } else {
                    st.name
                }
            }
            PLType::Primitive(pri) => pri.get_name(),
            PLType::Arr(arr) => format!("[{}]", arr.element_type.borrow().get_name()).into(),
            PLType::Void => "void".into(),
            PLType::Pointer(p) => ustr(&("*".to_string() + &p.borrow().get_name())),
            PLType::Generic(g) => {
                if g.curpltype.is_some() {
                    g.curpltype.as_ref().unwrap().borrow().get_name()
                } else {
                    g.name
                }
            }
            PLType::PlaceHolder(p) => p.name,
            PLType::Trait(t) => t.name,
            PLType::Union(u) => u.name,
            PLType::Closure(c) => c.get_name(),
            PLType::Unknown => "Unknown".into(),
            PLType::PartialInferred(p) => p.borrow().get_name(),
        }
    }
    pub fn get_llvm_name(&self) -> Ustr {
        match self {
            PLType::Fn(fu) => fu.name,
            PLType::Struct(st) => st.name,
            PLType::Trait(t) => t.name,
            PLType::Primitive(pri) => pri.get_name(),
            PLType::Arr(arr) => format!("[{}]", arr.element_type.borrow().get_name()).into(),
            PLType::Void => "void".into(),
            PLType::Pointer(p) => ustr(&("*".to_string() + &p.borrow().get_name())),
            PLType::Generic(g) => {
                if g.curpltype.is_some() {
                    g.curpltype.as_ref().unwrap().borrow().get_name()
                } else {
                    g.name
                }
            }
            PLType::PlaceHolder(p) => p.get_place_holder_name(),
            PLType::Union(u) => u.name,
            PLType::Closure(c) => c.get_name(),
            PLType::Unknown => "Unknown".into(),
            PLType::PartialInferred(p) => p.borrow().get_llvm_name(),
        }
    }

    pub fn get_full_elm_name(&self) -> Ustr {
        match self {
            PLType::Generic(g) => {
                if g.curpltype.is_some() {
                    g.curpltype.as_ref().unwrap().borrow().get_full_elm_name()
                } else {
                    g.name
                }
            }
            PLType::Fn(fu) => fu.llvmname,
            PLType::Struct(st) => st.get_full_name(),
            PLType::Trait(st) => st.get_full_name(),
            PLType::Primitive(pri) => pri.get_name(),
            PLType::Arr(arr) => {
                format!("[{}]", arr.element_type.borrow().get_full_elm_name(),).into()
            }
            PLType::Void => "void".into(),
            PLType::Pointer(p) => p.borrow().get_full_elm_name(),
            PLType::PlaceHolder(p) => p.name,
            PLType::Union(u) => u.get_full_name(),
            PLType::Closure(c) => c.get_name(),
            PLType::Unknown => "Unknown".into(),
            PLType::PartialInferred(p) => p.borrow().get_full_elm_name(),
        }
    }
    pub fn get_full_elm_name_without_generic(&self) -> Ustr {
        match self {
            PLType::Generic(g) => g.name,
            PLType::Fn(fu) => fu.llvmname,
            PLType::Struct(st) => st.get_full_name_except_generic(),
            PLType::Trait(st) => st.get_full_name_except_generic(),
            PLType::Primitive(pri) => pri.get_name(),
            PLType::Arr(_) => "[]".into(),
            PLType::Void => "void".into(),
            PLType::Pointer(p) => p.borrow().get_full_elm_name(),
            PLType::PlaceHolder(p) => p.name,
            PLType::Union(u) => u.get_full_name_except_generic(),
            PLType::Closure(c) => c.get_name(),
            PLType::Unknown => "Unknown".into(),
            PLType::PartialInferred(p) => p.borrow().get_full_elm_name_without_generic(),
        }
    }
    pub fn get_ptr_depth(&self) -> usize {
        match self {
            PLType::Pointer(p) => p.borrow().get_ptr_depth() + 1,
            _ => 0,
        }
    }

    pub fn is_pub(&self) -> bool {
        match self {
            PLType::Fn(f) => f.is_modified_by(TokenType::PUB),
            PLType::Struct(s) => {
                if_not_modified_by!(s.modifier, TokenType::PUB, return false);
                true
            }
            PLType::Trait(st) => {
                if_not_modified_by!(st.modifier, TokenType::PUB, return false);
                true
            }
            PLType::Union(st) => {
                if_not_modified_by!(st.modifier, TokenType::PUB, return false);
                true
            }
            PLType::PartialInferred(p) => p.borrow().is_pub(),
            _ => true,
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
                        s.name
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
                        st.name
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
                        st.name
                    )
                );
                Ok(())
            }
            PLType::PartialInferred(p) => p.borrow().expect_pub(ctx, range),
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
            PLType::Closure(c) => Some(c.range),
            PLType::Unknown => None,
            PLType::PartialInferred(p) => p.borrow().get_range(),
        }
    }

    pub fn is_void(&self) -> bool {
        matches!(self, PLType::Void)
    }
}

fn impl_in_mod(plmod: &Mod, name: &Ustr, tp: &STType) -> bool {
    plmod
        .impls
        .borrow()
        .get(name)
        .map(|v| {
            v.get(&tp.get_full_name_except_generic())
                .map(|gm| {
                    tp.get_full_name()
                        == append_name_with_generic(gm, &tp.get_full_name_except_generic())
                })
                .unwrap_or(v.get(&tp.get_full_name_except_generic()).is_some())
        })
        .unwrap_or_default()
}

#[derive(Debug, Clone, Eq)]
pub struct Field {
    pub index: u32,
    pub typenode: Box<TypeNodeEnum>,
    pub name: Ustr,
    pub range: Range,
    pub modifier: Option<(TokenType, Range)>,
}

impl PartialEq for Field {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.typenode == other.typenode
    }
}

impl Field {
    pub fn get_doc_symbol(&self) -> DocumentSymbol {
        #[allow(deprecated)]
        DocumentSymbol {
            name: self.name.to_string(),
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
    /// flag indicating whether the function is a method
    pub st_method: bool,
    pub trait_method: bool,
    pub generic_map: IndexMap<Ustr, Arc<RefCell<PLType>>>,
    pub generics_size: usize, // the size of generics except the generics from impl node
}
impl FnType {}

#[range]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FNValue {
    pub name: Ustr,     // name for lsp
    pub llvmname: Ustr, // name in llvm ir
    pub path: Ustr,
    pub param_names: Vec<Ustr>,
    pub doc: Vec<Box<NodeEnum>>,
    pub generic_infer: Arc<RefCell<IndexMap<Ustr, Arc<RefCell<PLType>>>>>,
    pub node: Option<Box<FuncDefNode>>,
    pub fntype: FnType,
    pub body_range: Range,
    pub in_trait: bool,
    pub is_declare: bool,
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

static GLOB_COUNTER: AtomicI64 = AtomicI64::new(0);

impl FNValue {
    pub fn get_generator_ctx_name(&self) -> Ustr {
        (self.name.clone().to_string()
            + "__generator_ctx"
            + &GLOB_COUNTER.fetch_add(1, Ordering::SeqCst).to_string())
            .into()
    }
    pub fn to_closure_ty<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> ClosureType {
        ctx.run_in_type_mod(self, |ctx, f| ClosureType {
            range: Default::default(),
            ret_type: f
                .fntype
                .ret_pltype
                .get_type(ctx, builder, true)
                .unwrap_or(unknown_arc()),
            arg_types: f
                .fntype
                .param_pltypes
                .iter()
                .map(|x| x.get_type(ctx, builder, true).unwrap_or(unknown_arc()))
                .collect(),
        })
    }
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
                self.name
            )
        );
        Ok(())
    }
    /// 用来比较接口函数与实现函数是否相同
    ///
    /// 忽略第一个参数比较（receiver
    ///
    /// 因为接口函数的第一个参数是*i64，而实现函数的第一个参数是实现类型
    pub fn eq_except_receiver<'a, 'b>(
        &self,
        other: &FNValue,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> bool {
        if self.name.split("::").last().unwrap() != other.name.split("::").last().unwrap() {
            return false;
        }
        if self.fntype.param_pltypes.len() != other.fntype.param_pltypes.len() {
            return false;
        }
        for i in 1..self.fntype.param_pltypes.len() {
            if get_type_deep(
                self.fntype.param_pltypes[i]
                    .get_type(ctx, builder, true)
                    .unwrap(),
            ) != get_type_deep(ctx.run_in_type_mod(other, |ctx, other| {
                other.fntype.param_pltypes[i]
                    .get_type(ctx, builder, true)
                    .unwrap()
            })) {
                return false;
            }
        }
        get_type_deep(self.fntype.ret_pltype.get_type(ctx, builder, true).unwrap())
            == get_type_deep(ctx.run_in_type_mod(other, |ctx, other| {
                other
                    .fntype
                    .ret_pltype
                    .get_type(ctx, builder, true)
                    .unwrap()
            }))
    }

    /// # append_name_with_generic
    ///
    /// it appends the generic names on the name if any and returns
    pub fn append_name_with_generic(&self, name: Ustr) -> Ustr {
        if self.fntype.need_gen_code() {
            let typeinfer = self
                .fntype
                .generic_map
                .iter()
                .map(|(_, v)| match &*v.clone().borrow() {
                    PLType::Generic(g) => {
                        let name = g
                            .curpltype
                            .as_ref()
                            .unwrap()
                            .borrow()
                            .get_llvm_name()
                            .to_string();
                        name
                    }
                    _ => unreachable!(),
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}<{}>", name, typeinfer).into()
        } else {
            name
        }
    }
    pub fn generic_infer_pltype<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> Result<FNValue, PLDiag> {
        let name = self.append_name_with_generic(self.name);
        // if let Some(pltype) = self.generic_infer.borrow().get(&name) {
        //     if let PLType::Fn(f) = &*pltype.borrow() {
        //         return Ok(f.clone());
        //     }
        //     unreachable!()
        // }
        let mut res = self.clone();
        res.llvmname = format!(
            "{}..{}",
            res.llvmname.split("..").collect::<Vec<&str>>()[0],
            name
        )
        .into();
        res.name = name;
        res.fntype.generic_map.clear();
        res.generic_infer = Arc::new(RefCell::new(IndexMap::default()));
        self.generic_infer
            .borrow_mut()
            .insert(name, Arc::new(RefCell::new(PLType::Fn(res.clone()))));

        let block = builder.get_cur_basic_block();
        *ctx.need_highlight.borrow_mut() += 1;
        let f = self.clone();
        if let Some(n) = &mut self.node {
            builder.rm_curr_debug_location();
            // gencode
            n.gen_fntype(ctx, false, builder, f)?;
            builder.set_di_file(&ctx.get_file());
        } else {
            unreachable!()
        }
        *ctx.need_highlight.borrow_mut() -= 1;
        ctx.position_at_end(block, builder);

        res.fntype.ret_pltype = self
            .fntype
            .ret_pltype
            .get_type(ctx, builder, true)
            .unwrap()
            .borrow()
            .get_typenode(&ctx.get_file());
        res.fntype.param_pltypes = self
            .fntype
            .param_pltypes
            .iter()
            .map(|p| {
                let np = p
                    .get_type(ctx, builder, true)
                    .unwrap()
                    .borrow()
                    .get_typenode(&ctx.get_file());
                np
            })
            .collect::<Vec<Box<TypeNodeEnum>>>();
        let pltype = self.generic_infer.borrow().get(&res.name).unwrap().clone();
        pltype.replace(PLType::Fn(res.clone()));
        if matches!(builder, BuilderEnum::NoOp(_)) {
            self.generic_infer.borrow_mut().remove(&res.name);
        }
        Ok(res.clone())
    }
    pub fn gen_snippet(&self) -> Ustr {
        let mut name = self.name;
        let mut iter = self.param_names.iter();
        if self.fntype.st_method {
            iter.next();
            name = name.split("::").last().unwrap().to_string().into();
        }
        (name.to_string()
            + "("
            + &iter
                .enumerate()
                .map(|(i, v)| format!("${{{}:{}}}", i + 1, v))
                .collect::<Vec<_>>()
                .join(", ")
            + ")$0")
            .into()
    }
    pub fn get_doc_symbol(&self) -> DocumentSymbol {
        #[allow(deprecated)]
        DocumentSymbol {
            name: if self.fntype.st_method {
                self.name.split("::").last().unwrap().to_string()
            } else {
                self.name.to_string()
            },
            detail: Some(self.get_signature().to_string()),
            kind: if self.fntype.st_method {
                SymbolKind::METHOD
            } else {
                SymbolKind::FUNCTION
            },
            tags: None,
            deprecated: None,
            range: self.body_range.to_diag_range(),
            selection_range: self.range.to_diag_range(),
            children: None,
        }
    }
    pub fn get_signature(&self) -> Ustr {
        let mut params = String::new();
        if !self.param_names.is_empty() {
            if !self.fntype.st_method {
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
        .into()
    }
}

#[derive(Debug, Clone)]
pub struct ARRType {
    pub element_type: Arc<RefCell<PLType>>,
    pub size_handle: ValueHandle,
    pub generic_map: IndexMap<Ustr, Arc<RefCell<PLType>>>,
}

impl TraitImplAble for ARRType {
    fn get_full_name_except_generic(&self) -> Ustr {
        "[]".into()
    }

    fn get_full_name(&self) -> Ustr {
        ustr(&format!(
            "[{}]",
            self.element_type.borrow().get_full_elm_name()
        ))
    }

    fn get_mod_path(&self) -> Option<std::borrow::Cow<Ustr>> {
        None
    }
}

impl ARRType {
    pub fn get_elem_type(&self) -> Arc<RefCell<PLType>> {
        self.element_type.clone()
    }
}

impl PartialEq for ARRType {
    fn eq(&self, other: &Self) -> bool {
        self.element_type == other.element_type
    }
}

impl Eq for ARRType {}

#[range]
#[derive(Debug, Clone, Eq, Default)]
pub struct STType {
    pub name: Ustr,
    pub path: Ustr,
    pub fields: LinkedHashMap<Ustr, Field>,
    pub doc: Vec<Box<NodeEnum>>,
    pub generic_map: IndexMap<Ustr, Arc<RefCell<PLType>>>,
    pub derives: Vec<Arc<RefCell<PLType>>>,
    pub modifier: Option<(TokenType, Range)>,
    pub body_range: Range,
    pub is_trait: bool,
    pub is_tuple: bool,
    pub generic_infer_types: IndexMap<Ustr, Arc<RefCell<PLType>>>,
    pub methods: Arc<RefCell<FxHashMap<Ustr, Arc<RefCell<FNValue>>>>>,
    pub trait_methods_impl: TraitMthdImpl,
    pub atomic: bool,
}

pub type TraitMthdImpl = Arc<RefCell<FxHashMap<Ustr, FxHashMap<Ustr, Arc<RefCell<FNValue>>>>>>;

pub fn append_name_with_generic(gm: &IndexMap<Ustr, Arc<RefCell<PLType>>>, name: &Ustr) -> Ustr {
    let typeinfer = gm
        .iter()
        .map(|(_, v)| match &*v.clone().borrow() {
            PLType::Generic(g) => g
                .curpltype
                .as_ref()
                .unwrap()
                .borrow()
                .get_name()
                .to_string(),
            a => a.get_name().to_string(),
        })
        .collect::<Vec<_>>()
        .join(", ");
    if typeinfer.is_empty() {
        return *name;
    }
    if *name == "[]" {
        return ustr(&format!("[{}]", typeinfer));
    }
    ustr(&format!("{}<{}>", name, typeinfer))
}

impl PartialEq for STType {
    fn eq(&self, other: &Self) -> bool {
        if self.is_tuple && other.is_tuple {
            self.fields == other.fields
        } else {
            self.name == other.name && self.path == other.path
        }
    }
}

impl STType {
    pub fn is_atomic(&self) -> bool {
        self.atomic
    }
    pub fn check_impl_derives(&self, ctx: &Ctx, st: &STType, range: Range) {
        debug_assert!(self.is_trait);
        let errnames = self
            .derives
            .iter()
            .map(|derive| {
                if let PLType::Trait(derive) = &*derive.borrow() {
                    derive.clone()
                } else {
                    unreachable!()
                }
            })
            .filter(|derive| !st.implements_trait(derive, ctx))
            .collect::<Vec<_>>();
        if !errnames.is_empty() {
            let mut err = range.new_err(ErrorCode::DERIVE_TRAIT_NOT_IMPL);

            for e in errnames {
                err.add_label(
                    e.range,
                    e.get_path(),
                    format_label!("the derive trait {} not impl", e.name),
                );
            }

            err.add_to_ctx(ctx);
        }
    }

    /// Test if current trait can cast to another trait (implicit cast)
    ///
    /// # Example
    ///
    /// ```pl
    /// trait A {}
    /// trait B:A {}
    /// ```
    /// trait B can cast to trait A
    ///
    /// # Note
    ///
    /// if source trait and target trait is the same, return **false**
    pub fn trait_can_cast_to(&self, to_trait: &STType) -> bool {
        debug_assert!(self.is_trait && to_trait.is_trait);

        for derives in self.derives.iter() {
            if let PLType::Trait(derive) = &*derives.borrow() {
                if derive == to_trait {
                    return true;
                }
                if derive.trait_can_cast_to(to_trait) {
                    return true;
                }
            } else {
                unreachable!()
            }
        }
        false
    }
    pub fn get_trait_field(&self, k: &Ustr) -> Option<Field> {
        debug_assert!(self.is_trait);
        fn walk(st: &STType, k: &Ustr) -> (Option<Field>, u32) {
            if let Some(f) = st.fields.get(k) {
                return (Some(f.clone()), f.index - 1);
            }
            let mut offset = st.fields.len() as u32;
            for derive in st.derives.iter() {
                if let PLType::Trait(t) = &*derive.borrow() {
                    let (f, walk_offset) = walk(t, k);
                    offset += walk_offset;
                    if let Some(f) = f {
                        return (Some(f), offset);
                    }
                }
            }
            (None, offset)
        }
        let (f, offset) = walk(self, k);
        if let Some(mut f) = f {
            f.index = offset + 1;
            return Some(f);
        }
        None
    }
    fn merge_field(&self) -> Vec<Field> {
        self.fields
            .values()
            .map(Clone::clone)
            .chain(self.derives.iter().flat_map(|pltype| {
                if let PLType::Trait(t) = &*pltype.borrow() {
                    return t.merge_field();
                }
                unreachable!()
            }))
            .collect::<Vec<_>>()
    }
    pub fn list_trait_fields(&self) -> Vec<Field> {
        debug_assert!(self.is_trait);
        self.merge_field()
            .iter()
            .enumerate()
            .map(|(i, f)| {
                let mut f = f.clone();
                f.index = i as u32 + 2;
                f
            })
            .collect()
    }
    // get all field include type_hash,ptr,vtable,this func only be used in llvmbuilder
    pub fn get_all_field(&self) -> Vec<Field> {
        let mut fields = vec![];
        if self.is_trait {
            fields.push(Field {
                index: 0,
                typenode: Box::new(TypeNameNode::new_from_str(&"u64".into()).into()),
                name: "__type_hash".into(),
                range: Default::default(),
                modifier: None,
            });
            // pointer to real value
            fields.push(Field {
                index: 1,
                typenode: Box::new(TypeNodeEnum::Pointer(PointerTypeNode {
                    elm: Box::new(TypeNameNode::new_from_str(&"i64".into()).into()),
                    range: Default::default(),
                })),
                name: "__ptr".into(),
                range: Default::default(),
                modifier: None,
            });
        } else {
            // gcrtti fields
            if !self.atomic {
                fields.push(Field {
                    index: 0,
                    typenode: Box::new(TypeNameNode::new_from_str(&"u64".into()).into()),
                    name: "_rtti".into(),
                    range: Default::default(),
                    modifier: None,
                });
            }
        }
        self.merge_field().iter().for_each(|f| {
            fields.push(f.clone());
        });
        fields
            .iter_mut()
            .enumerate()
            .for_each(|(i, f)| f.index = i as u32);
        fields
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
                f.name
            )
        );
        Ok(())
    }

    pub fn get_type_code(&self) -> u64 {
        let full_name = format!("{}..{}", self.path, self.append_name_with_generic());
        get_hash_code(full_name)
    }
    pub fn append_name_with_generic(&self) -> Ustr {
        append_name_with_generic(&self.generic_map, &self.name)
    }

    pub fn gen_code<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> Result<Arc<RefCell<PLType>>, PLDiag> {
        ctx.protect_generic_context(&self.generic_map, |ctx| {
            let name = self.append_name_with_generic();
            let generic_infer_types = self
                .generic_map
                .iter()
                .map(|(k, v)| match &*v.clone().borrow() {
                    PLType::Generic(g) => (*k, g.curpltype.as_ref().unwrap().clone()),
                    _ => unreachable!(),
                })
                .collect();
            if let Ok(pltype) = ctx.get_type(&name, Default::default()) {
                return Ok(pltype.typ);
            }
            let mut res = self.clone();
            res.name = name;
            ctx.add_type_without_check(Arc::new(RefCell::new(PLType::Struct(res.clone()))));
            let fields: Result<Vec<_>, _> = self
                .fields
                .values()
                .map(|f| {
                    let mut nf = f.clone();
                    if let TypeNodeEnum::Func(fd) = &*f.typenode {
                        let mut new_f = fd.clone();
                        let mut first = true;
                        for f in new_f.paralist.iter_mut() {
                            if first {
                                f.typenode = Box::new(TypeNodeEnum::Pointer(PointerTypeNode {
                                    elm: Box::new(TypeNameNode::new_from_str(&"i64".into()).into()),
                                    range: Default::default(),
                                }));
                            } else {
                                f.typenode = f
                                    .typenode
                                    .get_type(ctx, builder, true)?
                                    .borrow()
                                    .get_typenode(&ctx.get_file());
                            }
                            first = false;
                        }
                        // eprintln!("{:?}", new_f.ret);
                        new_f.ret = new_f
                            .ret
                            .get_type(ctx, builder, true)?
                            .borrow()
                            .get_typenode(&ctx.get_file());
                        nf.typenode = Box::new(TypeNodeEnum::Func(new_f));
                    } else {
                        nf.typenode = f
                            .typenode
                            .get_type(ctx, builder, true)?
                            .borrow()
                            .get_typenode(&ctx.get_file());
                    }
                    Ok::<(Ustr, Field), PLDiag>((nf.name, nf))
                })
                .collect();
            res.fields = LinkedHashMap::from_iter(fields?);
            let field_pltps = res
                .fields
                .values()
                .map(|f| {
                    f.typenode
                        .get_type(ctx, builder, true)
                        .unwrap_or(unknown_arc())
                })
                .collect::<Vec<_>>();
            if !res.is_trait {
                let is_atomic = field_pltps.iter().all(|f| f.borrow().is_atomic());
                res.atomic = is_atomic;
                if is_atomic {
                    res.fields.iter_mut().for_each(|(_, f)| {
                        f.index -= 1;
                    });
                } else {
                    builder.gen_st_visit_function(ctx, &res, &field_pltps);
                }
            }
            res.generic_map.clear();
            res.generic_infer_types = generic_infer_types;
            let pltype = ctx.get_type(&res.name, Default::default()).unwrap();
            if res.is_trait {
                pltype.typ.replace(PLType::Trait(res.clone()));
            } else {
                pltype.typ.replace(PLType::Struct(res.clone()));
            }
            // // TODO union & nested placeholder
            // if !res
            //     .generic_infer_types
            //     .values()
            //     .any(|v| matches!(&*v.borrow(), PLType::PlaceHolder(_)))
            // {
            //     ctx.add_infer_result(self, &res.name, pltype.tp.clone());
            // }

            ctx.add_infer_result(self, res.name, pltype.typ.clone());
            builder.set_di_file(&ctx.get_file());
            Ok(pltype.typ)
        })
    }
    pub fn get_field_completions(&self, must_pub: bool) -> Vec<CompletionItem> {
        let mut completions = Vec::new();
        for (name, f) in self.fields.iter() {
            if must_pub {
                skip_if_not_modified_by!(f.modifier, TokenType::PUB);
            }
            completions.push(CompletionItem {
                kind: Some(CompletionItemKind::FIELD),
                label: name.to_string(),
                detail: Some("field".to_string()),
                insert_text: Some(name.to_string()),
                insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
                ..Default::default()
            });
        }
        completions
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
        for (name, f) in self.fields.iter() {
            if let TypeNodeEnum::Func(func) = &*f.typenode {
                completions.push(CompletionItem {
                    kind: Some(CompletionItemKind::METHOD),
                    label: name.to_string(),
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
    pub fn find_method(&self, method: &Ustr) -> Option<Arc<RefCell<FNValue>>> {
        self.get_method(*method)
    }
    pub fn get_full_name(&self) -> Ustr {
        format!("{}..{}", self.path, self.name).into()
    }
    pub fn get_doc_symbol(&self) -> DocumentSymbol {
        let children: Vec<DocumentSymbol> = self
            .fields
            .values()
            .map(|order_field| order_field.get_doc_symbol())
            .collect();
        #[allow(deprecated)]
        DocumentSymbol {
            name: self.name.to_string(),
            detail: None,
            kind: SymbolKind::STRUCT,
            tags: None,
            deprecated: None,
            range: self.body_range.to_diag_range(),
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
    ctx.plmod.types.insert("void".into(), pltype_void.into());
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericType {
    pub name: Ustr,
    pub range: Range,
    pub curpltype: Option<Arc<RefCell<PLType>>>,
    pub trait_impl: Option<MultiTraitNode>,
    pub refs: Arc<MutVec<Location>>,
}
impl GenericType {
    pub fn set_type(&mut self, pltype: Arc<RefCell<PLType>>) {
        self.curpltype = Some(pltype);
    }
    pub fn clear_type(&mut self) {
        self.curpltype = None;
    }
    pub fn set_place_holder<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> Arc<RefCell<PLType>> {
        let range = self.range;
        if let Some(impls) = &self.trait_impl {
            let place_holder = STType {
                name: self.name,
                path: ctx.get_file(),
                fields: Default::default(),
                doc: Default::default(),
                generic_map: Default::default(),
                derives: Default::default(),
                modifier: Default::default(),
                body_range: range,
                is_trait: false,
                is_tuple: false,
                generic_infer_types: Default::default(),
                methods: Default::default(),
                trait_methods_impl: Default::default(),
                range,
                atomic: false,
            };
            let ph = Arc::new(RefCell::new(PLType::Struct(place_holder)));

            for it in impls.get_types(ctx, builder).unwrap_or_default() {
                if let PLType::Trait(t) = &*it.clone().borrow() {
                    ctx.get_root_ctx().plmod.add_impl(
                        &ph.borrow().get_full_elm_name_without_generic(),
                        &it.clone().borrow().get_full_elm_name(),
                        t.generic_map.clone(),
                    );
                    let fields = t.list_trait_fields();
                    for field in fields {
                        let re = field.typenode.get_type(ctx, builder, true);
                        if let Err(e) = re {
                            e.add_to_ctx(ctx);
                            continue;
                        }
                        let tp = re.unwrap();
                        if let PLType::Fn(f) = &*tp.clone().borrow() {
                            let mut f = f.clone();
                            f.fntype.param_pltypes[0] = ph.borrow().get_typenode(&ctx.get_file());
                            let generic = f.fntype.generic;
                            ctx.add_method(
                                &ph.borrow(),
                                &field.name,
                                f,
                                Some((it.clone(), Default::default())),
                                generic,
                                Default::default(),
                            )
                            .unwrap();
                        }
                    }
                }
            }

            return ph;
        }
        let p = PlaceHolderType {
            name: self.name,
            range,
            path: "".into(),
            methods: Arc::new(RefCell::new(FxHashMap::default())),
        };
        let name_in_map = p.get_place_holder_name();
        let pltype = Arc::new(RefCell::new(PLType::PlaceHolder(p)));
        ctx.add_type(name_in_map, pltype.clone(), range).unwrap();
        pltype
    }
}
generic_impl!(FnType, STType, UnionType);
#[range]
#[derive(Debug, Clone)]
pub struct PlaceHolderType {
    pub name: Ustr,
    pub path: Ustr,
    pub methods: Arc<RefCell<FxHashMap<Ustr, Arc<RefCell<FNValue>>>>>,
}
impl PlaceHolderType {
    fn get_place_holder_name(&self) -> Ustr {
        format!("placeholder_::{}", self.name).into()
    }
}

impl PartialEq for PlaceHolderType {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.path == other.path && self.methods == other.methods
    }
}

impl Eq for PlaceHolderType {}
