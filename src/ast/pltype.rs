use super::ctx::Ctx;
// use super::plmod::RwVec;

use crate::ast::builder::BuilderEnum;
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
    FN(FNType),
    STRUCT(STType),
    ARR(ARRType),
    PRIMITIVE(PriType),
    VOID,
    POINTER(Arc<RefCell<PLType>>),
    GENERIC(GenericType),
    PLACEHOLDER(PlaceHolderType),
    TRAIT(STType),
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
pub fn eq(l: Arc<RefCell<PLType>>, r: Arc<RefCell<PLType>>) -> bool {
    match (&*l.borrow(), &*r.borrow()) {
        (PLType::GENERIC(l), PLType::GENERIC(r)) => {
            if l == r {
                return true;
            }
        }
        _ => {}
    }
    match &mut *l.borrow_mut() {
        PLType::GENERIC(l) => {
            if l.curpltype.is_some() {
                return eq(l.curpltype.as_ref().unwrap().clone(), r);
            }
            l.set_type(r.clone());
            return true;
        }
        _ => {}
    }
    match (&*l.borrow(), &*r.borrow()) {
        (PLType::PRIMITIVE(l), PLType::PRIMITIVE(r)) => l == r,
        (PLType::VOID, PLType::VOID) => true,
        (PLType::POINTER(l), PLType::POINTER(r)) => eq(l.clone(), r.clone()),
        (PLType::ARR(l), PLType::ARR(r)) => {
            eq(l.get_elem_type(), r.get_elem_type()) && l.size == r.size
        }
        (PLType::STRUCT(l), PLType::STRUCT(r)) => l.name == r.name && l.path == r.path,
        (PLType::FN(l), PLType::FN(r)) => l == r,
        (PLType::PLACEHOLDER(l), PLType::PLACEHOLDER(r)) => l == r,
        _ => false,
    }
}

fn new_typename_node(name: &str, range: Range) -> Box<TypeNodeEnum> {
    Box::new(TypeNodeEnum::BasicTypeNode(TypeNameNode {
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
    Box::new(TypeNodeEnum::ArrayTypeNode(ArrayTypeNameNode {
        id: typenode,
        size: Box::new(NodeEnum::Num(NumNode {
            value: Num::INT(size),
            range: Default::default(),
        })),
        range: Default::default(),
    }))
}
fn new_ptrtype_node(typenode: Box<TypeNodeEnum>) -> Box<TypeNodeEnum> {
    Box::new(TypeNodeEnum::PointerTypeNode(PointerTypeNode {
        elm: typenode,
        range: Default::default(),
    }))
}
fn new_exid_node(modname: &str, name: &str, range: Range) -> Box<TypeNodeEnum> {
    Box::new(TypeNodeEnum::BasicTypeNode(TypeNameNode {
        id: Some(ExternIdNode {
            ns: vec![Box::new(VarNode {
                name: modname.to_string(),
                range,
            })],
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
pub fn get_type_deep(pltype: Arc<RefCell<PLType>>) -> Arc<RefCell<PLType>> {
    match &*pltype.borrow() {
        PLType::GENERIC(g) => {
            if g.curpltype.is_some() {
                g.curpltype.as_ref().unwrap().clone()
            } else {
                pltype.clone()
            }
        }
        _ => pltype.clone(),
    }
}
impl PLType {
    pub fn get_kind_name(&self) -> String {
        match self {
            PLType::PRIMITIVE(_) | PLType::VOID => "primitive".to_string(),
            PLType::POINTER(_) => "pointer".to_string(),
            PLType::ARR(_) => "array".to_string(),
            PLType::STRUCT(_) => "struct".to_string(),
            PLType::FN(_) => "function".to_string(),
            PLType::PLACEHOLDER(_) => "placeholder".to_string(),
            PLType::GENERIC(_) => "generic".to_string(),
            PLType::TRAIT(_) => "trait".to_string(),
        }
    }
    pub fn get_typenode(&self, ctx: &Ctx) -> Box<TypeNodeEnum> {
        match self {
            PLType::STRUCT(st) => {
                if st.path == ctx.plmod.path {
                    new_typename_node(&st.name, st.range)
                } else {
                    new_exid_node(
                        st.path
                            .split("/")
                            .collect::<Vec<_>>()
                            .last()
                            .unwrap()
                            .split(".")
                            .collect::<Vec<_>>()
                            .first()
                            .unwrap(),
                        &st.name,
                        st.range,
                    )
                }
            }
            PLType::ARR(arr) => new_arrtype_node(
                arr.get_elem_type().borrow().get_typenode(ctx),
                arr.size as u64,
            ),
            PLType::PRIMITIVE(p) => new_typename_node(&p.get_name(), Default::default()),
            PLType::VOID => new_typename_node("void", Default::default()),
            PLType::POINTER(p) => new_ptrtype_node(p.borrow().get_typenode(ctx)),
            PLType::GENERIC(g) => {
                if g.curpltype.is_some() {
                    g.curpltype.as_ref().unwrap().borrow().get_typenode(ctx)
                } else {
                    new_typename_node(&g.name, Default::default())
                }
            }
            PLType::PLACEHOLDER(p) => {
                new_typename_node(&p.get_place_holder_name(), Default::default())
            }
            _ => unreachable!(),
        }
    }
    pub fn is(&self, pri_type: &PriType) -> bool {
        if let PLType::PRIMITIVE(pri) = self {
            pri == pri_type
        } else {
            false
        }
    }
    /// # if_refs
    /// if support find refs
    pub fn if_refs(&self, f: impl FnOnce(&PLType)) {
        match self {
            PLType::FN(_) | PLType::STRUCT(_) | PLType::TRAIT(_) => f(self),
            PLType::ARR(_) => (),
            PLType::PRIMITIVE(_) => (),
            PLType::VOID => (),
            PLType::POINTER(_) => (),
            PLType::GENERIC(_) => (),
            PLType::PLACEHOLDER(_) => (),
        }
    }

    pub fn get_name<'a, 'ctx>(&self) -> String {
        match self {
            PLType::FN(fu) => fu.name.clone(),
            PLType::STRUCT(st) => st.name.clone(),
            PLType::PRIMITIVE(pri) => pri.get_name(),
            PLType::ARR(arr) => {
                format!("[{} * {}]", arr.element_type.borrow().get_name(), arr.size)
            }
            PLType::VOID => "void".to_string(),
            PLType::POINTER(p) => "*".to_string() + &p.borrow().get_name(),
            PLType::GENERIC(g) => {
                if g.curpltype.is_some() {
                    g.curpltype.as_ref().unwrap().borrow().get_name()
                } else {
                    g.name.clone()
                }
            }
            PLType::PLACEHOLDER(p) => p.name.clone(),
            PLType::TRAIT(t) => t.name.clone(),
        }
    }
    pub fn get_llvm_name<'a, 'ctx>(&self) -> String {
        match self {
            PLType::FN(fu) => fu.name.clone(),
            PLType::STRUCT(st) => st.name.clone(),
            PLType::TRAIT(t) => t.name.clone(),
            PLType::PRIMITIVE(pri) => pri.get_name(),
            PLType::ARR(arr) => {
                format!("[{} * {}]", arr.element_type.borrow().get_name(), arr.size)
            }
            PLType::VOID => "void".to_string(),
            PLType::POINTER(p) => "*".to_string() + &p.borrow().get_name(),
            PLType::GENERIC(g) => {
                if g.curpltype.is_some() {
                    g.curpltype.as_ref().unwrap().borrow().get_name()
                } else {
                    g.name.clone()
                }
            }
            PLType::PLACEHOLDER(p) => p.get_place_holder_name(),
        }
    }

    pub fn get_full_elm_name<'a, 'ctx>(&self) -> String {
        match self {
            PLType::GENERIC(g) => g.name.clone(),
            PLType::FN(fu) => fu.llvmname.clone(),
            PLType::STRUCT(st) => st.get_st_full_name(),
            PLType::TRAIT(st) => st.get_st_full_name(),
            PLType::PRIMITIVE(pri) => pri.get_name(),
            PLType::ARR(arr) => {
                format!(
                    "[{} * {}]",
                    arr.element_type.borrow().get_full_elm_name(),
                    arr.size
                )
            }
            PLType::VOID => "void".to_string(),
            PLType::POINTER(p) => p.borrow().get_full_elm_name(),
            PLType::PLACEHOLDER(p) => p.name.clone(),
        }
    }
    pub fn get_ptr_depth(&self) -> usize {
        match self {
            PLType::POINTER(p) => p.borrow().get_ptr_depth() + 1,
            _ => 0,
        }
    }

    /// # get_range
    /// get the defination range of the type
    pub fn get_range(&self) -> Option<Range> {
        match self {
            PLType::GENERIC(g) => Some(g.range.clone()),
            PLType::FN(f) => Some(f.range.clone()),
            PLType::STRUCT(s) => Some(s.range.clone()),
            PLType::ARR(_) => None,
            PLType::PRIMITIVE(_) => None,
            PLType::VOID => None,
            PLType::POINTER(_) => None,
            PLType::PLACEHOLDER(p) => Some(p.range.clone()),
            PLType::TRAIT(t) => Some(t.range.clone()),
        }
    }

    pub fn is_void(&self) -> bool {
        if let PLType::VOID = self {
            true
        } else {
            false
        }
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
    pub typenode: Box<TypeNodeEnum>,
    pub name: String,
    pub range: Range,
    // pub refs: Arc<RwVec<Location>>,
}

impl Field {
    pub fn get_doc_symbol<'a, 'ctx>(&self) -> DocumentSymbol {
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
pub struct FNType {
    pub name: String,     // name for lsp
    pub llvmname: String, // name in llvm ir
    pub param_pltypes: Vec<Box<TypeNodeEnum>>,
    pub param_names: Vec<String>,
    pub ret_pltype: Box<TypeNodeEnum>,
    pub range: Range,
    // pub refs: Arc<RwVec<Location>>,
    pub doc: Vec<Box<NodeEnum>>,
    pub method: bool,
    pub generic_map: IndexMap<String, Arc<RefCell<PLType>>>,
    pub generic_infer: Arc<RefCell<IndexMap<String, Arc<RefCell<PLType>>>>>,
    pub generic: bool,
    pub node: Box<FuncDefNode>,
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
    /// ???????????????????????????????????????????????????
    ///
    /// ??????????????????????????????receiver
    ///
    /// ???????????????????????????????????????*i64???????????????????????????????????????????????????
    pub fn eq_except_receiver<'a, 'ctx, 'b>(
        &self,
        other: &FNType,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> bool {
        if self.name.split("::").last().unwrap() != other.name.split("::").last().unwrap() {
            return false;
        }
        if self.param_pltypes.len() != other.param_pltypes.len() {
            return false;
        }
        for i in 1..self.param_pltypes.len() {
            if self.param_pltypes[i].get_type(ctx, builder)
                != other.param_pltypes[i].get_type(ctx, builder)
            {
                return false;
            }
        }
        self.ret_pltype.get_type(ctx, builder) == other.ret_pltype.get_type(ctx, builder)
    }
    pub fn append_name_with_generic(&self, name: String) -> String {
        if self.need_gen_code() {
            let typeinfer = self
                .generic_map
                .iter()
                .map(|(_, v)| match &*v.clone().borrow() {
                    PLType::GENERIC(g) => g.curpltype.as_ref().unwrap().borrow().get_llvm_name(),
                    _ => unreachable!(),
                })
                .collect::<Vec<_>>()
                .join(", ");
            return format!("{}<{}>", name, typeinfer);
        } else {
            name.clone()
        }
    }
    pub fn generic_infer_pltype<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<FNType, PLDiag> {
        let name = self.append_name_with_generic(self.name.clone());
        if let Some(pltype) = self.generic_infer.borrow().get(&name) {
            if let PLType::FN(f) = &*pltype.borrow() {
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
        res.generic_map.clear();
        res.generic_infer = Arc::new(RefCell::new(IndexMap::default()));
        self.generic_infer
            .borrow_mut()
            .insert(name, Arc::new(RefCell::new(PLType::FN(res.clone()))));

        let block = ctx.block;
        ctx.need_highlight = ctx.need_highlight + 1;
        self.node
            .gen_fntype(ctx, false, builder, Some(self.clone()))?;
        ctx.need_highlight = ctx.need_highlight - 1;
        ctx.position_at_end(block.unwrap(), builder);

        res.ret_pltype = self
            .ret_pltype
            .get_type(ctx, builder)
            .unwrap()
            .borrow()
            .get_typenode(ctx);
        res.param_pltypes = self
            .param_pltypes
            .iter()
            .map(|p| {
                let np = p.get_type(ctx, builder).unwrap().borrow().get_typenode(ctx);
                np
            })
            .collect::<Vec<Box<TypeNodeEnum>>>();
        let pltype = self
            .generic_infer
            .borrow()
            .get(&res.name)
            .as_ref()
            .unwrap()
            .clone()
            .clone();
        pltype.replace(PLType::FN(res.clone()));
        Ok(res.clone())
    }
    pub fn gen_snippet(&self) -> String {
        let mut name = self.name.clone();
        let mut iter = self.param_names.iter();
        if self.method {
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
            name: if self.method {
                self.name.split("::").last().unwrap().to_string()
            } else {
                self.name.clone()
            },
            detail: Some(self.get_signature()),
            kind: if self.method {
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
            if !self.method {
                params += &format!(
                    "{}: {}",
                    self.param_names[0],
                    FmtBuilder::generate_node(&self.param_pltypes[0])
                );
            }
            for i in 1..self.param_names.len() {
                params += &format!(
                    ", {}: {}",
                    self.param_names[i],
                    FmtBuilder::generate_node(&self.param_pltypes[i])
                );
            }
        }
        format!(
            "fn ({}) {}",
            params,
            FmtBuilder::generate_node(&self.ret_pltype)
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ARRType {
    pub element_type: Arc<RefCell<PLType>>,
    pub size: u32,
}

impl ARRType {
    pub fn get_elem_type<'a, 'ctx>(&'a self) -> Arc<RefCell<PLType>> {
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
    // pub refs: Arc<RwVec<Location>>,
    pub doc: Vec<Box<NodeEnum>>,
    pub generic_map: IndexMap<String, Arc<RefCell<PLType>>>,
    pub impls: FxHashMap<String, Arc<RefCell<PLType>>>,
    pub derives: Vec<Arc<RefCell<PLType>>>,
}

impl STType {
    pub fn implements(&self, tp: &PLType) -> bool {
        self.impls.get(&tp.get_full_elm_name()).is_some()
    }
    pub fn implements_trait(&self, tp: &STType) -> bool {
        let re = self.impls.get(&tp.get_st_full_name()).is_some();
        if !re {
            return re;
        }
        for de in &tp.derives {
            let re = self.implements(&de.borrow());
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
        if self.need_gen_code() {
            let typeinfer = self
                .generic_map
                .iter()
                .map(|(_, v)| match &*v.clone().borrow() {
                    PLType::GENERIC(g) => g.curpltype.as_ref().unwrap().borrow().get_name(),
                    _ => unreachable!(),
                })
                .collect::<Vec<_>>()
                .join(", ");
            return format!("{}<{}>", self.name, typeinfer);
        }
        unreachable!()
    }
    pub fn generic_infer_pltype<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> STType {
        let name = self.append_name_with_generic();
        if let Ok(pltype) = ctx.get_type(&name, Default::default()) {
            match &*pltype.borrow() {
                PLType::STRUCT(st) => {
                    return st.clone();
                }
                _ => unreachable!(),
            }
        }
        let mut res = self.clone();
        res.name = name;
        ctx.add_type_without_check(Arc::new(RefCell::new(PLType::STRUCT(res.clone()))));
        res.ordered_fields = self
            .ordered_fields
            .iter()
            .map(|f| {
                let mut nf = f.clone();
                nf.typenode = f
                    .typenode
                    .get_type(ctx, builder)
                    .unwrap()
                    .borrow()
                    .get_typenode(ctx);
                nf
            })
            .collect::<Vec<Field>>();
        res.ordered_fields.iter().for_each(|f| {
            res.fields.insert(f.name.clone(), f.clone());
        });
        res.generic_map.clear();
        let pltype = ctx.get_type(&res.name, Default::default()).unwrap();
        pltype.replace(PLType::STRUCT(res.clone()));
        res
    }
    pub fn get_field_completions(&self) -> Vec<CompletionItem> {
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
    pub fn get_mthd_completions<'a, 'ctx>(&self, ctx: &Ctx<'a>) -> Vec<CompletionItem> {
        ctx.plmod.get_methods_completions(&self.get_st_full_name())
    }

    pub fn get_completions<'a, 'ctx>(&self, ctx: &Ctx<'a>) -> Vec<CompletionItem> {
        let mut coms = self.get_field_completions();
        coms.extend(self.get_mthd_completions(ctx));
        coms
    }
    pub fn get_trait_completions<'a, 'ctx>(&self, ctx: &Ctx<'a>) -> Vec<CompletionItem> {
        let mut coms = self.get_trait_field_completions();
        coms.extend(self.get_mthd_completions(ctx));
        coms
    }
    pub fn get_trait_field_completions<'a, 'ctx>(&self) -> Vec<CompletionItem> {
        let mut completions = Vec::new();
        for (name, f) in &self.fields {
            if let TypeNodeEnum::FuncTypeNode(func) = &*f.typenode {
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
    pub fn find_method<'a, 'ctx>(&self, ctx: &Ctx<'a>, method: &str) -> Option<FNType> {
        ctx.plmod.find_method(&self.get_st_full_name(), method)
    }
    pub fn get_st_full_name(&self) -> String {
        format!("{}..{}", self.path, self.name)
    }
    pub fn get_doc_symbol<'a, 'ctx>(&self) -> DocumentSymbol {
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

pub fn add_primitive_types<'a, 'ctx>(ctx: &mut Ctx<'a>) {
    let pltype_i128 = PLType::PRIMITIVE(PriType::I128);
    ctx.plmod.types.insert(
        "i128".to_string(),
        Arc::new(RefCell::new(pltype_i128.clone())),
    );

    let pltype_i64 = PLType::PRIMITIVE(PriType::I64);
    ctx.plmod.types.insert(
        "i64".to_string(),
        Arc::new(RefCell::new(pltype_i64.clone())),
    );

    let pltype_i32 = PLType::PRIMITIVE(PriType::I32);
    ctx.plmod.types.insert(
        "i32".to_string(),
        Arc::new(RefCell::new(pltype_i32.clone())),
    );

    let pltype_i16 = PLType::PRIMITIVE(PriType::I16);
    ctx.plmod.types.insert(
        "i16".to_string(),
        Arc::new(RefCell::new(pltype_i16.clone())),
    );

    let pltype_i8 = PLType::PRIMITIVE(PriType::I8);
    ctx.plmod
        .types
        .insert("i8".to_string(), Arc::new(RefCell::new(pltype_i8.clone())));

    let pltype_u128 = PLType::PRIMITIVE(PriType::U128);
    ctx.plmod.types.insert(
        "u128".to_string(),
        Arc::new(RefCell::new(pltype_u128.clone())),
    );

    let pltype_u64 = PLType::PRIMITIVE(PriType::U64);
    ctx.plmod.types.insert(
        "u64".to_string(),
        Arc::new(RefCell::new(pltype_u64.clone())),
    );

    let pltype_u32 = PLType::PRIMITIVE(PriType::U32);
    ctx.plmod.types.insert(
        "u32".to_string(),
        Arc::new(RefCell::new(pltype_u32.clone())),
    );

    let pltype_u16 = PLType::PRIMITIVE(PriType::U16);
    ctx.plmod.types.insert(
        "u16".to_string(),
        Arc::new(RefCell::new(pltype_u16.clone())),
    );

    let pltype_u8 = PLType::PRIMITIVE(PriType::U8);
    ctx.plmod
        .types
        .insert("u8".to_string(), Arc::new(RefCell::new(pltype_u8.clone())));

    let pltype_f64 = PLType::PRIMITIVE(PriType::F64);
    ctx.plmod.types.insert(
        "f64".to_string(),
        Arc::new(RefCell::new(pltype_f64.clone())),
    );

    let pltype_f32 = PLType::PRIMITIVE(PriType::F32);
    ctx.plmod.types.insert(
        "f32".to_string(),
        Arc::new(RefCell::new(pltype_f32.clone())),
    );

    let pltype_bool = PLType::PRIMITIVE(PriType::BOOL);
    ctx.plmod.types.insert(
        "bool".to_string(),
        Arc::new(RefCell::new(pltype_bool.clone())),
    );

    let pltype_void = PLType::VOID;
    ctx.plmod.types.insert(
        "void".to_string(),
        Arc::new(RefCell::new(pltype_void.clone())),
    );
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericType {
    pub name: String,
    pub range: Range,
    pub curpltype: Option<Arc<RefCell<PLType>>>,
}
impl GenericType {
    pub fn set_type(&mut self, pltype: Arc<RefCell<PLType>>) {
        self.curpltype = Some(pltype);
    }
    pub fn clear_type(&mut self) {
        self.curpltype = None;
    }
    pub fn set_place_holder(&mut self, ctx: &mut Ctx) {
        let range = self.range;
        let p = PlaceHolderType {
            name: self.name.clone(),
            range,
        };
        let name_in_map = p.get_place_holder_name();
        let pltype = Arc::new(RefCell::new(PLType::PLACEHOLDER(p)));
        self.curpltype = Some(pltype.clone());
        ctx.add_type(name_in_map, pltype, range).unwrap();
    }
}
macro_rules! generic_impl {
    ($($args:ident),*) => (
        $(
            impl $args {
                pub fn need_gen_code(&self) -> bool {
                    if self.generic_map.is_empty() {
                        return false;
                    }
                    for (_, v) in self.generic_map.iter() {
                        match &*v.clone().borrow() {
                            PLType::GENERIC(g) => {
                                if g.curpltype.is_none() {
                                    return false;
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                    true
                }
                pub fn clear_generic(&mut self) {
                    self.generic_map
                        .iter_mut()
                        .for_each(|(_, v)| match &mut *v.clone().borrow_mut() {
                            PLType::GENERIC(g) => {
                                g.clear_type();
                            }
                            _ => unreachable!(),
                        })
                }
                pub fn add_generic_type(&self, ctx: &mut Ctx) -> Result<(), PLDiag> {
                    for (name, g) in self.generic_map.iter() {
                        ctx.add_generic_type(
                            name.clone(),
                            g.clone(),
                            (&*g.clone().borrow()).get_range().unwrap(),
                        );
                    }
                    Ok(())
                }
                pub fn new_pltype(&self) -> $args {
                    let mut res = self.clone();
                    res.generic_map = res
                        .generic_map
                        .iter()
                        .map(|(k, pltype)| {
                            if let PLType::GENERIC(g) = &*pltype.borrow() {
                                return (k.clone(), Arc::new(RefCell::new(PLType::GENERIC(g.clone()))));
                            }
                            unreachable!()
                        })
                        .collect::<IndexMap<String, Arc<RefCell<PLType>>>>();
                    res.clear_generic();
                    res
                }
            }
        )*
    );
}
generic_impl!(FNType, STType);
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
