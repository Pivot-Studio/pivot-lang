use super::ctx::Ctx;
use super::ctx::MemberType;
use super::ctx::PLDiag;
use super::diag::ErrorCode;
use super::node::function::FuncDefNode;
use super::node::pkg::ExternIDNode;
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
use inkwell::debug_info::*;
use inkwell::module::Linkage;
use inkwell::types::ArrayType;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::types::FunctionType;
use inkwell::types::StructType;
use inkwell::types::VoidType;
use inkwell::values::FunctionValue;
use inkwell::AddressSpace;
use lsp_types::Command;
use lsp_types::CompletionItem;
use lsp_types::CompletionItemKind;
use lsp_types::DocumentSymbol;
use lsp_types::InsertTextFormat;
use lsp_types::Location;
use lsp_types::SymbolKind;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;
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
    POINTER(Rc<RefCell<PLType>>),
    GENERIC(GenericType),
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

fn new_typename_node(name: &str) -> Box<TypeNodeEnum> {
    Box::new(TypeNodeEnum::BasicTypeNode(TypeNameNode {
        id: Some(ExternIDNode {
            ns: vec![],
            id: Box::new(VarNode {
                name: name.to_string(),
                range: Default::default(),
            }),
            complete: true,
            singlecolon: false,
            range: Default::default(),
        }),
        range: Default::default(),
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
pub fn eq_or_infer(l: Rc<RefCell<PLType>>, r: Rc<RefCell<PLType>>) -> bool {
    match &mut *l.borrow_mut() {
        PLType::GENERIC(l) => {
            l.set_type(r.clone());
            return true;
        }
        _ => {}
    }
    match (&*l.borrow(), &*r.borrow()) {
        (PLType::PRIMITIVE(l), PLType::PRIMITIVE(r)) => l == r,
        (PLType::VOID, PLType::VOID) => true,
        (PLType::POINTER(l), PLType::POINTER(r)) => eq_or_infer(l.clone(), r.clone()),
        (PLType::ARR(l), PLType::ARR(r)) => {
            eq_or_infer(l.get_elem_type(), r.get_elem_type()) && l.size == r.size
        }
        (PLType::STRUCT(l), PLType::STRUCT(r)) => l.name == r.name && l.path == r.path,
        (PLType::FN(l), PLType::FN(r)) => l == r,
        _ => false,
    }
}
impl PLType {
    pub fn get_typenode(&self, ctx: &Ctx) -> Box<TypeNodeEnum> {
        match self {
            PLType::STRUCT(st) => {
                if st.path == ctx.plmod.path {
                    new_typename_node(&st.name)
                } else {
                    todo!()
                }
            }
            PLType::ARR(arr) => new_arrtype_node(
                arr.get_elem_type().borrow().get_typenode(ctx),
                arr.size as u64,
            ),
            PLType::PRIMITIVE(p) => new_typename_node(&p.get_name()),
            PLType::VOID => new_typename_node("void"),
            PLType::POINTER(p) => new_ptrtype_node(p.borrow().get_typenode(ctx)),
            PLType::GENERIC(g) => {
                if !g.curpltype.is_none() {
                    g.curpltype.as_ref().unwrap().borrow().get_typenode(ctx)
                } else {
                    unreachable!()
                }
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
            PLType::GENERIC(_) => None,
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
            PLType::ARR(arr) => {
                format!("[{} * {}]", arr.element_type.borrow().get_name(), arr.size)
            }
            PLType::VOID => "void".to_string(),
            PLType::POINTER(p) => "*".to_string() + &p.borrow().get_name(),
            PLType::GENERIC(g) => g.name.clone(),
        }
    }

    pub fn get_full_elm_name<'a, 'ctx>(&self) -> String {
        match self {
            PLType::GENERIC(g) => g.name.clone(),
            PLType::FN(fu) => fu.name.clone(),
            PLType::STRUCT(st) => st.get_st_full_name(),
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
        }
    }

    /// # get_basic_type_op
    /// get the basic type of the type
    /// used in code generation
    pub fn get_basic_type_op<'a, 'ctx>(&self, ctx: &Ctx<'a, 'ctx>) -> Option<BasicTypeEnum<'ctx>> {
        match self {
            PLType::GENERIC(g) => match &g.curpltype {
                Some(pltype) => pltype.borrow().get_basic_type_op(ctx),
                None => Some({
                    let name = &format!("__generic__{}", g.name);
                    ctx.module
                        .get_struct_type(name)
                        .or(Some({
                            let st = ctx
                                .context
                                .opaque_struct_type(&format!("__generic__{}", g.name));
                            st.set_body(&[], false);
                            st
                        }))
                        .unwrap()
                        .into()
                }),
            },
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
                p.borrow()
                    .get_basic_type(ctx)
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
            PLType::GENERIC(g) => {
                if g.curpltype.is_some() {
                    let pltype = g.curpltype.as_ref().unwrap();
                    pltype.clone().borrow().get_ditype(ctx)
                } else {
                    PLType::PRIMITIVE(PriType::I64).get_ditype(ctx)
                }
            }
            PLType::ARR(arr) => {
                let elemdi = arr.element_type.borrow().get_ditype(ctx)?;
                let etp = &arr.element_type.borrow().get_basic_type(ctx);
                let size = td.get_bit_size(etp) * arr.size as u64;
                let align = td.get_preferred_alignment(etp);
                Some(
                    ctx.dibuilder
                        .create_array_type(elemdi, size, align, &[(0..arr.size as i64)])
                        .as_type(),
                )
            }
            PLType::STRUCT(x) => {
                // 若已经生成过，直接查表返回
                if RefCell::borrow(&ctx.ditypes).contains_key(&x.get_st_full_name()) {
                    return Some(
                        RefCell::borrow(&ctx.ditypes)
                            .get(&x.get_st_full_name())
                            .unwrap()
                            .clone(),
                    );
                }
                let mut offset = 0;
                // 生成占位符，为循环引用做准备
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
                        ctx.diunit.get_file().as_debug_info_scope(),
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
                // 替换循环引用生成的占位符
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
                let elemdi = p.borrow().get_ditype(ctx)?;
                let etp = &p
                    .borrow()
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
    pub typenode: Box<TypeNodeEnum>,
    pub name: String,
    pub range: Range,
    pub refs: Rc<RefCell<Vec<Location>>>,
}

impl Field {
    pub fn get_di_type<'a, 'ctx>(&self, ctx: &Ctx<'a, 'ctx>, offset: u64) -> (DIType<'ctx>, u64) {
        let pltp = self.typenode.get_type(ctx).unwrap();
        let depth = RefCell::borrow(&pltp).get_ptr_depth();
        if let Some(x) = ctx
            .ditypes_placeholder
            .borrow_mut()
            .get(&*RefCell::borrow(&pltp).get_full_elm_name())
        {
            if !matches!(*RefCell::borrow(&pltp), PLType::POINTER(_)) {
                // 出现循环引用，但是不是指针
                // TODO 应该只需要一层是指针就行，目前的检查要求每一层都是指针
                ctx.add_err(self.range, ErrorCode::ILLEGAL_SELF_RECURSION);
            }
            let placeholder = unsafe { ctx.dibuilder.create_placeholder_derived_type(ctx.context) };
            let td = ctx.targetmachine.get_target_data();
            let etp = ctx.context.i8_type().ptr_type(AddressSpace::Generic);
            let size = td.get_bit_size(&etp);
            x.borrow_mut().push(MemberType {
                ditype: placeholder,
                offset,
                scope: ctx.diunit.get_file().as_debug_info_scope(),
                line: self.range.start.line as u32,
                name: self.name.clone(),
                di_file: ctx.diunit.get_file(),
                ptr_depth: depth,
            });
            return (placeholder.as_type(), offset + size);
        }
        let di_type = RefCell::borrow(&pltp).get_ditype(ctx);
        let debug_type = di_type.unwrap();
        (
            ctx.dibuilder
                .create_member_type(
                    ctx.diunit.get_file().as_debug_info_scope(),
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
    pub fn get_doc_symbol<'a, 'ctx>(&self, ctx: &Ctx<'a, 'ctx>) -> DocumentSymbol {
        #[allow(deprecated)]
        DocumentSymbol {
            name: self.name.clone(),
            detail: Some(RefCell::borrow(&self.typenode.get_type(ctx).unwrap()).get_name()),
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
    pub param_pltypes: Vec<Rc<RefCell<PLType>>>,
    pub param_names: Vec<String>,
    pub ret_pltype: Rc<RefCell<PLType>>,
    pub range: Range,
    pub refs: Rc<RefCell<Vec<Location>>>,
    pub doc: Vec<Box<NodeEnum>>,
    pub method: bool,
    pub generic_map: IndexMap<String, Rc<RefCell<PLType>>>,
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
    pub fn append_name_with_generic(&self, name: String) -> String {
        if self.need_gen_code() {
            let mut newname = format!("{}", name);
            for (_, v) in self.generic_map.iter() {
                match &*v.clone().borrow() {
                    PLType::GENERIC(g) => {
                        newname.push_str(
                            format!("__{}", g.curpltype.as_ref().unwrap().borrow().get_name())
                                .as_str(),
                        );
                    }
                    _ => unreachable!(),
                }
            }
            newname.clone()
        } else {
            name.clone()
        }
    }
    /// try get function value from module
    ///
    /// if not found, create a declaration
    pub fn get_or_insert_fn<'a, 'ctx>(&self, ctx: &Ctx<'a, 'ctx>) -> FunctionValue<'ctx> {
        let llvmname = self.append_name_with_generic(self.llvmname.clone());
        if let Some(v) = ctx.module.get_function(&llvmname) {
            return v;
        }
        let mut param_types = vec![];
        for param_pltype in self.param_pltypes.iter() {
            param_types.push(RefCell::borrow(&param_pltype).get_basic_type(ctx).into());
        }
        let fn_type = RefCell::borrow(&self.ret_pltype)
            .get_ret_type(ctx)
            .fn_type(&param_types, false);
        let fn_value = ctx
            .module
            .add_function(&llvmname, fn_type, Some(Linkage::External));
        fn_value
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
                    RefCell::borrow(&self.param_pltypes[0]).get_name()
                );
            }
            for i in 1..self.param_names.len() {
                params += &format!(
                    ", {}: {}",
                    self.param_names[i],
                    RefCell::borrow(&self.param_pltypes[i]).get_name()
                );
            }
        }
        format!(
            "fn ({}) {}",
            params,
            RefCell::borrow(&self.ret_pltype).get_name()
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ARRType {
    pub element_type: Rc<RefCell<PLType>>,
    pub size: u32,
}

impl ARRType {
    pub fn arr_type<'a, 'ctx>(&'a self, ctx: &Ctx<'a, 'ctx>) -> ArrayType<'ctx> {
        self.element_type
            .borrow()
            .get_basic_type(ctx)
            .array_type(self.size)
    }
    pub fn get_elem_type<'a, 'ctx>(&'a self) -> Rc<RefCell<PLType>> {
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
    pub generic_map: IndexMap<String, Rc<RefCell<PLType>>>,
}

impl STType {
    pub fn generic_name(&self) -> String {
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
    pub fn generic_infer_pltype(&self, ctx: &Ctx) -> STType {
        let mut res = self.clone();
        res.name = self.generic_name();
        res.ordered_fields = self
            .ordered_fields
            .iter()
            .map(|f| {
                let mut nf = f.clone();
                nf.typenode = f.typenode.get_type(ctx).unwrap().borrow().get_typenode(ctx);
                nf
            })
            .collect::<Vec<Field>>();
        res.ordered_fields.iter().for_each(|f| {
            res.fields.insert(f.name.clone(), f.clone());
        });
        res.generic_map.clear();
        res
    }
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
                    RefCell::borrow(&order_field.typenode.get_type(ctx).unwrap())
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
        for (name, v) in &self.methods {
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
        }
        completions
    }
    pub fn get_st_full_name(&self) -> String {
        format!("{}..{}", self.path, self.name)
    }
    pub fn get_doc_symbol<'a, 'ctx>(&self, ctx: &Ctx<'a, 'ctx>) -> DocumentSymbol {
        let children: Vec<DocumentSymbol> = self
            .ordered_fields
            .iter()
            .map(|order_field| order_field.get_doc_symbol(ctx))
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

pub fn add_primitive_types<'a, 'ctx>(ctx: &mut Ctx<'a, 'ctx>) {
    let pltype_i128 = PLType::PRIMITIVE(PriType::I128);
    ctx.plmod.types.insert(
        "i128".to_string(),
        Rc::new(RefCell::new(pltype_i128.clone())),
    );

    let pltype_i64 = PLType::PRIMITIVE(PriType::I64);
    ctx.plmod
        .types
        .insert("i64".to_string(), Rc::new(RefCell::new(pltype_i64.clone())));

    let pltype_i32 = PLType::PRIMITIVE(PriType::I32);
    ctx.plmod
        .types
        .insert("i32".to_string(), Rc::new(RefCell::new(pltype_i32.clone())));

    let pltype_i16 = PLType::PRIMITIVE(PriType::I16);
    ctx.plmod
        .types
        .insert("i16".to_string(), Rc::new(RefCell::new(pltype_i16.clone())));

    let pltype_i8 = PLType::PRIMITIVE(PriType::I8);
    ctx.plmod
        .types
        .insert("i8".to_string(), Rc::new(RefCell::new(pltype_i8.clone())));

    let pltype_u128 = PLType::PRIMITIVE(PriType::U128);
    ctx.plmod.types.insert(
        "u128".to_string(),
        Rc::new(RefCell::new(pltype_u128.clone())),
    );

    let pltype_u64 = PLType::PRIMITIVE(PriType::U64);
    ctx.plmod
        .types
        .insert("u64".to_string(), Rc::new(RefCell::new(pltype_u64.clone())));

    let pltype_u32 = PLType::PRIMITIVE(PriType::U32);
    ctx.plmod
        .types
        .insert("u32".to_string(), Rc::new(RefCell::new(pltype_u32.clone())));

    let pltype_u16 = PLType::PRIMITIVE(PriType::U16);
    ctx.plmod
        .types
        .insert("u16".to_string(), Rc::new(RefCell::new(pltype_u16.clone())));

    let pltype_u8 = PLType::PRIMITIVE(PriType::U8);
    ctx.plmod
        .types
        .insert("u8".to_string(), Rc::new(RefCell::new(pltype_u8.clone())));

    let pltype_f64 = PLType::PRIMITIVE(PriType::F64);
    ctx.plmod
        .types
        .insert("f64".to_string(), Rc::new(RefCell::new(pltype_f64.clone())));

    let pltype_f32 = PLType::PRIMITIVE(PriType::F32);
    ctx.plmod
        .types
        .insert("f32".to_string(), Rc::new(RefCell::new(pltype_f32.clone())));

    let pltype_bool = PLType::PRIMITIVE(PriType::BOOL);
    ctx.plmod.types.insert(
        "bool".to_string(),
        Rc::new(RefCell::new(pltype_bool.clone())),
    );

    let pltype_void = PLType::VOID;
    ctx.plmod.types.insert(
        "void".to_string(),
        Rc::new(RefCell::new(pltype_void.clone())),
    );
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericType {
    pub name: String,
    pub range: Range,
    pub curpltype: Option<Rc<RefCell<PLType>>>,
}
impl GenericType {
    pub fn set_type(&mut self, pltype: Rc<RefCell<PLType>>) {
        self.curpltype = Some(pltype);
    }
    pub fn clear_type(&mut self) {
        self.curpltype = None;
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
                        ctx.add_type(
                            name.clone(),
                            g.clone(),
                            (&*g.clone().borrow()).get_range().unwrap(),
                        )?;
                    }
                    Ok(())
                }
            }
        )*
    );
}
generic_impl!(FNType, STType);
