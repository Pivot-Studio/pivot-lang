/// 此包代码应该遵循以下原则：
/// 1. 所有Builder的字段都应该private，不应该被外部直接访问
/// 2. 所有涉及llvm类型的函数（包括参数或返回值）都应该是private的
use std::{
    cell::{Cell, RefCell},
    path::Path,
    sync::Arc,
};

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    debug_info::*,
    module::{FlagBehavior, Linkage, Module},
    targets::{InitializationConfig, Target, TargetMachine},
    types::{ArrayType, BasicType, BasicTypeEnum, FunctionType, StructType},
    values::{
        AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallableValue,
        FunctionValue, PointerValue,
    },
    AddressSpace, FloatPredicate, IntPredicate, OptimizationLevel,
};
use rustc_hash::FxHashMap;

use crate::ast::diag::PLDiag;

use super::{
    super::{
        ctx::Ctx,
        diag::ErrorCode,
        node::{types::TypedIdentifierNode, TypeNode, TypeNodeEnum},
        pltype::{ARRType, FNType, Field, PLType, PriType, RetTypeEnum, STType},
        range::{Pos, Range},
    },
    IRBuilder,
};

use super::BlockHandle;
use super::ValueHandle;

// TODO: match all case
// const DW_ATE_UTF: u32 = 0x10;
const DW_ATE_BOOLEAN: u32 = 0x02;
const DW_ATE_FLOAT: u32 = 0x04;
const DW_ATE_SIGNED: u32 = 0x05;
const DW_ATE_UNSIGNED: u32 = 0x07;
// const DW_TAG_REFERENCE_TYPE: u32 = 16;
fn get_dw_ate_encoding<'a, 'ctx>(pritp: &PriType) -> u32 {
    match pritp {
        PriType::I8 | PriType::I16 | PriType::I32 | PriType::I64 | PriType::I128 => DW_ATE_SIGNED,
        PriType::U8 | PriType::U16 | PriType::U32 | PriType::U64 | PriType::U128 => DW_ATE_UNSIGNED,
        PriType::F32 | PriType::F64 => DW_ATE_FLOAT,
        PriType::BOOL => DW_ATE_BOOLEAN,
    }
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

pub fn create_llvm_deps<'ctx>(
    context: &'ctx Context,
    dir: &str,
    file: &str,
) -> (
    Module<'ctx>,
    Builder<'ctx>,
    DebugInfoBuilder<'ctx>,
    DICompileUnit<'ctx>,
    TargetMachine,
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
    module.set_triple(&tm.get_triple());
    module.set_data_layout(&tm.get_target_data().get_data_layout());
    (module, builder, dibuilder, compile_unit, tm)
}

#[derive(Clone)]
pub struct LLVMBuilder<'a, 'ctx> {
    handle_table: Arc<RefCell<FxHashMap<ValueHandle, AnyValueEnum<'ctx>>>>,
    handle_reverse_table: Arc<RefCell<FxHashMap<AnyValueEnum<'ctx>, ValueHandle>>>,
    block_table: Arc<RefCell<FxHashMap<BlockHandle, BasicBlock<'ctx>>>>,
    block_reverse_table: Arc<RefCell<FxHashMap<BasicBlock<'ctx>, BlockHandle>>>,
    context: &'ctx Context,                // llvm context
    builder: &'a Builder<'ctx>,            // llvm builder
    module: &'a Module<'ctx>,              // llvm module
    dibuilder: &'a DebugInfoBuilder<'ctx>, // debug info builder
    diunit: &'a DICompileUnit<'ctx>,       // debug info unit
    targetmachine: &'a TargetMachine,      // might be used in debug info
    discope: Cell<DIScope<'ctx>>,          // debug info scope
    ditypes_placeholder: Arc<RefCell<FxHashMap<String, RefCell<Vec<MemberType<'ctx>>>>>>, // hold the generated debug info type place holder
    ditypes: Arc<RefCell<FxHashMap<String, DIType<'ctx>>>>, // hold the generated debug info type
}

pub fn get_target_machine(level: OptimizationLevel) -> TargetMachine {
    let triple = &TargetMachine::get_default_triple();
    let s1 = TargetMachine::get_host_cpu_name();
    let cpu = s1.to_str().unwrap();
    let s2 = TargetMachine::get_host_cpu_features();
    let features = s2.to_str().unwrap();
    Target::initialize_native(&InitializationConfig::default()).unwrap();
    let target = Target::from_triple(triple).unwrap();
    target
        .create_target_machine(
            triple,
            cpu,
            features,
            level,
            inkwell::targets::RelocMode::Static,
            inkwell::targets::CodeModel::Default,
        )
        .unwrap()
}

impl<'a, 'ctx> LLVMBuilder<'a, 'ctx> {
    pub fn new(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
        dibuilder: &'a DebugInfoBuilder<'ctx>,
        diunit: &'a DICompileUnit<'ctx>,
        tm: &'a TargetMachine,
    ) -> Self {
        module.set_triple(&TargetMachine::get_default_triple());
        Self {
            context,
            builder,
            module,
            dibuilder,
            diunit,
            targetmachine: tm,
            discope: Cell::new(diunit.get_file().as_debug_info_scope()),
            ditypes: Arc::new(RefCell::new(FxHashMap::default())),
            ditypes_placeholder: Arc::new(RefCell::new(FxHashMap::default())),
            handle_table: Arc::new(RefCell::new(FxHashMap::default())),
            handle_reverse_table: Arc::new(RefCell::new(FxHashMap::default())),
            block_table: Arc::new(RefCell::new(FxHashMap::default())),
            block_reverse_table: Arc::new(RefCell::new(FxHashMap::default())),
        }
    }
    fn get_llvm_value_handle(&self, value: &AnyValueEnum<'ctx>) -> ValueHandle {
        let len = self.handle_table.borrow().len();
        let nh = match self.handle_reverse_table.borrow().get(value) {
            Some(handle) => *handle,
            None => len,
        };
        if nh == len {
            self.handle_table.borrow_mut().insert(nh, value.clone());
            self.handle_reverse_table
                .borrow_mut()
                .insert(value.clone(), nh);
        }
        nh
    }
    fn get_llvm_value(&self, handle: ValueHandle) -> Option<AnyValueEnum<'ctx>> {
        match self.handle_table.borrow().get(&handle) {
            Some(value) => Some(value.clone()),
            None => None,
        }
    }
    fn get_llvm_block_handle(&self, block: BasicBlock<'ctx>) -> BlockHandle {
        let len = self.block_table.borrow().len();
        let nh = match self.block_reverse_table.borrow().get(&block) {
            Some(handle) => *handle,
            None => len,
        };
        if nh == len {
            self.block_table.borrow_mut().insert(nh, block);
            self.block_reverse_table.borrow_mut().insert(block, nh);
        }
        nh
    }
    fn get_llvm_block(&self, handle: BlockHandle) -> Option<BasicBlock<'ctx>> {
        match self.block_table.borrow().get(&handle) {
            Some(block) => Some(*block),
            None => None,
        }
    }
    fn get_pri_basic_type(&self, tp: &PriType) -> BasicTypeEnum<'ctx> {
        match tp {
            PriType::I8 => self.context.i8_type().into(),
            PriType::I16 => self.context.i16_type().into(),
            PriType::I32 => self.context.i32_type().into(),
            PriType::I64 => self.context.i64_type().as_basic_type_enum(),
            PriType::I128 => self.context.i128_type().as_basic_type_enum(),
            PriType::U8 => self.context.i8_type().as_basic_type_enum(),
            PriType::U16 => self.context.i16_type().as_basic_type_enum(),
            PriType::U32 => self.context.i32_type().as_basic_type_enum(),
            PriType::U64 => self.context.i64_type().as_basic_type_enum(),
            PriType::U128 => self.context.i128_type().as_basic_type_enum(),
            PriType::F32 => self.context.f32_type().as_basic_type_enum(),
            PriType::F64 => self.context.f64_type().as_basic_type_enum(),
            PriType::BOOL => self.context.bool_type().as_basic_type_enum(),
        }
    }

    fn get_fn_type(&self, f: &FNType, ctx: &mut Ctx<'a>) -> FunctionType<'ctx> {
        let mut param_types = vec![];
        for param_pltype in f.param_pltypes.iter() {
            param_types.push(
                self.get_basic_type_op(
                    &param_pltype
                        .get_type(ctx, &self.clone().into())
                        .unwrap()
                        .borrow(),
                    ctx,
                )
                .unwrap()
                .into(),
            );
        }
        let fn_type = self
            .get_ret_type(
                &f.ret_pltype
                    .get_type(ctx, &self.clone().into())
                    .unwrap()
                    .borrow(),
                ctx,
            )
            .fn_type(&param_types, false);
        fn_type
    }
    /// # get_basic_type_op
    /// get the basic type of the type
    /// used in code generation
    fn get_basic_type_op(&self, pltp: &PLType, ctx: &mut Ctx<'a>) -> Option<BasicTypeEnum<'ctx>> {
        match pltp {
            PLType::GENERIC(g) => match &g.curpltype {
                Some(pltype) => self.get_basic_type_op(&pltype.borrow(), ctx),
                None => Some({
                    let name = &format!("__generic__{}", g.name);
                    self.module
                        .get_struct_type(name)
                        .or(Some({
                            let st = self
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
                self.get_fn_type(f, ctx)
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum(),
            ),
            PLType::STRUCT(s) => Some(self.struct_type(s, ctx).as_basic_type_enum()),
            PLType::TRAIT(s) => Some(self.struct_type(s, ctx).as_basic_type_enum()),
            PLType::ARR(a) => Some(self.arr_type(a, ctx).as_basic_type_enum()),
            PLType::PRIMITIVE(t) => Some(self.get_pri_basic_type(t)),
            PLType::VOID => None,
            PLType::POINTER(p) => Some(
                self.get_basic_type_op(&p.borrow(), ctx)
                    .unwrap()
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum(),
            ),
            PLType::PLACEHOLDER(p) => Some({
                let name = &format!("__placeholder__{}", p.name);
                self.module
                    .get_struct_type(name)
                    .or(Some({
                        let st = self
                            .context
                            .opaque_struct_type(&format!("__placeholder__{}", p.name));
                        st.set_body(&[], false);
                        st
                    }))
                    .unwrap()
                    .into()
            }),
        }
    }
    /// # get_ret_type
    /// get the return type, which is void type or primitive type
    fn get_ret_type(&self, pltp: &PLType, ctx: &mut Ctx<'a>) -> RetTypeEnum<'ctx> {
        match pltp {
            PLType::VOID => RetTypeEnum::VOID(self.context.void_type()),
            _ => RetTypeEnum::BASIC(self.get_basic_type_op(pltp, ctx).unwrap()),
        }
    }
    fn arr_type(&self, arrtp: &ARRType, ctx: &mut Ctx<'a>) -> ArrayType<'ctx> {
        self.get_basic_type_op(&arrtp.element_type.borrow(), ctx)
            .unwrap()
            .array_type(arrtp.size)
    }

    fn get_field_di_type(
        &self,
        field: &Field,
        ctx: &mut Ctx<'a>,
        offset: u64,
    ) -> (DIType<'ctx>, u64) {
        let field_pltype = match field.typenode.get_type(ctx, &self.clone().into()) {
            Ok(field_pltype) => field_pltype,
            Err(_) => ctx.get_type("i64", Default::default()).unwrap(),
        };
        let depth = RefCell::borrow(&field_pltype).get_ptr_depth();
        if let Some(x) = self
            .ditypes_placeholder
            .borrow_mut()
            .get(&*RefCell::borrow(&field_pltype).get_full_elm_name())
        {
            if !matches!(*RefCell::borrow(&field_pltype), PLType::POINTER(_)) {
                // 出现循环引用，但是不是指针
                // TODO 应该只需要一层是指针就行，目前的检查要求每一层都是指针
                ctx.add_diag(field.range.new_err(ErrorCode::ILLEGAL_SELF_RECURSION));
            }
            let placeholder =
                unsafe { self.dibuilder.create_placeholder_derived_type(self.context) };
            let td = self.targetmachine.get_target_data();
            let etp = self.context.i8_type().ptr_type(AddressSpace::default());
            let size = td.get_bit_size(&etp);
            x.borrow_mut().push(MemberType {
                ditype: placeholder,
                offset,
                scope: self.diunit.get_file().as_debug_info_scope(),
                line: field.range.start.line as u32,
                name: field.name.clone(),
                di_file: self.diunit.get_file(),
                ptr_depth: depth,
            });
            return (placeholder.as_type(), offset + size);
        }
        let di_type = self.get_ditype(&field_pltype.borrow(), ctx);
        let debug_type = di_type.unwrap();
        (
            self.dibuilder
                .create_member_type(
                    self.diunit.get_file().as_debug_info_scope(),
                    &field.name,
                    self.diunit.get_file(),
                    field.range.start.line as u32,
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

    /// # get_ditype
    /// get the debug info type of the pltype
    fn get_ditype(&self, pltp: &PLType, ctx: &mut Ctx<'a>) -> Option<DIType<'ctx>> {
        let td = self.targetmachine.get_target_data();
        match pltp {
            PLType::FN(_) => self.get_ditype(&PLType::PRIMITIVE(PriType::I64), ctx),
            PLType::GENERIC(g) => {
                if g.curpltype.is_some() {
                    let pltype = g.curpltype.as_ref().unwrap();
                    self.get_ditype(&pltype.clone().borrow(), ctx)
                } else {
                    self.get_ditype(&PLType::PRIMITIVE(PriType::I64), ctx)
                }
            }
            PLType::PLACEHOLDER(_) => self.get_ditype(&PLType::PRIMITIVE(PriType::I64), ctx),
            PLType::ARR(arr) => {
                let elemdi = self.get_ditype(&arr.element_type.borrow(), ctx)?;
                let etp = &self
                    .get_basic_type_op(&arr.element_type.borrow(), ctx)
                    .unwrap();
                let size = td.get_bit_size(etp) * arr.size as u64;
                let align = td.get_preferred_alignment(etp);
                Some(
                    self.dibuilder
                        .create_array_type(elemdi, size, align, &[(0..arr.size as i64)])
                        .as_type(),
                )
            }
            PLType::STRUCT(x) | PLType::TRAIT(x) => {
                // 若已经生成过，直接查表返回
                if RefCell::borrow(&self.ditypes).contains_key(&x.get_st_full_name()) {
                    return Some(
                        RefCell::borrow(&self.ditypes)
                            .get(&x.get_st_full_name())
                            .unwrap()
                            .clone(),
                    );
                }
                let mut offset = 0;
                // 生成占位符，为循环引用做准备
                self.ditypes_placeholder
                    .borrow_mut()
                    .insert(x.get_st_full_name(), RefCell::new(vec![]));
                let m = x
                    .ordered_fields
                    .iter()
                    .map(|v| {
                        let (tp, off) = self.get_field_di_type(v, ctx, offset);
                        offset = off;
                        tp
                    })
                    .collect::<Vec<_>>();
                let sttp = self.struct_type(x, ctx);
                let st = self
                    .dibuilder
                    .create_struct_type(
                        self.diunit.get_file().as_debug_info_scope(),
                        &x.name,
                        self.diunit.get_file(),
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
                let members = self
                    .ditypes_placeholder
                    .borrow_mut()
                    .remove(&x.get_st_full_name())
                    .unwrap();
                // 替换循环引用生成的占位符
                for m in members.borrow().iter() {
                    let mut elemdi = st;
                    for _ in 0..m.ptr_depth {
                        elemdi = self
                            .dibuilder
                            .create_pointer_type(
                                "",
                                elemdi,
                                td.get_bit_size(
                                    &sttp.ptr_type(AddressSpace::default()).as_basic_type_enum(),
                                ),
                                td.get_preferred_alignment(
                                    &sttp.ptr_type(AddressSpace::default()).as_basic_type_enum(),
                                ),
                                AddressSpace::default(),
                            )
                            .as_type();
                    }

                    let realtp = self.dibuilder.create_member_type(
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
                        self.dibuilder
                            .replace_placeholder_derived_type(m.ditype, realtp);
                    }
                }
                self.ditypes.borrow_mut().insert(x.get_st_full_name(), st);
                return Some(st);
            }
            PLType::PRIMITIVE(pt) => {
                let mut size = td.get_bit_size(&self.get_pri_basic_type(pt));
                if size < 8 {
                    size = 8; // walkaround for lldb <Unable to determine byte size.> issue
                }
                return Some(
                    self.dibuilder
                        .create_basic_type(&pt.get_name(), size, get_dw_ate_encoding(pt), 0)
                        .unwrap()
                        .as_type(),
                );
            }
            PLType::VOID => None,
            PLType::POINTER(p) => {
                let elemdi = self.get_ditype(&p.borrow(), ctx)?;
                let etp = &self
                    .get_basic_type_op(&p.borrow(), ctx)
                    .unwrap()
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum();
                let size = td.get_bit_size(etp);
                let align = td.get_preferred_alignment(etp);
                Some(
                    self.dibuilder
                        .create_pointer_type("", elemdi, size, align, AddressSpace::default())
                        .as_type(),
                )
            }
        }
    }

    /// try get function value from module
    ///
    /// if not found, create a declaration
    fn get_or_insert_fn(&self, pltp: &FNType, ctx: &mut Ctx<'a>) -> FunctionValue<'ctx> {
        let llvmname = pltp.append_name_with_generic(pltp.llvmname.clone());
        if let Some(v) = self.module.get_function(&llvmname) {
            return v;
        }
        let fn_type = self.get_fn_type(pltp, ctx);
        let fn_value = self
            .module
            .add_function(&llvmname, fn_type, Some(Linkage::External));
        fn_value
    }
    fn struct_type(&self, pltp: &STType, ctx: &mut Ctx<'a>) -> StructType<'ctx> {
        let st = self.module.get_struct_type(&pltp.get_st_full_name());
        if let Some(st) = st {
            return st;
        }
        let st = self.context.opaque_struct_type(&pltp.get_st_full_name());
        st.set_body(
            &pltp
                .ordered_fields
                .clone()
                .into_iter()
                .map(|order_field| {
                    self.get_basic_type_op(
                        &order_field
                            .typenode
                            .get_type(ctx, &self.clone().into())
                            .unwrap()
                            .borrow(),
                        ctx,
                    )
                    .unwrap()
                })
                .collect::<Vec<_>>(),
            false,
        );
        st
    }

    fn get_gc(&self, ctx: &mut Ctx<'a>) -> PointerValue<'ctx> {
        let gcmod = ctx.plmod.submods.get("gc").unwrap();
        let gc = gcmod.get_global_symbol("diogc").unwrap();
        self.get_or_add_global_value(&gcmod.get_full_name("diogc"), gc.tp.clone(), ctx)
    }
    /// 用来获取外部模块的全局变量
    /// 如果没在当前module的全局变量表中找到，将会生成一个
    /// 该全局变量的声明
    fn get_or_add_global_value(
        &self,
        name: &str,
        pltype: Arc<RefCell<PLType>>,
        ctx: &mut Ctx<'a>,
    ) -> PointerValue<'ctx> {
        let global = self.get_global_var_handle(name);
        if global.is_none() {
            let global = self.module.add_global(
                self.get_basic_type_op(&pltype.borrow(), ctx).unwrap(),
                None,
                name,
            );
            global.set_linkage(Linkage::External);
            return global.as_pointer_value();
        }
        self.get_llvm_value(global.unwrap())
            .unwrap()
            .into_pointer_value()
    }
}
impl<'a, 'ctx> IRBuilder<'a, 'ctx> for LLVMBuilder<'a, 'ctx> {
    fn bitcast(
        &self,
        ctx: &mut Ctx<'a>,
        from: ValueHandle,
        to: &PLType,
        name: &str,
    ) -> ValueHandle {
        let lv = self.get_llvm_value(from).unwrap();
        let re = if lv.is_function_value() {
            self.builder.build_bitcast(
                lv.into_function_value()
                    .as_global_value()
                    .as_pointer_value(),
                self.get_basic_type_op(to, ctx).unwrap(),
                name,
            )
        } else {
            self.builder.build_bitcast(
                lv.into_pointer_value(),
                self.get_basic_type_op(to, ctx).unwrap(),
                name,
            )
        };
        self.get_llvm_value_handle(&re.as_any_value_enum())
    }
    fn pointer_cast(
        &self,
        ctx: &mut Ctx<'a>,
        from: ValueHandle,
        to: &PLType,
        name: &str,
    ) -> ValueHandle {
        let lv = self.get_llvm_value(from).unwrap();

        let re = self.builder.build_pointer_cast(
            lv.into_pointer_value(),
            self.get_basic_type_op(to, ctx).unwrap().into_pointer_type(),
            name,
        );
        self.get_llvm_value_handle(&re.as_any_value_enum())
    }
    fn get_global_var_handle(&self, name: &str) -> Option<ValueHandle> {
        match self.module.get_global(name) {
            Some(value) => Some(self.get_llvm_value_handle(&value.as_any_value_enum())),
            None => None,
        }
    }
    fn new_subscope(&self, start: Pos) {
        let scope = self.discope.get();
        self.discope.set(
            self.dibuilder
                .create_lexical_block(
                    scope,
                    self.diunit.get_file(),
                    start.line as u32,
                    start.column as u32,
                )
                .as_debug_info_scope(),
        );
    }
    fn position_at_end_block(&self, block: BlockHandle) {
        self.builder
            .position_at_end(self.block_table.borrow()[&block]);
    }
    fn get_or_insert_fn_handle(&self, pltp: &FNType, ctx: &mut Ctx<'a>) -> ValueHandle {
        self.get_llvm_value_handle(&self.get_or_insert_fn(pltp, ctx).as_any_value_enum())
    }

    fn mv2heap(&self, val: ValueHandle, ctx: &mut Ctx<'a>) -> ValueHandle {
        if !ctx.usegc {
            return val;
        }
        let val = self.get_llvm_value(val).unwrap();
        let gcmod = ctx.plmod.submods.get("gc").unwrap();
        let f: FNType = gcmod
            .get_type("DioGC__malloc")
            .unwrap()
            .borrow()
            .clone()
            .try_into()
            .unwrap();
        let f = self.get_or_insert_fn(&f, ctx);
        let gc = self.builder.build_load(self.get_gc(ctx), "gc");
        let td = self.targetmachine.get_target_data();
        let loaded = self.builder.build_load(val.into_pointer_value(), "loaded");
        let size = td.get_store_size(&loaded.get_type());
        let size = self.context.i64_type().const_int(size as u64, true);
        let heapptr = self
            .builder
            .build_call(f, &[gc.into(), size.into()], "gc_malloc")
            .try_as_basic_value()
            .left()
            .unwrap();
        let heapptr = self.builder.build_pointer_cast(
            heapptr.into_pointer_value(),
            val.get_type().into_pointer_type(),
            "heapptr",
        );
        self.builder.build_store(heapptr, loaded);
        self.get_llvm_value_handle(&heapptr.as_any_value_enum())
    }
    fn gc_add_root(&self, stackptr: BasicValueEnum<'ctx>, ctx: &mut Ctx<'a>) {
        if !ctx.usegc {
            return;
        }
        let gcmod = ctx.plmod.submods.get("gc").unwrap();
        let f: FNType = gcmod
            .get_type("DioGC__add_root")
            .unwrap()
            .borrow()
            .clone()
            .try_into()
            .unwrap();
        let f = self.get_or_insert_fn(&f, ctx);
        let gc = self.builder.build_load(self.get_gc(ctx), "gc");
        let stackptr = self.builder.build_pointer_cast(
            stackptr.into_pointer_value(),
            self.context.i64_type().ptr_type(AddressSpace::default()),
            "stackptr",
        );
        self.builder
            .build_call(f, &[gc.into(), stackptr.into()], "add_root");
    }
    fn gc_rm_root(&self, stackptr: ValueHandle, ctx: &mut Ctx<'a>) {
        if !ctx.usegc {
            return;
        }
        self.builder.unset_current_debug_location();
        let block = self.builder.get_insert_block().unwrap();
        if let Some(inst) = block.get_first_instruction() {
            self.builder.position_before(&inst);
        }
        self.gc_rm_root_current(stackptr, ctx);
    }
    fn gc_rm_root_current(&self, stackptr: ValueHandle, ctx: &mut Ctx<'a>) {
        if !ctx.usegc {
            return;
        }
        let stackptr = self.get_llvm_value(stackptr).unwrap();
        let gcmod = ctx.plmod.submods.get("gc").unwrap();
        let f: FNType = gcmod
            .get_type("DioGC__rm_root")
            .unwrap()
            .borrow()
            .clone()
            .try_into()
            .unwrap();
        let f = self.get_or_insert_fn(&f, ctx);
        let gc = self.builder.build_load(self.get_gc(ctx), "gc");
        let stackptr = self.builder.build_pointer_cast(
            stackptr.into_pointer_value(),
            self.context.i64_type().ptr_type(AddressSpace::default()),
            "stackptr",
        );
        self.builder
            .build_call(f, &[gc.into(), stackptr.into()], "rm_root");
    }
    fn gc_collect(&self, ctx: &mut Ctx<'a>) {
        if !ctx.usegc {
            return;
        }
        self.builder.unset_current_debug_location();
        let gcmod = ctx.plmod.submods.get("gc").unwrap();
        let f: FNType = gcmod
            .get_type("DioGC__collect")
            .unwrap()
            .borrow()
            .clone()
            .try_into()
            .unwrap();
        let f = self.get_or_insert_fn(&f, ctx);
        let gc = self.builder.build_load(self.get_gc(ctx), "gc");
        self.builder.build_call(f, &[gc.into()], "collect");
    }
    fn get_or_add_global(
        &self,
        name: &str,
        pltype: Arc<RefCell<PLType>>,
        ctx: &mut Ctx<'a>,
    ) -> ValueHandle {
        self.get_llvm_value_handle(
            &self
                .get_or_add_global_value(name, pltype, ctx)
                .as_any_value_enum(),
        )
    }

    fn build_load(&self, ptr: ValueHandle, name: &str) -> ValueHandle {
        let ptr = self.get_llvm_value(ptr).unwrap();
        let ptr = ptr.into_pointer_value();
        let ptr = self.builder.build_load(ptr, name);
        self.get_llvm_value_handle(&ptr.as_any_value_enum())
    }
    fn try_load2var(
        &self,
        range: Range,
        v: ValueHandle,
        tp: Arc<RefCell<PLType>>,
        ctx: &mut Ctx<'a>,
    ) -> Result<(ValueHandle, Arc<RefCell<PLType>>), PLDiag> {
        let handle = v;
        let v = self.get_llvm_value(handle).unwrap();
        if !v.is_pointer_value() {
            return Ok(match v {
                AnyValueEnum::ArrayValue(_)
                | AnyValueEnum::IntValue(_)
                | AnyValueEnum::FloatValue(_)
                | AnyValueEnum::PointerValue(_)
                | AnyValueEnum::StructValue(_)
                | AnyValueEnum::VectorValue(_) => (handle, tp),
                _ => return Err(ctx.add_diag(range.new_err(ErrorCode::EXPECT_VALUE))),
            });
        } else {
            let tp = &tp;
            Ok((
                self.build_load(
                    self.get_llvm_value_handle(&v.into_pointer_value().as_any_value_enum()),
                    "loadtmp",
                ),
                tp.clone(),
            ))
        }
    }

    fn get_function(&self, name: &str) -> Option<ValueHandle> {
        let f = self.module.get_function(name);
        if f.is_none() {
            return None;
        }
        let f = f.unwrap();
        Some(self.get_llvm_value_handle(&f.as_any_value_enum()))
    }

    fn build_call(&self, f: ValueHandle, args: &[ValueHandle]) -> Option<ValueHandle> {
        let builder = self.builder;
        let f = self.get_llvm_value(f).unwrap();
        let f: CallableValue = if f.is_function_value() {
            f.into_function_value().into()
        } else {
            f.into_pointer_value().try_into().unwrap()
        };
        let args = args
            .iter()
            .map(|v| {
                let be: BasicValueEnum = self.get_llvm_value(*v).unwrap().try_into().unwrap();
                let bme: BasicMetadataValueEnum = be.into();
                bme
            })
            .collect::<Vec<_>>();
        let v = builder.build_call(f, &args, "calltmp").try_as_basic_value();
        if v.right().is_some() {
            return None;
        }
        Some(self.get_llvm_value_handle(&v.left().unwrap().as_any_value_enum()))
    }
    fn add_function(
        &self,
        name: &str,
        paramtps: &[PLType],
        ret: PLType,
        ctx: &mut Ctx<'a>,
    ) -> ValueHandle {
        let mut param_types = vec![];
        for param_pltype in paramtps.iter() {
            param_types.push(self.get_basic_type_op(param_pltype, ctx).unwrap().into());
        }
        let fn_type = self.get_ret_type(&ret, ctx).fn_type(&param_types, false);
        let fn_value = self
            .module
            .add_function(name, fn_type, Some(Linkage::External));
        self.get_llvm_value_handle(&fn_value.as_any_value_enum())
    }
    fn opaque_struct_type(&self, name: &str) {
        self.context.opaque_struct_type(name);
    }

    fn add_body_to_struct_type(&self, name: &str, order_fields: &[Field], ctx: &mut Ctx<'a>) {
        let st = self.module.get_struct_type(name).unwrap();
        st.set_body(
            &order_fields
                .into_iter()
                .map(|order_field| {
                    self.get_basic_type_op(
                        &order_field
                            .typenode
                            .get_type(ctx, &self.clone().into())
                            .unwrap()
                            .borrow(),
                        ctx,
                    )
                    .unwrap()
                })
                .collect::<Vec<_>>(),
            false,
        );
    }
    fn alloc(&self, name: &str, pltype: &PLType, ctx: &mut Ctx<'a>) -> ValueHandle {
        let builder = self.builder;
        builder.unset_current_debug_location();
        let lb = builder.get_insert_block().unwrap();
        match self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap()
            .get_first_basic_block()
        {
            Some(alloca) => {
                builder.position_at_end(alloca);
                let p = builder.build_alloca(self.get_basic_type_op(pltype, ctx).unwrap(), name);
                self.gc_add_root(p.as_basic_value_enum(), ctx);
                ctx.roots
                    .borrow_mut()
                    .push(self.get_llvm_value_handle(&p.as_any_value_enum()));
                builder.position_at_end(lb);
                self.get_llvm_value_handle(&p.as_any_value_enum())
            }
            None => panic!("alloc get entry failed!"),
        }
    }
    fn build_struct_gep(
        &self,
        structv: ValueHandle,
        index: u32,
        name: &str,
    ) -> Result<ValueHandle, ()> {
        let structv = self.get_llvm_value(structv).unwrap();
        let structv = structv.into_pointer_value();
        let gep = self.builder.build_struct_gep(structv, index, name);
        if let Ok(gep) = gep {
            return Ok(self.get_llvm_value_handle(&gep.as_any_value_enum()));
        } else {
            return Err(());
        };
    }
    fn build_store(&self, ptr: ValueHandle, value: ValueHandle) {
        let value = self.get_llvm_value(value).unwrap();
        let ptr = self.get_llvm_value(ptr).unwrap();
        let ptr = ptr.into_pointer_value();
        self.builder
            .build_store::<BasicValueEnum>(ptr, value.try_into().unwrap());
    }
    fn build_const_in_bounds_gep(
        &self,
        ptr: ValueHandle,
        index: &[u64],
        name: &str,
    ) -> ValueHandle {
        let ptr = self.get_llvm_value(ptr).unwrap();
        let ptr = ptr.into_pointer_value();
        let gep = unsafe {
            self.builder.build_in_bounds_gep(
                ptr,
                &index
                    .into_iter()
                    .map(|i| self.context.i64_type().const_int(*i, false))
                    .collect::<Vec<_>>(),
                name,
            )
        };
        self.get_llvm_value_handle(&gep.as_any_value_enum())
    }
    fn build_in_bounds_gep(
        &self,
        ptr: ValueHandle,
        index: &[ValueHandle],
        name: &str,
    ) -> ValueHandle {
        let ptr = self.get_llvm_value(ptr).unwrap();
        let ptr = ptr.into_pointer_value();
        let gep = unsafe {
            self.builder.build_in_bounds_gep(
                ptr,
                &index
                    .into_iter()
                    .map(|i| self.get_llvm_value(*i).unwrap().try_into().unwrap())
                    .collect::<Vec<_>>(),
                name,
            )
        };
        self.get_llvm_value_handle(&gep.as_any_value_enum())
    }
    fn const_string(&self, s: &str) -> ValueHandle {
        let s = self.context.const_string(s.as_bytes(), false);
        self.get_llvm_value_handle(&s.as_any_value_enum())
    }
    fn build_dbg_location(&self, pos: Pos) {
        let loc = self.dibuilder.create_debug_location(
            self.context,
            pos.line as u32,
            pos.column as u32,
            self.discope.get(),
            None,
        );
        self.builder.set_current_debug_location(self.context, loc);
    }
    fn insert_var_declare(
        &self,
        name: &str,
        pos: Pos,
        pltype: &PLType,
        v: ValueHandle,
        ctx: &mut Ctx<'a>,
    ) {
        let ditype = self.get_ditype(pltype, ctx);
        let debug_var_info = self.dibuilder.create_auto_variable(
            self.discope.get(),
            name,
            self.diunit.get_file(),
            pos.line as u32,
            ditype.unwrap(),
            true,
            DIFlags::PUBLIC,
            ditype.unwrap().get_align_in_bits(),
        );
        self.dibuilder.insert_declare_at_end(
            self.get_llvm_value(v).unwrap().into_pointer_value(),
            Some(debug_var_info),
            None,
            self.builder.get_current_debug_location().unwrap(),
            self.builder
                .get_insert_block()
                .unwrap()
                .get_parent()
                .unwrap()
                .get_first_basic_block()
                .unwrap(),
        );
    }

    fn build_phi(
        &self,
        pltype: &PLType,
        ctx: &mut Ctx<'a>,
        vbs: &[(ValueHandle, BlockHandle)],
    ) -> ValueHandle {
        let phi = self
            .builder
            .build_phi(self.get_basic_type_op(pltype, ctx).unwrap(), "");
        for (value, block) in vbs {
            let value = self.get_llvm_value(*value).unwrap().into_int_value();
            let block = self.get_llvm_block(*block).unwrap();
            phi.add_incoming(&[(&value, block)]);
        }
        self.get_llvm_value_handle(&phi.as_any_value_enum())
    }

    fn build_unconditional_branch(&self, bb: BlockHandle) {
        let bb = self.get_llvm_block(bb).unwrap();
        self.builder.build_unconditional_branch(bb);
    }

    fn get_first_instruction(&self, bb: BlockHandle) -> Option<ValueHandle> {
        let bb = self.get_llvm_block(bb).unwrap();
        let first = bb.get_first_instruction();
        if let Some(first) = first {
            return Some(self.get_llvm_value_handle(&first.as_any_value_enum()));
        } else {
            return None;
        }
    }
    fn position_at(&self, v: ValueHandle) {
        // inkwell hack
        let v = self.get_llvm_value(v).unwrap();
        let v = if v.is_instruction_value() {
            v.into_instruction_value()
        } else {
            let bs: BasicValueEnum = v.try_into().unwrap();
            bs.as_instruction_value().unwrap()
        };
        self.builder.position_at(v.get_parent().unwrap(), &v);
    }
    fn finalize_debug(&self) {
        self.dibuilder.finalize();
    }
    fn print_to_file(&self, file: &Path) -> Result<(), String> {
        if let Err(s) = self.module.print_to_file(file) {
            return Err(s.to_string());
        }
        Ok(())
    }
    fn write_bitcode_to_path(&self, path: &Path) -> bool {
        self.module.write_bitcode_to_path(path)
    }
    fn int_value(&self, ty: &PriType, v: u64, sign_ext: bool) -> ValueHandle {
        let ty = self.get_pri_basic_type(ty).into_int_type();
        let v = ty.const_int(v, sign_ext);
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn float_value(&self, ty: &PriType, v: f64) -> ValueHandle {
        let ty = self.get_pri_basic_type(ty).into_float_type();
        let v = ty.const_float(v);
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_int_z_extend(&self, v: ValueHandle, ty: &PriType, name: &str) -> ValueHandle {
        let v = self.get_llvm_value(v).unwrap().into_int_value();
        let ty = self.get_pri_basic_type(ty).into_int_type();
        let v = self.builder.build_int_z_extend(v, ty, name);
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_or(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_int_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_int_value();
        let v = self.builder.build_or(lhs, rhs, name);
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_and(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_int_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_int_value();
        let v = self.builder.build_and(lhs, rhs, name);
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_float_compare(
        &self,
        op: FloatPredicate,
        lhs: ValueHandle,
        rhs: ValueHandle,
        name: &str,
    ) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_float_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_float_value();
        let v = self.builder.build_float_compare(op, lhs, rhs, name);
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_int_compare(
        &self,
        op: IntPredicate,
        lhs: ValueHandle,
        rhs: ValueHandle,
        name: &str,
    ) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_int_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_int_value();
        let v = self.builder.build_int_compare(op, lhs, rhs, name);
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_int_neg(&self, v: ValueHandle, name: &str) -> ValueHandle {
        let v = self.get_llvm_value(v).unwrap().into_int_value();
        let v = self.builder.build_int_neg(v, name);
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_int_add(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_int_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_int_value();
        let v = self.builder.build_int_add(lhs, rhs, name);
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_int_sub(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_int_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_int_value();
        let v = self.builder.build_int_sub(lhs, rhs, name);
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_int_mul(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_int_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_int_value();
        let v = self.builder.build_int_mul(lhs, rhs, name);
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_int_signed_div(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_int_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_int_value();
        let v = self.builder.build_int_signed_div(lhs, rhs, name);
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_float_neg(&self, v: ValueHandle, name: &str) -> ValueHandle {
        let v = self.get_llvm_value(v).unwrap().into_float_value();
        let v = self.builder.build_float_neg(v, name);
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_float_add(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_float_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_float_value();
        let v = self.builder.build_float_add(lhs, rhs, name);
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_float_sub(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_float_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_float_value();
        let v = self.builder.build_float_sub(lhs, rhs, name);
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_float_mul(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_float_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_float_value();
        let v = self.builder.build_float_mul(lhs, rhs, name);
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_float_div(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_float_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_float_value();
        let v = self.builder.build_float_div(lhs, rhs, name);
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn append_basic_block(&self, func: ValueHandle, name: &str) -> BlockHandle {
        let bb = self.context.append_basic_block(
            self.get_llvm_value(func).unwrap().into_function_value(),
            name,
        );
        self.get_llvm_block_handle(bb)
    }
    fn build_int_truncate(&self, v: ValueHandle, dest_ty: &PriType, name: &str) -> ValueHandle {
        let v = self.get_llvm_value(v).unwrap().into_int_value();
        let dest_ty = self.get_pri_basic_type(dest_ty).into_int_type();
        let v = self.builder.build_int_truncate(v, dest_ty, name);
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_conditional_branch(
        &self,
        cond: ValueHandle,
        then_bb: BlockHandle,
        else_bb: BlockHandle,
    ) {
        let cond = self.get_llvm_value(cond).unwrap().into_int_value();
        let then_bb = self.get_llvm_block(then_bb).unwrap();
        let else_bb = self.get_llvm_block(else_bb).unwrap();
        self.builder
            .build_conditional_branch(cond, then_bb, else_bb);
    }
    fn rm_curr_debug_location(&self) {
        self.builder.unset_current_debug_location();
    }
    fn clear_insertion_position(&self) {
        self.builder.clear_insertion_position();
    }
    fn try_set_fn_dbg(&self, pos: Pos, f: ValueHandle) {
        let f = self.get_llvm_value(f).unwrap().into_function_value();
        if f.get_subprogram().is_some() {
            self.discope
                .set(f.get_subprogram().unwrap().as_debug_info_scope());
            // ctx.discope = currscope;
            self.build_dbg_location(pos)
        }
    }
    fn build_sub_program(
        &self,
        paralist: Vec<Box<TypedIdentifierNode>>,
        ret: Box<TypeNodeEnum>,
        fntype: &FNType,
        fnvalue: ValueHandle,
        child: &mut Ctx<'a>,
    ) -> Result<(), PLDiag> {
        let mut param_ditypes = vec![];
        for para in paralist.iter() {
            let pltype = para.typenode.get_type(child, &self.clone().into())?;
            match &*pltype.borrow() {
                PLType::VOID => {
                    return Err(child
                        .add_diag(para.range.new_err(ErrorCode::VOID_TYPE_CANNOT_BE_PARAMETER)))
                }
                pltype => {
                    param_ditypes.push(self.get_ditype(&pltype, child).unwrap());
                }
            };
        }
        // debug info
        let subroutine_type = self.dibuilder.create_subroutine_type(
            self.diunit.get_file(),
            self.get_ditype(&ret.get_type(child, &self.clone().into())?.borrow(), child),
            &param_ditypes,
            DIFlags::PUBLIC,
        );
        let subprogram = self.dibuilder.create_function(
            self.diunit.get_file().as_debug_info_scope(),
            &fntype.append_name_with_generic(fntype.name.clone()),
            None,
            self.diunit.get_file(),
            fntype.range.start.line as u32,
            subroutine_type,
            true,
            true,
            fntype.range.start.line as u32,
            DIFlags::PUBLIC,
            false,
        );
        let funcvalue = self.get_llvm_value(fnvalue).unwrap().into_function_value();
        funcvalue.set_subprogram(subprogram);
        // let discope = child.discope;
        self.discope.set(subprogram.as_debug_info_scope().clone());
        Ok(())
    }
    fn build_return(&self, v: Option<ValueHandle>) {
        if let Some(v) = v {
            let v = self.get_llvm_value(v).unwrap();
            let v: BasicValueEnum = v.try_into().unwrap();
            self.builder.build_return(Some(&v));
        } else {
            self.builder.build_return(None);
        }
    }
    fn create_parameter_variable(
        &self,
        fntype: &FNType,
        pos: Pos,
        i: usize,
        child: &mut Ctx<'a>,
        fnvalue: ValueHandle,
        alloca: ValueHandle,
        allocab: BlockHandle,
    ) {
        let divar = self.dibuilder.create_parameter_variable(
            self.discope.get(),
            &fntype.param_names[i],
            i as u32,
            self.diunit.get_file(),
            pos.line as u32,
            self.get_ditype(
                &fntype.param_pltypes[i]
                    .get_type(child, &self.clone().into())
                    .unwrap()
                    .borrow(),
                child,
            )
            .unwrap(),
            false,
            DIFlags::PUBLIC,
        );
        self.build_dbg_location(pos);
        self.dibuilder.insert_declare_at_end(
            self.get_llvm_value(alloca).unwrap().into_pointer_value(),
            Some(divar),
            None,
            self.builder.get_current_debug_location().unwrap(),
            self.get_llvm_block(allocab).unwrap(),
        );
        let funcvalue = self.get_llvm_value(fnvalue).unwrap().into_function_value();
        self.build_store(
            alloca,
            self.get_llvm_value_handle(
                &funcvalue
                    .get_nth_param(i as u32)
                    .unwrap()
                    .as_any_value_enum(),
            ),
        );
    }
    fn get_cur_basic_block(&self) -> BlockHandle {
        self.get_llvm_block_handle(self.builder.get_insert_block().unwrap())
    }
    fn get_last_basic_block(&self, v: ValueHandle) -> BlockHandle {
        let v = self.get_llvm_value(v).unwrap().into_function_value();
        self.get_llvm_block_handle(v.get_last_basic_block().unwrap())
    }
    fn get_first_basic_block(&self, v: ValueHandle) -> BlockHandle {
        let v = self.get_llvm_value(v).unwrap().into_function_value();
        self.get_llvm_block_handle(v.get_first_basic_block().unwrap())
    }
    fn delete_block(&self, b: BlockHandle) {
        let b = self.get_llvm_block(b).unwrap();
        unsafe {
            _ = b.delete();
        }
    }
    fn add_global(
        &self,
        name: &str,
        pltype: Arc<RefCell<PLType>>,
        ctx: &mut Ctx<'a>,
        line: u32,
        pltp: &PLType,
    ) -> ValueHandle {
        let base_type = self.get_basic_type_op(&pltype.borrow(), ctx).unwrap();
        let global = self.module.add_global(base_type, None, name);
        let ditype = self.get_ditype(pltp, ctx);
        let exp = self.dibuilder.create_global_variable_expression(
            self.diunit.as_debug_info_scope(),
            name,
            "",
            self.diunit.get_file(),
            line,
            ditype.unwrap(),
            false,
            None,
            None,
            ditype.unwrap().get_align_in_bits(),
        );
        global.set_initializer(&base_type.const_zero());
        global.set_metadata(exp.as_metadata_value(self.context), 0);
        self.get_llvm_value_handle(&global.as_any_value_enum())
    }
}
