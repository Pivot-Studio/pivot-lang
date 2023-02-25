/// 此包代码应该遵循以下原则：
/// 1. 所有Builder的字段都应该private，不应该被外部直接访问
/// 2. 所有涉及llvm类型的函数（包括参数或返回值）都应该是private的
use std::{
    cell::{Cell, RefCell},
    path::Path,
    sync::{
        atomic::{AtomicI64, Ordering},
        Arc,
    },
};

use immix::{IntEnum, ObjectType};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    debug_info::*,
    module::{FlagBehavior, Linkage, Module},
    targets::{InitializationConfig, Target, TargetMachine},
    types::{BasicType, BasicTypeEnum, FunctionType, PointerType, StructType},
    values::{
        AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallableValue,
        FunctionValue, PointerValue,
    },
    AddressSpace, FloatPredicate, IntPredicate, OptimizationLevel,
};
use rustc_hash::FxHashMap;

use crate::ast::{diag::PLDiag, pass::run_immix_pass};

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
static ID: AtomicI64 = AtomicI64::new(0);
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

fn get_nth_mark_fn<'ctx>(f: FunctionValue<'ctx>, n: u32) -> CallableValue<'ctx> {
    f.get_nth_param(n)
        .unwrap()
        .into_pointer_value()
        .try_into()
        .unwrap()
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
    let module = context.create_module(&format!("{}__{}", dir, file));
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
    // FIXME: win debug need these info, but currently it is broken
    if cfg!(target_os = "windows") {
        let metacv = context.metadata_node(&[BasicMetadataValueEnum::IntValue(
            context.i32_type().const_int(1, false),
        )]);
        module.add_metadata_flag("CodeView", FlagBehavior::Warning, metacv);
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
    heap_stack_map: Arc<RefCell<FxHashMap<ValueHandle, ValueHandle>>>,
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
            inkwell::targets::RelocMode::Default,
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
            heap_stack_map: Arc::new(RefCell::new(FxHashMap::default())),
        }
    }
    fn gc_malloc(
        &self,
        name: &str,
        ctx: &mut Ctx<'a>,
        tp: &PLType,
    ) -> (PointerValue<'ctx>, PointerValue<'ctx>, BasicTypeEnum<'ctx>) {
        let obj_type = tp.get_immix_type().int_value();
        let mut root_ctx = &*ctx;
        while let Some(f) = root_ctx.father {
            root_ctx = f
        }
        let gcmod = root_ctx.plmod.submods.get("gc").unwrap_or(&root_ctx.plmod);
        let f: FNType = gcmod
            .get_type("DioGC__malloc")
            .unwrap()
            .borrow()
            .clone()
            .try_into()
            .unwrap();
        let f = self.get_or_insert_fn(&f, ctx);
        let llvmtp = self.get_basic_type_op(tp, ctx).unwrap();
        let tp = self
            .context
            .i8_type()
            .const_int(tp.get_immix_type().int_value() as u64, false);
        let td = self.targetmachine.get_target_data();
        let size = td.get_store_size(&llvmtp);
        let size = self.context.i64_type().const_int(size as u64, false);
        let heapptr = self
            .builder
            .build_call(f, &[size.into(), tp.into()], &format!("heapptr_{}", name))
            .try_as_basic_value()
            .left()
            .unwrap();
        let casted_result = self.builder.build_bitcast(
            heapptr.into_pointer_value(),
            llvmtp.ptr_type(AddressSpace::default()),
            name,
        );
        let lb = self.builder.get_insert_block().unwrap();
        let alloca = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap()
            .get_first_basic_block()
            .unwrap();
        self.builder.position_at_end(alloca);
        let stack_ptr = self
            .builder
            .build_alloca(llvmtp.ptr_type(AddressSpace::default()), "stack_ptr");
        self.builder.position_at_end(lb);
        self.builder.build_store(stack_ptr, casted_result);
        self.gc_add_root(stack_ptr.as_basic_value_enum(), ctx, obj_type);
        (casted_result.into_pointer_value(), stack_ptr, llvmtp)
    }

    fn gc_add_root(&self, stackptr: BasicValueEnum<'ctx>, ctx: &mut Ctx<'a>, obj_type: u8) {
        if !ctx.usegc {
            return;
        }
        self.module
            .get_function("llvm.gcroot")
            .or_else(|| {
                let i8ptr = self.context.i8_type().ptr_type(AddressSpace::default());
                let ty = self.context.void_type().fn_type(
                    &[i8ptr.ptr_type(AddressSpace::default()).into(), i8ptr.into()],
                    false,
                );
                Some(self.module.add_function("llvm.gcroot", ty, None))
            })
            .and_then(|f| {
                let i8ptr = self.context.i8_type().ptr_type(AddressSpace::default());
                let stackptr = self.builder.build_bitcast(
                    stackptr.into_pointer_value(),
                    i8ptr.ptr_type(AddressSpace::default()),
                    "stackptr",
                );
                let tp = ObjectType::from_int(obj_type).expect("invalid object type");
                let tp_const_name = format!(
                    "@{}_IMMIX_OBJTYPE_{}",
                    self.module.get_source_file_name().to_str().unwrap(),
                    match tp {
                        ObjectType::Atomic => "ATOMIC",
                        ObjectType::Trait => "TRAIT",
                        ObjectType::Complex => "COMPLEX",
                        ObjectType::Pointer => "POINTER",
                    }
                );
                self.module
                    .get_global(&tp_const_name)
                    .or_else(|| {
                        let g =
                            self.module
                                .add_global(self.context.i8_type(), None, &tp_const_name);
                        g.set_linkage(Linkage::Internal);
                        g.set_constant(true);
                        g.set_initializer(&self.context.i8_type().const_int(obj_type as u64, true));
                        Some(g)
                    })
                    .and_then(|g| {
                        self.builder.build_call(
                            f,
                            &[
                                stackptr.into_pointer_value().into(),
                                g.as_pointer_value().into(),
                            ],
                            "add_root",
                        );
                        Some(())
                    })
            });
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

    fn visit_f_tp(&self) -> PointerType<'ctx> {
        let i8ptrtp = self.context.i8_type().ptr_type(AddressSpace::default());
        self.context
            .void_type()
            .fn_type(&[i8ptrtp.into(), i8ptrtp.into()], false)
            .ptr_type(AddressSpace::default())
    }

    fn mark_fn_tp(&self, ptrtp: PointerType<'ctx>) -> FunctionType<'ctx> {
        let i8ptrtp = self.context.i8_type().ptr_type(AddressSpace::default());
        let visit_ftp = self.visit_f_tp();
        self.context.void_type().fn_type(
            &[
                ptrtp.into(),
                i8ptrtp.into(),
                visit_ftp.into(),
                visit_ftp.into(),
                visit_ftp.into(),
            ],
            false,
        )
    }

    fn gen_or_get_arr_visit_function(&self, ctx: &mut Ctx<'a>, v: &ARRType) -> FunctionValue<'ctx> {
        let currentbb = self.builder.get_insert_block();
        self.builder.unset_current_debug_location();
        let ptrtp = self.arr_type(v, ctx).ptr_type(AddressSpace::default());
        let ty = ptrtp.get_element_type().into_struct_type();
        let ftp = self.mark_fn_tp(ptrtp);
        let arr_tp = ty.get_field_type_at_index(1).unwrap().into_array_type();
        let fname = &(arr_tp.to_string() + "@" + &ctx.plmod.path);
        if let Some(f) = self.module.get_function(fname) {
            return f;
        }
        let f = self.module.add_function(&fname, ftp, None);
        // the array is a struct, the first field is the visit function, the second field is the real array
        // array struct it self is the first parameter
        // the other three parameters are the visit function for different type
        let bb = self.context.append_basic_block(f, "entry");
        self.builder.position_at_end(bb);
        let arr = f.get_nth_param(0).unwrap().into_pointer_value();
        let arr = self.builder.build_struct_gep(arr, 1, "arr").unwrap();
        let loop_var = self.builder.build_alloca(self.context.i32_type(), "i");
        // arr is the real array
        let arr_len = self
            .context
            .i32_type()
            .const_int(arr_tp.len() as u64, false);
        // generate a loop, iterate the real array, and do nothing
        let condbb = self.context.append_basic_block(f, "cond");
        self.builder.build_unconditional_branch(condbb);
        self.builder.position_at_end(condbb);
        let i = self.builder.build_load(loop_var, "i").into_int_value();
        let cond = self
            .builder
            .build_int_compare(IntPredicate::ULT, i, arr_len, "cond");
        let loopbb = self.context.append_basic_block(f, "loop");
        let endbb = self.context.append_basic_block(f, "end");
        self.builder.build_conditional_branch(cond, loopbb, endbb);
        self.builder.position_at_end(loopbb);
        let i = self.builder.build_load(loop_var, "i").into_int_value();
        let elm = unsafe {
            self.builder.build_in_bounds_gep(
                arr,
                &[self.context.i64_type().const_int(0, false), i],
                "elm",
            )
        };
        let visitor = f.get_nth_param(1).unwrap().into_pointer_value();
        let visit_ptr_f = get_nth_mark_fn(f, 2);
        // complex type needs to provide a visit function by itself
        // which is stored in the first field of the struct
        let visit_complex_f = get_nth_mark_fn(f, 3);
        let visit_trait_f = get_nth_mark_fn(f, 4);
        match &*v.element_type.borrow() {
            PLType::ARR(_) | PLType::STRUCT(_) => {
                // call the visit_complex function
                self.builder
                    .build_call(visit_complex_f, &[visitor.into(), elm.into()], "call");
            }
            PLType::POINTER(_) => {
                // call the visit_ptr function
                self.builder
                    .build_call(visit_ptr_f, &[visitor.into(), elm.into()], "call");
            }
            PLType::TRAIT(_) => {
                // call the visit_trait function
                self.builder
                    .build_call(visit_trait_f, &[visitor.into(), elm.into()], "call");
            }
            _ => {}
        }
        let i = self.builder.build_load(loop_var, "i").into_int_value();
        let i = self
            .builder
            .build_int_add(i, self.context.i32_type().const_int(1, false), "i");
        self.builder.build_store(loop_var, i);
        self.builder.build_unconditional_branch(condbb);
        self.builder.position_at_end(endbb);
        self.builder.build_return(None);
        if let Some(currentbb) = currentbb {
            self.builder.position_at_end(currentbb);
        }
        f
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
    fn get_or_insert_st_visit_fn_handle(&self, p: &PointerValue<'ctx>) -> FunctionValue<'ctx> {
        let ptrtp = p.get_type();
        let ty = ptrtp.get_element_type().into_struct_type();
        let llvmname = ty.get_name().unwrap().to_str().unwrap().to_string() + "@";
        if let Some(v) = self.module.get_function(&llvmname) {
            return v;
        }
        let i8ptrtp = self.context.i8_type().ptr_type(AddressSpace::default());
        let visit_ftp = self
            .context
            .void_type()
            .fn_type(&[i8ptrtp.into(), i8ptrtp.into()], false)
            .ptr_type(AddressSpace::default());
        let ftp = self.context.void_type().fn_type(
            &[
                ptrtp.into(),
                i8ptrtp.into(),
                visit_ftp.into(),
                visit_ftp.into(),
                visit_ftp.into(),
            ],
            false,
        );
        let fn_type = ftp;
        let fn_value = self
            .module
            .add_function(&llvmname, fn_type, Some(Linkage::External));
        fn_value
    }

    fn get_fn_type(&self, f: &FNType, ctx: &mut Ctx<'a>) -> FunctionType<'ctx> {
        ctx.run_in_fn_mod(f, |ctx, f| {
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
            Ok(fn_type)
        })
        .unwrap()
    }
    /// # get_basic_type_op
    /// get the basic type of the type
    /// used in code generation
    fn get_basic_type_op(&self, pltp: &PLType, ctx: &mut Ctx<'a>) -> Option<BasicTypeEnum<'ctx>> {
        match pltp {
            PLType::GENERIC(g) => match &g.curpltype {
                Some(pltype) => self.get_basic_type_op(&pltype.borrow(), ctx),
                None => Some({
                    let name = &format!("__placeholder__{}", g.name);
                    self.module
                        .get_struct_type(name)
                        .or(Some({
                            let st = self
                                .context
                                .opaque_struct_type(&format!("__placeholder__{}", g.name));
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
            PLType::ARR(a) => Some(self.arr_type(a, ctx)),
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
    /// array type in fact is a struct with two fields,
    /// the first is a function pointer to the visit function(used in gc)
    /// the second is the array itself
    fn arr_type(&self, arrtp: &ARRType, ctx: &mut Ctx<'a>) -> BasicTypeEnum<'ctx> {
        self.context
            .struct_type(
                &[
                    self.context.i64_type().as_basic_type_enum(),
                    self.get_basic_type_op(&arrtp.element_type.borrow(), ctx)
                        .unwrap()
                        .array_type(arrtp.size)
                        .as_basic_type_enum(),
                ],
                false,
            )
            .as_basic_type_enum()
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
                let arr_st_tp = self.arr_type(arr, ctx).into_struct_type();
                let size = td.get_bit_size(etp) * arr.size as u64;
                let align = td.get_preferred_alignment(etp);
                let st_size = td.get_bit_size(&arr_st_tp);
                let vtabledi = self.get_ditype(&PLType::PRIMITIVE(PriType::U64), ctx)?;
                let offset = td.offset_of_element(&arr_st_tp, 0).unwrap();
                let vtabletp = self.dibuilder.create_member_type(
                    self.diunit.get_file().as_debug_info_scope(),
                    "_vtable",
                    self.diunit.get_file(),
                    0,
                    vtabledi.get_size_in_bits(),
                    vtabledi.get_align_in_bits(),
                    offset * 8,
                    DIFlags::PUBLIC,
                    vtabledi,
                );
                let arrdi = self
                    .dibuilder
                    .create_array_type(elemdi, size, align, &[(0..arr.size as i64)])
                    .as_type();
                let offset = td.offset_of_element(&arr_st_tp, 1).unwrap();
                let arrtp = self.dibuilder.create_member_type(
                    self.diunit.get_file().as_debug_info_scope(),
                    "array",
                    self.diunit.get_file(),
                    0,
                    arrdi.get_size_in_bits(),
                    arrdi.get_align_in_bits(),
                    offset * 8,
                    DIFlags::PUBLIC,
                    arrdi,
                );
                let st = self
                    .dibuilder
                    .create_struct_type(
                        self.diunit.get_file().as_debug_info_scope(),
                        "arr_wrapper",
                        self.diunit.get_file(),
                        0,
                        st_size,
                        align,
                        DIFlags::PUBLIC,
                        None,
                        &[vtabletp.as_type(), arrtp.as_type()],
                        0,
                        None,
                        "arr_wrapper",
                    )
                    .as_type();
                Some(st)
            }
            PLType::STRUCT(x) | PLType::TRAIT(x) => {
                let sttp = self.struct_type(x, ctx);
                // 若已经生成过，直接查表返回
                if RefCell::borrow(&self.ditypes).contains_key(&x.get_st_full_name()) {
                    return Some(
                        RefCell::borrow(&self.ditypes)
                            .get(&x.get_st_full_name())
                            .unwrap()
                            .clone(),
                    );
                }
                // 生成占位符，为循环引用做准备
                self.ditypes_placeholder
                    .borrow_mut()
                    .insert(x.get_st_full_name(), RefCell::new(vec![]));
                let mut m = vec![];
                ctx.run_in_st_mod(x, |ctx, x| {
                    m = x
                        .ordered_fields
                        .iter()
                        .map(|v| {
                            let offset = td.offset_of_element(&sttp, v.index as u32).unwrap() * 8;
                            let (tp, _) = self.get_field_di_type(v, ctx, offset);
                            tp
                        })
                        .collect::<Vec<_>>();
                    Ok(())
                })
                .unwrap();
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
        ctx.run_in_st_mod(pltp, |ctx, pltp| {
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
            Ok(())
        })
        .unwrap();
        st
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
    fn alloc(
        &self,
        name: &str,
        pltype: &PLType,
        ctx: &mut Ctx<'a>,
        declare: Option<Pos>,
    ) -> ValueHandle {
        let td = self.targetmachine.get_target_data();
        let builder = self.builder;
        builder.unset_current_debug_location();
        let bt = self.get_basic_type_op(pltype, ctx).unwrap();
        let (p, stack_root, _) = self.gc_malloc(name, ctx, pltype);
        // TODO: force user to manually init all structs, so we can remove this memset
        let size_val = self
            .context
            .i64_type()
            .const_int(td.get_store_size(&bt) as u64, false);
        self.builder
            .build_memset(
                p,
                td.get_abi_alignment(&bt),
                self.context.i8_type().const_zero(),
                size_val,
            )
            .unwrap();
        if let PLType::STRUCT(_) = pltype {
            let f = self.get_or_insert_st_visit_fn_handle(&p);
            let i = self.builder.build_ptr_to_int(
                f.as_global_value().as_pointer_value(),
                self.context.i64_type(),
                "_vtable",
            );
            let vtable = self.builder.build_struct_gep(p, 0, "vtable").unwrap();
            self.builder.build_store(vtable, i);
        } else if let PLType::ARR(tp) = pltype {
            let f = self.gen_or_get_arr_visit_function(ctx, tp);
            let i = self.builder.build_ptr_to_int(
                f.as_global_value().as_pointer_value(),
                self.context.i64_type(),
                "_vtable",
            );
            let vtable = self.builder.build_struct_gep(p, 0, "vtable").unwrap();
            self.builder.build_store(vtable, i);
        }
        declare.and_then(|p| {
            self.build_dbg_location(p);
            self.insert_var_declare(
                name,
                p,
                &PLType::POINTER(Arc::new(RefCell::new(pltype.clone()))),
                self.get_llvm_value_handle(&stack_root.as_any_value_enum()),
                ctx,
            );
            Some(())
        });
        let v_stack = self.get_llvm_value_handle(&stack_root.as_any_value_enum());
        let v_heap = self.get_llvm_value_handle(&p.as_any_value_enum());
        self.heap_stack_map.borrow_mut().insert(v_heap, v_stack);
        v_heap
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
        let s = self.builder.build_global_string_ptr(
            s,
            format!("str_{}", ID.fetch_add(1, Ordering::Relaxed)).as_str(),
        );
        let s = self.builder.build_bitcast(
            s,
            self.context.i8_type().ptr_type(Default::default()),
            "str",
        );
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
        self.builder.set_current_debug_location(loc);
    }
    fn insert_var_declare(
        &self,
        name: &str,
        pos: Pos,
        pltype: &PLType,
        v: ValueHandle,
        ctx: &mut Ctx<'a>,
    ) {
        // let dbg = self.builder.get_current_debug_location();
        // self.builder.unset_current_debug_location();
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
        // dbg.map(|d| self.builder.set_current_debug_location(d));
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
        run_immix_pass(&self.module);
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
            false,
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
        let stack_ptr = self.get_stack_root(alloca);
        self.dibuilder.insert_declare_at_end(
            self.get_llvm_value(stack_ptr).unwrap().into_pointer_value(),
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

    fn gen_st_visit_function(
        &self,
        ctx: &mut Ctx<'a>,
        v: &STType,
        field_tps: &[Arc<RefCell<PLType>>],
    ) {
        let currentbb = ctx.block;
        self.builder.unset_current_debug_location();
        let i8ptrtp = self.context.i8_type().ptr_type(AddressSpace::default());
        let ptrtp = self.struct_type(v, ctx).ptr_type(AddressSpace::default());
        let ty = ptrtp.get_element_type().into_struct_type();
        let ftp = self.mark_fn_tp(ptrtp);
        let f = self.module.add_function(
            &(ty.get_name().unwrap().to_str().unwrap().to_string() + "@"),
            ftp,
            None,
        );
        let bb = self.context.append_basic_block(f, "entry");
        self.builder.position_at_end(bb);
        let fieldn = ty.count_fields();
        let st = f.get_nth_param(0).unwrap().into_pointer_value();
        // iterate all fields but the first
        for i in 1..fieldn {
            let field_pltp = &*field_tps[i as usize - 1].borrow();
            let visitor = f.get_nth_param(1).unwrap().into_pointer_value();
            let visit_ptr_f = get_nth_mark_fn(f, 2);
            // complex type needs to provide a visit function by itself
            // which is stored in the first field of the struct
            let visit_complex_f = get_nth_mark_fn(f, 3);
            let visit_trait_f = get_nth_mark_fn(f, 4);
            let f = self.builder.build_struct_gep(st, i, "gep").unwrap();
            // 指针类型，递归调用visit函数
            if let PLType::POINTER(_) = field_pltp {
                let ptr = f;
                let casted = self.builder.build_bitcast(ptr, i8ptrtp, "casted_arg");
                self.builder
                    .build_call(visit_ptr_f, &[visitor.into(), casted.into()], "call");
            }
            // 数组类型，递归调用visit函数
            else if let PLType::ARR(_) = field_pltp {
                let ptr = f;
                let casted = self.builder.build_bitcast(ptr, i8ptrtp, "casted_arg");
                self.builder
                    .build_call(visit_complex_f, &[visitor.into(), casted.into()], "call");
            }
            // 结构体类型，递归调用visit函数
            else if let PLType::STRUCT(_) = field_pltp {
                let ptr = f;
                let casted = self.builder.build_bitcast(ptr, i8ptrtp, "casted_arg");
                self.builder
                    .build_call(visit_complex_f, &[visitor.into(), casted.into()], "call");
            }
            // trait类型，递归调用visit函数
            else if let PLType::TRAIT(_) = field_pltp {
                let ptr = f;
                let casted = self.builder.build_bitcast(ptr, i8ptrtp, "casted_arg");
                self.builder
                    .build_call(visit_trait_f, &[visitor.into(), casted.into()], "call");
            }
            // 其他为原子类型，跳过
        }
        self.builder.build_return(None);
        if let Some(currentbb) = currentbb {
            self.builder
                .position_at_end(self.get_llvm_block(currentbb).unwrap());
        }
    }

    fn get_stack_root(&self, v: ValueHandle) -> ValueHandle {
        *self.heap_stack_map.borrow().get(&v).unwrap()
    }
}
