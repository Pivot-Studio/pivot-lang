#![cfg(feature = "llvm")]
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
    types::{
        AnyType, AsTypeRef, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
        PointerType, StructType, VoidType,
    },
    values::{
        AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallableValue,
        FunctionValue, PointerValue,
    },
    AddressSpace, FloatPredicate, IntPredicate, OptimizationLevel,
};
use llvm_sys::{core::LLVMStructSetBody, prelude::LLVMTypeRef};
use rustc_hash::FxHashMap;

use crate::ast::{
    ctx::{CtxFlag, PLSymbol},
    diag::PLDiag,
    pltype::{get_type_deep, ClosureType, TraitImplAble},
};

use super::{
    super::{
        ctx::Ctx,
        diag::ErrorCode,
        node::{types::TypedIdentifierNode, TypeNode, TypeNodeEnum},
        pltype::{ARRType, FNValue, Field, PLType, PriType, STType},
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
// pub const DW_TAG_union_type: u32 = 0x17;
static ID: AtomicI64 = AtomicI64::new(0);
// const DW_TAG_REFERENCE_TYPE: u32 = 16;
fn get_dw_ate_encoding(pritp: &PriType) -> u32 {
    match pritp {
        PriType::I8 | PriType::I16 | PriType::I32 | PriType::I64 | PriType::I128 => DW_ATE_SIGNED,
        PriType::U8 | PriType::U16 | PriType::U32 | PriType::U64 | PriType::U128 => DW_ATE_UNSIGNED,
        PriType::F32 | PriType::F64 => DW_ATE_FLOAT,
        PriType::BOOL => DW_ATE_BOOLEAN,
    }
}

fn get_nth_mark_fn(f: FunctionValue, n: u32) -> CallableValue {
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
    ditypes_placeholder: Arc<RefCell<FxHashMap<String, RefCell<Vec<DIDerivedType<'ctx>>>>>>, // hold the generated debug info type place holder
    ditypes: Arc<RefCell<FxHashMap<String, DIType<'ctx>>>>, // hold the generated debug info type
    heap_stack_map: Arc<RefCell<FxHashMap<ValueHandle, ValueHandle>>>,
    optimized: Arc<RefCell<bool>>,
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
            inkwell::targets::RelocMode::DynamicNoPic,
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
            optimized: Arc::new(RefCell::new(false)),
        }
    }
    fn alloc_raw(
        &self,
        name: &str,
        pltype: &PLType,
        ctx: &mut Ctx<'a>,
        declare: Option<Pos>,
        malloc_fn: &str,
    ) -> ValueHandle {
        let builder = self.builder;
        builder.unset_current_debug_location();
        let (p, stack_root, _) = self.gc_malloc(name, ctx, pltype, malloc_fn);
        if let PLType::Struct(tp) = pltype {
            let f = self.get_or_insert_st_visit_fn_handle(&p, tp);
            let i = self.builder.build_ptr_to_int(
                f.as_global_value().as_pointer_value(),
                self.context.i64_type(),
                "_vtable",
            );
            let vtable = self.builder.build_struct_gep(p, 0, "vtable").unwrap();
            self.builder.build_store(vtable, i);
        } else if let PLType::Arr(tp) = pltype {
            let f = self.gen_or_get_arr_visit_function(ctx, tp);
            let i = self.builder.build_ptr_to_int(
                f.as_global_value().as_pointer_value(),
                self.context.i64_type(),
                "_vtable",
            );
            let vtable = self.builder.build_struct_gep(p, 0, "vtable").unwrap();
            self.builder.build_store(vtable, i);
        }
        if let Some(p) = declare {
            self.build_dbg_location(p);
            self.insert_var_declare(
                name,
                p,
                &PLType::Pointer(Arc::new(RefCell::new(pltype.clone()))),
                self.get_llvm_value_handle(&stack_root.as_any_value_enum()),
                ctx,
            );
        }
        let v_stack = self.get_llvm_value_handle(&stack_root.as_any_value_enum());
        let v_heap = self.get_llvm_value_handle(&p.as_any_value_enum());
        self.set_root(v_heap, v_stack);
        v_heap
    }

    fn set_root(&self, v_heap: usize, v_stack: usize) {
        self.heap_stack_map.borrow_mut().insert(v_heap, v_stack);
    }
    fn gc_malloc(
        &self,
        name: &str,
        ctx: &mut Ctx<'a>,
        tp: &PLType,
        malloc_fn: &str,
    ) -> (PointerValue<'ctx>, PointerValue<'ctx>, BasicTypeEnum<'ctx>) {
        let lb = self.builder.get_insert_block().unwrap();
        let alloca = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap()
            .get_first_basic_block()
            .unwrap();
        let obj_type = tp.get_immix_type().int_value();
        let f = self.get_malloc_f(ctx, malloc_fn);
        let llvmtp = self.get_basic_type_op(tp, ctx).unwrap();
        let immix_tp = self
            .context
            .i8_type()
            .const_int(tp.get_immix_type().int_value() as u64, false);
        let td = self.targetmachine.get_target_data();
        let size = td.get_store_size(&llvmtp);
        let mut size = self.context.i64_type().const_int(size, false);
        if name == "___ctx" {
            // generator ctx, use stack variable as type
            self.builder.position_at_end(alloca);
            let stack_ptr = self
                .builder
                .build_alloca(self.context.i64_type(), "ctx_tp_ptr");
            ctx.generator_data
                .as_ref()
                .unwrap()
                .borrow_mut()
                .ctx_size_handle = self.get_llvm_value_handle(&stack_ptr.as_any_value_enum());

            self.builder.position_at_end(lb);

            size = self
                .builder
                .build_load(stack_ptr, "ctx_tp")
                .into_int_value();
        }
        let heapptr = self
            .builder
            .build_call(
                f,
                &[size.into(), immix_tp.into()],
                &format!("heapptr_{}", name),
            )
            .try_as_basic_value()
            .left()
            .unwrap();

        let casted_result = self.builder.build_bitcast(
            heapptr.into_pointer_value(),
            llvmtp.ptr_type(AddressSpace::default()),
            name,
        );

        // TODO: force user to manually init all structs, so we can remove this memset
        let size_val = self
            .context
            .i64_type()
            .const_int(td.get_store_size(&llvmtp), false);
        self.builder
            .build_memset(
                casted_result.into_pointer_value(),
                td.get_abi_alignment(&llvmtp),
                self.context.i8_type().const_zero(),
                size_val,
            )
            .unwrap();

        if let PLType::Arr(arr) = tp {
            if arr.size_handle != 0 {
                let f = self.get_malloc_f(ctx, "DioGC__malloc_no_collect");
                let etp = self
                    .get_basic_type_op(&arr.element_type.borrow(), ctx)
                    .unwrap();
                let size = td.get_store_size(&etp);
                let size = self.context.i64_type().const_int(size, false);
                let arr_len = self
                    .get_llvm_value(arr.size_handle)
                    .unwrap()
                    .into_int_value();
                let arr_size = self.builder.build_int_mul(arr_len, size, "arr_size");
                let arr_size = self.builder.build_int_z_extend_or_bit_cast(
                    arr_size,
                    self.context.i64_type(),
                    "arr_size",
                );
                let len_ptr = self
                    .builder
                    .build_struct_gep(casted_result.into_pointer_value(), 2, "arr_len")
                    .unwrap();
                self.builder.build_store(len_ptr, arr_len);
                let arr_ptr = self
                    .builder
                    .build_struct_gep(casted_result.into_pointer_value(), 1, "arr_ptr")
                    .unwrap();
                let arr_space = self
                    .builder
                    .build_call(
                        f,
                        &[
                            arr_size.into(),
                            self.context
                                .i8_type()
                                .const_int(immix::ObjectType::Atomic.int_value() as u64, false)
                                .into(),
                        ],
                        "arr_space",
                    )
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                let arr_space = self.builder.build_bitcast(
                    arr_space.into_pointer_value(),
                    etp.ptr_type(AddressSpace::default()),
                    "arr_space",
                );
                self.builder.build_store(arr_ptr, arr_space);
            }
        }

        self.builder.position_at_end(alloca);
        if alloca.get_terminator().is_some() {
            panic!("alloca block should not have terminator yet")
        }
        let stack_ptr = self
            .builder
            .build_alloca(llvmtp.ptr_type(AddressSpace::default()), "stack_ptr");
        self.gc_add_root(stack_ptr.as_basic_value_enum(), obj_type);
        self.builder.position_at_end(lb);
        self.builder.build_store(stack_ptr, casted_result);
        (casted_result.into_pointer_value(), stack_ptr, llvmtp)
    }

    fn get_malloc_f(&self, ctx: &mut Ctx<'a>, malloc_fn: &str) -> FunctionValue<'ctx> {
        let mut root_ctx = &*ctx;
        while let Some(f) = root_ctx.root {
            root_ctx = f
        }
        let gcmod = root_ctx.plmod.submods.get("gc").unwrap_or(&root_ctx.plmod);
        let f: FNValue = gcmod
            .types
            .get(malloc_fn)
            .unwrap()
            .borrow()
            .clone()
            .try_into()
            .unwrap();
        let f = self.get_or_insert_fn(&f, ctx);
        f.0
    }

    /// # create_root_for
    ///
    /// 为一个堆上的对象创建gcroot
    ///
    /// ## safety
    ///
    /// 1. 如果传入的值不是堆上的对象，可能导致segment fault在内的一系列问题。
    /// 2. 如果在应该调用这个函数的时候没有调用，可能导致pl程序在gc进行evacuation之后，无法正确的访问堆上的对象。导致
    ///   segment fault、bus error在内的一系列问题。关于驱逐算法的详细信息，见[gc文档](https://lang.pivotstudio.cn/docs/systemlib/immix.html#evacuation)
    /// 3. 如果禁用了evacuation，那么这个函数将不会有实际作用
    fn create_root_for(&self, heap_ptr: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
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
        let stack_ptr = self.builder.build_alloca(heap_ptr.get_type(), "stack_ptr");
        self.gc_add_root(
            stack_ptr.as_basic_value_enum(),
            ObjectType::Pointer.int_value(),
        );
        self.builder.position_at_end(lb);
        self.builder.build_store(stack_ptr, heap_ptr);
        self.heap_stack_map.borrow_mut().insert(
            self.get_llvm_value_handle(&heap_ptr.as_any_value_enum()),
            self.get_llvm_value_handle(&stack_ptr.as_any_value_enum()),
        );
        stack_ptr.as_basic_value_enum()
    }

    /// 第一个参数必须是一个二重以上的指针，且不能是一重指针bitcast过来的二重指针
    /// 否则可能导致bus error
    fn gc_add_root(&self, stackptr: BasicValueEnum<'ctx>, obj_type: u8) {
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
                    .map(|g| {
                        self.builder.build_call(
                            f,
                            &[
                                stackptr.into_pointer_value().into(),
                                g.as_pointer_value().into(),
                            ],
                            "add_root",
                        );
                    })
            });
    }

    fn get_llvm_value_handle(&self, value: &AnyValueEnum<'ctx>) -> ValueHandle {
        let len = self.handle_table.borrow().len();
        let nh = match self.handle_reverse_table.borrow().get(value) {
            Some(handle) => *handle,
            None => len + 1,
        };
        if nh == len + 1 {
            self.handle_table.borrow_mut().insert(nh, *value);
            self.handle_reverse_table.borrow_mut().insert(*value, nh);
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
        let i8ptrtp = self.context.i8_type().ptr_type(AddressSpace::default());
        let currentbb = self.builder.get_insert_block();
        self.builder.unset_current_debug_location();
        let ptrtp = self.arr_type(v, ctx).ptr_type(AddressSpace::default());
        let ty = ptrtp.get_element_type().into_struct_type();
        let ftp = self.mark_fn_tp(ptrtp);
        let arr_tp = ty.get_field_type_at_index(1).unwrap();
        let fname = &(arr_tp.to_string() + "@" + &ctx.plmod.path);
        if let Some(f) = self.module.get_function(fname) {
            return f;
        }
        let f = self
            .module
            .add_function(fname, ftp, Some(Linkage::External));
        // the array is a struct, the first field is the visit function,
        // the second field is the real array, the third field is it's length
        // array struct it self is the first parameter
        // the other three parameters are the visit function for different type
        let bb = self.context.append_basic_block(f, "entry");
        self.builder.position_at_end(bb);
        let arr = f.get_nth_param(0).unwrap().into_pointer_value();
        let real_arr_raw = self.builder.build_struct_gep(arr, 1, "arr").unwrap();
        let real_arr = self
            .builder
            .build_load(real_arr_raw, "loaded_arr")
            .into_pointer_value();
        let loop_var = self.builder.build_alloca(self.context.i64_type(), "i");
        // arr is the real array
        let arr_len = self.builder.build_struct_gep(arr, 2, "arr_len").unwrap();
        let arr_len = self.builder.build_load(arr_len, "arr_len").into_int_value();
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
        let elm = unsafe { self.builder.build_in_bounds_gep(real_arr, &[i], "elm") };
        let visitor = f.get_nth_param(1).unwrap().into_pointer_value();
        let visit_ptr_f = get_nth_mark_fn(f, 2);
        // complex type needs to provide a visit function by itself
        // which is stored in the first field of the struct
        let visit_complex_f = get_nth_mark_fn(f, 3);
        let visit_trait_f = get_nth_mark_fn(f, 4);
        match &*get_type_deep(v.element_type.clone()).borrow() {
            PLType::Arr(_) | PLType::Struct(_) => {
                let casted = self.builder.build_bitcast(elm, i8ptrtp, "casted_arg");
                // call the visit_complex function
                self.builder
                    .build_call(visit_complex_f, &[visitor.into(), casted.into()], "call");
            }
            PLType::Pointer(_) => {
                // call the visit_ptr function
                self.builder
                    .build_call(visit_ptr_f, &[visitor.into(), elm.into()], "call");
            }
            PLType::Trait(_) | PLType::Union(_) | PLType::Closure(_) => {
                // call the visit_trait function
                self.builder
                    .build_call(visit_trait_f, &[visitor.into(), elm.into()], "call");
            }
            PLType::Fn(_)
            | PLType::Primitive(_)
            | PLType::Void
            | PLType::Generic(_)
            | PLType::PlaceHolder(_) => (),
        }
        let i = self.builder.build_load(loop_var, "i").into_int_value();
        let i = self
            .builder
            .build_int_add(i, self.context.i64_type().const_int(1, false), "i");
        self.builder.build_store(loop_var, i);
        self.builder.build_unconditional_branch(condbb);
        self.builder.position_at_end(endbb);

        // call the visit_ptr function
        let casted = self
            .builder
            .build_bitcast(real_arr_raw, i8ptrtp, "casted_arg");
        self.builder.build_call(
            get_nth_mark_fn(f, 2),
            &[visitor.into(), casted.into()],
            "call",
        );
        self.builder.build_return(None);
        if let Some(currentbb) = currentbb {
            self.builder.position_at_end(currentbb);
        }
        f
    }

    fn get_llvm_value(&self, handle: ValueHandle) -> Option<AnyValueEnum<'ctx>> {
        if let Some(root) = self.heap_stack_map.borrow().get(&handle) {
            self.handle_table.borrow().get(root).map(|v| {
                let handle = self.handle_table.borrow().get(&handle).copied().unwrap();
                if v.into_pointer_value().get_type().get_element_type() != handle.get_type() {
                    let bt: BasicTypeEnum = handle.get_type().try_into().unwrap();
                    let v = self.builder.build_bitcast(
                        v.into_pointer_value(),
                        bt.ptr_type(AddressSpace::default()),
                        "get_root_cast",
                    );
                    self.builder
                        .build_load(v.into_pointer_value(), "load_stack")
                        .as_any_value_enum()
                } else {
                    self.builder
                        .build_load(v.into_pointer_value(), "load_stack")
                        .as_any_value_enum()
                }
            })
        } else {
            self.handle_table.borrow().get(&handle).copied()
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
        self.block_table.borrow().get(&handle).copied()
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
    fn get_or_insert_st_visit_fn_handle(
        &self,
        p: &PointerValue<'ctx>,
        st: &STType,
    ) -> FunctionValue<'ctx> {
        let ptrtp = p.get_type();
        let llvmname = st.get_full_name() + "@";
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
        self.module
            .add_function(&llvmname, fn_type, Some(Linkage::External))
    }

    fn get_fn_type(&self, fnvalue: &FNValue, ctx: &mut Ctx<'a>) -> FunctionType<'ctx> {
        ctx.run_in_type_mod(fnvalue, |ctx, fnvalue| {
            let mut param_types = vec![];
            for param_pltype in fnvalue.fntype.param_pltypes.iter() {
                param_types.push(
                    self.get_basic_type_op(
                        &param_pltype
                            .get_type(ctx, &self.clone().into(), true)
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
                    &fnvalue
                        .fntype
                        .ret_pltype
                        .get_type(ctx, &self.clone().into(), true)
                        .unwrap()
                        .borrow(),
                    ctx,
                )
                .fn_type(&param_types, false);
            fn_type
        })
    }

    fn get_closure_fn_type(&self, closure: &ClosureType, ctx: &mut Ctx<'a>) -> FunctionType<'ctx> {
        let ptr: BasicMetadataTypeEnum = self
            .context
            .i8_type()
            .ptr_type(AddressSpace::default())
            .as_basic_type_enum()
            .into();
        let params = vec![ptr]
            .iter()
            .copied()
            .chain(closure.arg_types.iter().map(|pltype| {
                let tp = self.get_basic_type_op(&pltype.borrow(), ctx).unwrap();
                let tp: BasicMetadataTypeEnum = tp.into();
                tp
            }))
            .collect::<Vec<_>>();
        let fn_type = self
            .get_ret_type(&closure.ret_type.borrow(), ctx)
            .fn_type(&params, false);
        fn_type
    }
    /// # get_basic_type_op
    /// get the basic type of the type
    /// used in code generation
    fn get_basic_type_op(&self, pltp: &PLType, ctx: &mut Ctx<'a>) -> Option<BasicTypeEnum<'ctx>> {
        match pltp {
            PLType::Generic(g) => match &g.curpltype {
                Some(pltype) => self.get_basic_type_op(&pltype.borrow(), ctx),
                None => Some({
                    let name = &format!("__placeholder__{}", g.name);
                    self.module
                        .get_struct_type(name)
                        .unwrap_or({
                            let st = self
                                .context
                                .opaque_struct_type(&format!("__placeholder__{}", g.name));
                            st.set_body(&[], false);
                            st
                        })
                        .into()
                }),
            },
            PLType::Fn(f) => Some(
                self.get_fn_type(f, ctx)
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum(),
            ),
            PLType::Struct(s) => Some(self.struct_type(s, ctx).as_basic_type_enum()),
            PLType::Trait(s) => Some(self.struct_type(s, ctx).as_basic_type_enum()),
            PLType::Arr(a) => Some(self.arr_type(a, ctx)),
            PLType::Primitive(t) => Some(self.get_pri_basic_type(t)),
            PLType::Void => None,
            PLType::Pointer(p) => Some(
                self.get_basic_type_op(&p.borrow(), ctx)
                    .unwrap()
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum(),
            ),
            PLType::PlaceHolder(p) => Some({
                let name = &format!("__placeholder__{}", p.name);
                self.module
                    .get_struct_type(name)
                    .unwrap_or({
                        let st = self
                            .context
                            .opaque_struct_type(&format!("__placeholder__{}", p.name));
                        st.set_body(&[], false);
                        st
                    })
                    .into()
            }),
            PLType::Union(_) => {
                // all unions are represented as a struct with a tag(i64) and an i8ptr
                let fields = vec![
                    self.context.i64_type().into(),
                    self.context
                        .i8_type()
                        .ptr_type(AddressSpace::default())
                        .into(),
                ];
                Some(self.context.struct_type(&fields, false).into())
            }
            PLType::Closure(c) => {
                // all closures are represented as a struct with a function pointer and an i8ptr(point to closure data)
                let fields = vec![
                    self.get_closure_fn_type(c, ctx)
                        .ptr_type(AddressSpace::default())
                        .into(),
                    self.context
                        .i8_type()
                        .ptr_type(AddressSpace::default())
                        .into(),
                ];
                Some(self.context.struct_type(&fields, false).into())
            }
        }
    }
    /// # get_ret_type
    /// get the return type, which is void type or primitive type
    fn get_ret_type(&self, pltp: &PLType, ctx: &mut Ctx<'a>) -> RetTypeEnum<'ctx> {
        match pltp {
            PLType::Void => RetTypeEnum::Void(self.context.void_type()),
            _ => RetTypeEnum::Basic(self.get_basic_type_op(pltp, ctx).unwrap()),
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
                        .ptr_type(Default::default())
                        .as_basic_type_enum(),
                    self.context.i64_type().as_basic_type_enum(),
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
        let field_pltype = match field.typenode.get_type(ctx, &self.clone().into(), true) {
            Ok(field_pltype) => field_pltype,
            Err(_) => ctx.get_type("i64", Default::default()).unwrap().tp,
        };
        let di_type = self.get_ditype(&field_pltype.borrow(), ctx);
        let debug_type = di_type.unwrap();
        let td = self.targetmachine.get_target_data();
        let (size, align) = if matches!(*RefCell::borrow(&field_pltype), PLType::Pointer(_)) {
            let ptr = self.context.i8_type().ptr_type(AddressSpace::default());
            (td.get_bit_size(&ptr), td.get_abi_alignment(&ptr))
        } else {
            (
                debug_type.get_size_in_bits(),
                debug_type.get_align_in_bits(),
            )
        };
        (
            self.dibuilder
                .create_member_type(
                    self.diunit.get_file().as_debug_info_scope(),
                    &field.name,
                    self.diunit.get_file(),
                    field.range.start.line as u32,
                    size,
                    align,
                    offset + debug_type.get_offset_in_bits(),
                    DIFlags::PUBLIC,
                    debug_type,
                )
                .as_type(),
            offset + size,
        )
    }

    /// # get_ditype
    /// get the debug info type of the pltype
    fn get_ditype(&self, pltp: &PLType, ctx: &mut Ctx<'a>) -> Option<DIType<'ctx>> {
        let td = self.targetmachine.get_target_data();
        match pltp {
            PLType::Fn(_) => self.get_ditype(&PLType::Primitive(PriType::I64), ctx),
            PLType::Generic(g) => {
                if g.curpltype.is_some() {
                    let pltype = g.curpltype.as_ref().unwrap();
                    self.get_ditype(&pltype.clone().borrow(), ctx)
                } else {
                    self.get_ditype(&PLType::Primitive(PriType::I64), ctx)
                }
            }
            PLType::PlaceHolder(_) => self.get_ditype(&PLType::Primitive(PriType::I64), ctx),
            PLType::Arr(arr) => {
                let elemdi = self.get_ditype(&arr.element_type.borrow(), ctx)?;
                let etp = &self
                    .get_basic_type_op(&arr.element_type.borrow(), ctx)
                    .unwrap();
                let arr_st_tp = self.arr_type(arr, ctx).into_struct_type();
                let size = 0;
                let align = td.get_preferred_alignment(etp);
                let st_size = td.get_bit_size(&arr_st_tp);
                let vtabledi = self.get_ditype(&PLType::Primitive(PriType::U64), ctx)?;
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
                    .create_array_type(elemdi, size, align, &[(0..0)])
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
            PLType::Struct(x) | PLType::Trait(x) => {
                let sttp = self.struct_type(x, ctx);
                // 若已经生成过，直接查表返回
                if RefCell::borrow(&self.ditypes).contains_key(&x.get_full_name()) {
                    return Some(
                        *RefCell::borrow(&self.ditypes)
                            .get(&x.get_full_name())
                            .unwrap(),
                    );
                }
                // 生成占位符，为循环引用做准备
                self.ditypes_placeholder
                    .borrow_mut()
                    .insert(x.get_full_name(), RefCell::new(vec![]));
                let mut m = vec![];
                ctx.run_in_type_mod(x, |ctx, x| {
                    m = x
                        .get_all_field()
                        .iter()
                        .map(|v| {
                            let offset = td.offset_of_element(&sttp, v.index).unwrap() * 8;
                            let (tp, _) = self.get_field_di_type(v, ctx, offset);
                            tp
                        })
                        .collect::<Vec<_>>();
                });
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
                    .remove(&x.get_full_name())
                    .unwrap();
                // 替换循环引用生成的占位符
                for m in members.borrow().iter() {
                    let realtp = self.dibuilder.create_pointer_type(
                        "",
                        st,
                        st.get_size_in_bits(),
                        st.get_align_in_bits(),
                        AddressSpace::default(),
                    );
                    unsafe { self.dibuilder.replace_placeholder_derived_type(*m, realtp) };
                }
                self.ditypes.borrow_mut().insert(x.get_full_name(), st);
                Some(st)
            }
            PLType::Primitive(pt) => {
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
            PLType::Void => None,
            PLType::Pointer(p) => {
                if let Some(di) = self.ditypes.borrow().get(&p.borrow().get_llvm_name()) {
                    return Some(*di);
                }
                if let Some(x) = self
                    .ditypes_placeholder
                    .borrow_mut()
                    .get(&p.borrow().get_full_elm_name())
                {
                    // 循环引用
                    let placeholder =
                        unsafe { self.dibuilder.create_placeholder_derived_type(self.context) };
                    x.borrow_mut().push(placeholder);
                    return Some(placeholder.as_type());
                }
                let elemdi = self.get_ditype(&p.borrow(), ctx)?;
                let etp = &self
                    .get_basic_type_op(&p.borrow(), ctx)
                    .unwrap()
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum();
                let size = td.get_bit_size(etp);
                let align = td.get_preferred_alignment(etp);
                let di = self
                    .dibuilder
                    .create_pointer_type("", elemdi, size, align, AddressSpace::default())
                    .as_type();
                self.ditypes
                    .borrow_mut()
                    .insert(p.borrow().get_llvm_name(), di);
                Some(di)
            }
            PLType::Union(u) => {
                let utp = self.get_basic_type_op(pltp, ctx).unwrap();
                // 若已经生成过，直接查表返回
                if RefCell::borrow(&self.ditypes).contains_key(&u.get_full_name()) {
                    return Some(
                        *RefCell::borrow(&self.ditypes)
                            .get(&u.get_full_name())
                            .unwrap(),
                    );
                }
                // 生成占位符，为循环引用做准备
                self.ditypes_placeholder
                    .borrow_mut()
                    .insert(u.get_full_name(), RefCell::new(vec![]));
                let ditps = u
                    .sum_types
                    .iter()
                    .map(|v| {
                        let tp =
                            PLType::Pointer(v.get_type(ctx, &self.clone().into(), true).unwrap());
                        let base_di = self.get_ditype(&tp, ctx).unwrap();
                        self.dibuilder
                            .create_member_type(
                                self.diunit.get_file().as_debug_info_scope(),
                                &tp.get_name(),
                                self.diunit.get_file(),
                                u.range.start.line as u32 + 1,
                                td.get_bit_size(&self.context.i64_type()),
                                td.get_abi_alignment(&self.context.i64_type()),
                                0,
                                DIFlags::PUBLIC,
                                base_di,
                            )
                            .as_type()
                    })
                    .collect::<Vec<_>>();
                let ptr = self.context.i8_type().ptr_type(AddressSpace::default());
                let tp = self.dibuilder.create_union_type(
                    self.diunit.get_file().as_debug_info_scope(),
                    "data",
                    self.diunit.get_file(),
                    u.range.start.line as u32 + 1,
                    td.get_bit_size(&ptr),
                    td.get_abi_alignment(&ptr),
                    DIFlags::PUBLIC,
                    &ditps,
                    0,
                    &(u.name.clone() + "_data"),
                );

                let tag_di = self
                    .dibuilder
                    .create_basic_type(
                        &(u.name.clone() + "_tag"),
                        td.get_bit_size(&self.context.i64_type()),
                        get_dw_ate_encoding(&PriType::I64),
                        0,
                    )
                    .unwrap()
                    .as_type();
                let tag = self.dibuilder.create_member_type(
                    self.diunit.get_file().as_debug_info_scope(),
                    "tag",
                    self.diunit.get_file(),
                    u.range.start.line as u32 + 1,
                    td.get_bit_size(&self.context.i64_type()),
                    td.get_abi_alignment(&self.context.i64_type()),
                    0,
                    DIFlags::PUBLIC,
                    tag_di,
                );
                let data = self.dibuilder.create_member_type(
                    self.diunit.get_file().as_debug_info_scope(),
                    "data",
                    self.diunit.get_file(),
                    u.range.start.line as u32 + 1,
                    td.get_bit_size(&ptr),
                    td.get_abi_alignment(&ptr),
                    td.get_bit_size(&ptr),
                    DIFlags::PUBLIC,
                    tp.as_type(),
                );
                let st = self.dibuilder.create_struct_type(
                    self.diunit.get_file().as_debug_info_scope(),
                    &format!("union::{}", u.name),
                    self.diunit.get_file(),
                    u.range.start.line as u32 + 1,
                    td.get_bit_size(&utp),
                    td.get_abi_alignment(&utp),
                    DIFlags::PUBLIC,
                    None,
                    &[tag.as_type(), data.as_type()],
                    0,
                    None,
                    &format!("union::{}", u.name),
                );
                // 填充占位符
                for placeholder in RefCell::borrow_mut(&self.ditypes_placeholder)
                    .remove(&u.get_full_name())
                    .unwrap()
                    .borrow()
                    .iter()
                {
                    let size = td.get_bit_size(&utp);
                    let align = td.get_preferred_alignment(&utp);
                    let realtp = self.dibuilder.create_pointer_type(
                        "",
                        st.as_type(),
                        size,
                        align,
                        AddressSpace::default(),
                    );
                    unsafe {
                        self.dibuilder
                            .replace_placeholder_derived_type(*placeholder, realtp)
                    };
                }
                self.ditypes
                    .borrow_mut()
                    .insert(u.get_full_name(), st.as_type());
                Some(st.as_type())
            }
            PLType::Closure(_) => self.get_ditype(&PLType::Primitive(PriType::I64), ctx), // TODO
        }
    }

    /// try get function value from module
    ///
    /// if not found, create a declaration
    ///
    /// bool: 是否已经实现过该函数
    fn get_or_insert_fn(&self, pltp: &FNValue, ctx: &mut Ctx<'a>) -> (FunctionValue<'ctx>, bool) {
        let llvmname = pltp.append_name_with_generic(pltp.llvmname.clone());
        if let Some(v) = self.module.get_function(&llvmname) {
            return (v, v.count_basic_blocks() != 0);
        }
        let fn_type = self.get_fn_type(pltp, ctx);
        (
            self.module
                .add_function(&llvmname, fn_type, Some(Linkage::External)),
            false,
        )
    }
    fn get_fields(&self, pltp: &STType, ctx: &mut Ctx<'a>) -> Vec<BasicTypeEnum> {
        ctx.run_in_type_mod(pltp, |ctx, pltp| {
            pltp.get_all_field()
                .iter()
                .map(|order_field| {
                    self.get_basic_type_op(
                        &order_field
                            .typenode
                            .get_type(ctx, &self.clone().into(), true)
                            .unwrap()
                            .borrow(),
                        ctx,
                    )
                    .unwrap()
                })
                .collect::<Vec<_>>()
        })
    }

    fn struct_type(&self, pltp: &STType, ctx: &mut Ctx<'a>) -> StructType<'ctx> {
        let st = self.module.get_struct_type(&pltp.get_full_name());
        if let Some(st) = st {
            return st;
        }

        if pltp.is_tuple {
            let fields = &self.get_fields(pltp, ctx);
            return self.context.struct_type(fields, false);
        }
        let st = self.context.opaque_struct_type(&pltp.get_full_name());
        st.set_body(&self.get_fields(pltp, ctx), false);
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
    fn optimize(&self) {
        if *self.optimized.borrow() {
            return;
        }
        if crate::ast::jit_config::IS_JIT.load(std::sync::atomic::Ordering::Relaxed) {
            // jit is using shadow stack, skip immix pass
            self.module.get_functions().for_each(|f| {
                f.set_gc("shadow-stack");
            });
        } else {
            extern "C" {
                fn add_module_pass(ptr: *mut u8);
            }
            let ptr = unsafe { llvm_sys::core::LLVMCreatePassManager() };
            let mpm: inkwell::passes::PassManager<Module> =
                unsafe { inkwell::passes::PassManager::new(ptr) };

            unsafe {
                add_module_pass(ptr as _);
            };
            mpm.run_on(self.module);
        }
        *self.optimized.borrow_mut() = true;
    }

    fn try_load2var_inner(&self, v: usize) -> Result<usize, ()> {
        let handle = v;
        let v = self.get_llvm_value(handle).unwrap();
        if !v.is_pointer_value() {
            Ok(match v {
                AnyValueEnum::ArrayValue(_)
                | AnyValueEnum::IntValue(_)
                | AnyValueEnum::FloatValue(_)
                | AnyValueEnum::PointerValue(_)
                | AnyValueEnum::StructValue(_)
                | AnyValueEnum::VectorValue(_) => handle,
                AnyValueEnum::FunctionValue(f) => {
                    return Ok(self.get_llvm_value_handle(&f.as_global_value().as_any_value_enum()));
                }
                _ => return Err(()),
            })
        } else {
            Ok(self.build_load(
                self.get_llvm_value_handle(&v.into_pointer_value().as_any_value_enum()),
                "loadtmp",
            ))
        }
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
        let new_handle = self.get_llvm_value_handle(&re.as_any_value_enum());
        let root = self.heap_stack_map.borrow().get(&from).copied();
        if let Some(v) = root {
            self.heap_stack_map.borrow_mut().insert(new_handle, v);
        }
        new_handle
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
        self.module
            .get_global(name)
            .map(|value| self.get_llvm_value_handle(&value.as_any_value_enum()))
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
    /// 返回值的bool：函数是否已有函数体
    fn get_or_insert_fn_handle(&self, pltp: &FNValue, ctx: &mut Ctx<'a>) -> (ValueHandle, bool) {
        let (f, b) = self.get_or_insert_fn(pltp, ctx);
        (self.get_llvm_value_handle(&f.as_any_value_enum()), b)
    }

    fn get_or_insert_helper_fn_handle(&self, name: &str) -> ValueHandle {
        if let Some(f) = self.module.get_function(name) {
            self.get_llvm_value_handle(&f.as_any_value_enum())
        } else {
            let ftp = self.context.void_type().fn_type(&[], false);
            let f = self.module.add_function(name, ftp, None);
            self.get_llvm_value_handle(&f.as_any_value_enum())
        }
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
        if ptr.is_pointer_value() {
            self.create_root_for(ptr);
        }
        self.get_llvm_value_handle(&ptr.as_any_value_enum())
    }
    fn try_load2var(
        &self,
        range: Range,
        v: ValueHandle,
        ctx: &mut Ctx<'a>,
    ) -> Result<ValueHandle, PLDiag> {
        match self.try_load2var_inner(v) {
            Ok(value) => Ok(value),
            Err(_) => Err(range.new_err(ErrorCode::EXPECT_VALUE).add_to_ctx(ctx)),
        }
    }

    fn get_function(&self, name: &str) -> Option<ValueHandle> {
        let f = self.module.get_function(name);
        f?;
        let f = f.unwrap();
        Some(self.get_llvm_value_handle(&f.as_any_value_enum()))
    }

    fn build_call(
        &self,
        f: ValueHandle,
        args: &[ValueHandle],
        ret_type: &PLType,
        ctx: &mut Ctx<'a>,
    ) -> Option<ValueHandle> {
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
        let ret = v.left().unwrap();

        let alloca = self.alloc_raw(
            "ret_alloca",
            ret_type,
            ctx,
            None,
            "DioGC__malloc_no_collect",
        );

        self.builder.build_store(
            self.get_llvm_value(alloca).unwrap().into_pointer_value(),
            ret,
        );
        Some(alloca)
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

    fn add_body_to_struct_type(&self, name: &str, sttype: &STType, ctx: &mut Ctx<'a>) {
        let st = self.module.get_struct_type(name).unwrap();
        st.set_body(
            &sttype
                .get_all_field()
                .iter()
                .map(|order_field| {
                    self.get_basic_type_op(
                        &order_field
                            .typenode
                            .get_type(ctx, &self.clone().into(), true)
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
    fn sizeof(&self, pltype: &PLType, ctx: &mut Ctx<'a>) -> u64 {
        self.targetmachine
            .get_target_data()
            .get_store_size(&self.get_basic_type_op(pltype, ctx).unwrap())
    }
    fn alloc(
        &self,
        name: &str,
        pltype: &PLType,
        ctx: &mut Ctx<'a>,
        declare: Option<Pos>,
    ) -> ValueHandle {
        let mut ret_handle = self.alloc_raw(name, pltype, ctx, declare, "DioGC__malloc");
        if ctx.ctx_flag == CtxFlag::InGeneratorYield {
            let data = ctx.generator_data.as_ref().unwrap().clone();

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

            let f_v = ctx.function.unwrap();
            let f = self.get_llvm_value(f_v).unwrap().into_function_value();
            let yield_ctx = f.get_nth_param(0).unwrap();
            let bt = self.get_basic_type_op(pltype, ctx).unwrap();
            let count = add_field(
                yield_ctx.as_any_value_enum(),
                bt.ptr_type(Default::default()).into(),
            );
            let i = count - 1;
            let data_ptr = self
                .build_struct_gep(self.get_nth_param(f_v, 0), i, name)
                .unwrap();

            let load = self.build_load(data_ptr, "data_load");
            let stack_root = self.get_stack_root(ret_handle);
            self.build_store(stack_root, load);

            self.builder.position_at_end(lb);

            let load_again = self.build_load(load, "data_load");
            data.borrow_mut().param_tmp = load_again;
            // self.build_store(ret_handle, load_again);
            self.build_store(data_ptr, ret_handle);
            self.set_root(load, stack_root);
            ret_handle = load;

            let id = data.borrow().table.len().to_string();
            data.borrow_mut().table.insert(
                name.to_string() + &id,
                PLSymbol {
                    value: load,
                    pltype: Arc::new(RefCell::new(pltype.clone())),
                    range: Default::default(),
                    refs: None,
                },
            );
            // let data_ptr = self.build_struct_gep(ctx_v, (i +2) as u32, "para").unwrap();
            // return data_ptr;
        }

        ret_handle
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
            if gep.get_type().get_element_type().is_pointer_type() {
                let loadgep = self.builder.build_load(gep, "field_heap_ptr");
                self.create_root_for(loadgep);
                return Ok(self.get_llvm_value_handle(&gep.as_any_value_enum()));
            } else {
                return Ok(self.get_llvm_value_handle(&gep.as_any_value_enum()));
            }
        } else {
            Err(())
        }
    }
    fn build_store(&self, ptr: ValueHandle, value: ValueHandle) {
        let ptr = self.get_llvm_value(ptr).unwrap();
        let ptr = ptr.into_pointer_value();
        let value = self.get_llvm_value(value).unwrap();
        let value = if value.is_function_value() {
            value
                .into_function_value()
                .as_global_value()
                .as_basic_value_enum()
        } else {
            value.try_into().unwrap()
        };
        self.builder.build_store(ptr, value);
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
                    .iter()
                    .map(|i| self.context.i64_type().const_int(*i, false))
                    .collect::<Vec<_>>(),
                name,
            )
        };
        if gep.get_type().get_element_type().is_pointer_type() {
            let loadgep = self.builder.build_load(gep, "field_heap_ptr");
            self.create_root_for(loadgep);
            return self.get_llvm_value_handle(&gep.as_any_value_enum());
        } else {
            return self.get_llvm_value_handle(&gep.as_any_value_enum());
        }
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
                    .iter()
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
        // this line is necessary at least in llvm 14.
        // otherwise, the debug info may be changed by `create_auto_variable` in some cases.
        // for example: a structure with a field of type tuple.
        self.build_dbg_location(pos);
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
            None
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
        self.optimize();
        if let Err(s) = self.module.print_to_file(file) {
            return Err(s.to_string());
        }
        Ok(())
    }
    fn write_bitcode_to_path(&self, path: &Path) -> bool {
        self.optimize();
        // run_immix_pass(self.module);
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
        op: super::FloatPredicate,
        lhs: ValueHandle,
        rhs: ValueHandle,
        name: &str,
    ) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_float_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_float_value();

        let v = self.builder.build_float_compare(op.into(), lhs, rhs, name);
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_int_compare(
        &self,
        op: super::IntPredicate,
        lhs: ValueHandle,
        rhs: ValueHandle,
        name: &str,
    ) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_int_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_int_value();
        let v = self.builder.build_int_compare(op.into(), lhs, rhs, name);
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
        }
        self.build_dbg_location(pos)
    }
    fn build_sub_program(
        &self,
        paralist: Vec<Box<TypedIdentifierNode>>,
        ret: Box<TypeNodeEnum>,
        fntype: &FNValue,
        fnvalue: ValueHandle,
        child: &mut Ctx<'a>,
    ) -> Result<(), PLDiag> {
        let mut param_ditypes = vec![];
        for para in paralist.iter() {
            let pltype = para.typenode.get_type(child, &self.clone().into(), true)?;
            match &*pltype.borrow() {
                PLType::Void => {
                    return Err(child
                        .add_diag(para.range.new_err(ErrorCode::VOID_TYPE_CANNOT_BE_PARAMETER)))
                }
                pltype => {
                    param_ditypes.push(self.get_ditype(pltype, child).unwrap());
                }
            };
        }
        // debug info
        let subroutine_type = self.dibuilder.create_subroutine_type(
            self.diunit.get_file(),
            self.get_ditype(
                &ret.get_type(child, &self.clone().into(), true)?.borrow(),
                child,
            ),
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
        self.discope.set(subprogram.as_debug_info_scope());
        Ok(())
    }

    fn build_sub_program_by_pltp(
        &self,
        paralist: &[Arc<RefCell<PLType>>],
        ret: Arc<RefCell<PLType>>,
        name: &str,
        start_line: u32,
        fnvalue: ValueHandle,
        child: &mut Ctx<'a>,
    ) {
        let mut param_ditypes = vec![];
        for pltype in paralist.iter() {
            param_ditypes.push(self.get_ditype(&pltype.borrow(), child).unwrap());
        }
        // debug info
        let subroutine_type = self.dibuilder.create_subroutine_type(
            self.diunit.get_file(),
            self.get_ditype(&ret.borrow(), child),
            &param_ditypes,
            DIFlags::PUBLIC,
        );
        let subprogram = self.dibuilder.create_function(
            self.diunit.get_file().as_debug_info_scope(),
            &format!("{}__fn", name),
            None,
            self.diunit.get_file(),
            start_line,
            subroutine_type,
            false,
            true,
            start_line,
            DIFlags::PUBLIC,
            false,
        );
        let funcvalue = self.get_llvm_value(fnvalue).unwrap().into_function_value();
        funcvalue.set_subprogram(subprogram);
        // let discope = child.discope;
        self.discope.set(subprogram.as_debug_info_scope());
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
    #[allow(clippy::too_many_arguments)]
    fn create_parameter_variable_dbg(
        &self,
        pltp: &PLType,
        pos: Pos,
        i: usize,
        child: &mut Ctx<'a>,
        value_handle: ValueHandle,
        allocab: BlockHandle,
        name: &str,
    ) {
        let divar = self.dibuilder.create_parameter_variable(
            self.discope.get(),
            name,
            i as u32,
            self.diunit.get_file(),
            pos.line as u32,
            self.get_ditype(pltp, child).unwrap(),
            false,
            DIFlags::PUBLIC,
        );
        self.build_dbg_location(pos);
        let allocab = self.get_llvm_block(allocab).unwrap();
        let v: BasicValueEnum = self
            .get_llvm_value(value_handle)
            .unwrap()
            .try_into()
            .unwrap();
        let raw_tp = v.get_type();
        // self.builder.position_at_end(allocab);
        let alloca = self.builder.build_alloca(raw_tp, "para");
        self.builder.build_store(alloca, v);
        self.dibuilder.insert_declare_at_end(
            alloca,
            Some(divar),
            None,
            self.builder.get_current_debug_location().unwrap(),
            allocab,
        );
    }
    #[allow(clippy::too_many_arguments)]
    fn create_parameter_variable(
        &self,
        fnvalue: &FNValue,
        pos: Pos,
        i: usize,
        child: &mut Ctx<'a>,
        value_handle: ValueHandle,
        alloca: ValueHandle,
        allocab: BlockHandle,
        tp: &PLType,
    ) {
        let divar = self.dibuilder.create_parameter_variable(
            self.discope.get(),
            &fnvalue.param_names[i],
            i as u32,
            self.diunit.get_file(),
            pos.line as u32,
            self.get_ditype(
                &PLType::Pointer(
                    fnvalue.fntype.param_pltypes[i]
                        .get_type(child, &self.clone().into(), true)
                        .unwrap(),
                ),
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
        if child.ctx_flag == CtxFlag::InGeneratorYield {
            let data = child.generator_data.as_ref().unwrap().clone();
            let bb_v = data.borrow().entry_bb;
            let bb = self.get_llvm_block(bb_v).unwrap();
            let funcvalue = bb.get_parent().unwrap();
            let origin_bb = child.block.unwrap();
            self.position_at_end_block(bb_v);
            let ctx_v = data.borrow().ctx_handle;
            let para_ptr = self
                .build_struct_gep(ctx_v, (i + 2) as u32, "para")
                .unwrap();
            child.ctx_flag = CtxFlag::Normal;
            let ptr = self.alloc("param_ptr", tp, child, None);
            child.ctx_flag = CtxFlag::InGeneratorYield;
            self.build_store(
                ptr,
                self.get_llvm_value_handle(
                    &funcvalue
                        .get_nth_param(i as u32)
                        .unwrap()
                        .as_any_value_enum(),
                ),
            );
            self.build_store(para_ptr, ptr);
            self.position_at_end_block(origin_bb);

            self.build_store(alloca, data.borrow().param_tmp);
            return;
        }
        let funcvalue = self
            .get_llvm_value(value_handle)
            .unwrap()
            .into_function_value();
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
        let name = v.get_full_name() + "@";
        let f = match self.module.get_function(&name) {
            Some(f) => f,
            None => self
                .module
                .add_function(&name, ftp, Some(Linkage::External)),
        };
        f.get_basic_blocks().iter().for_each(|bb| {
            unsafe { bb.delete().unwrap() };
        });
        let bb = self.context.append_basic_block(f, "entry");
        self.builder.position_at_end(bb);
        let fieldn = ty.count_fields();
        let st = f.get_nth_param(0).unwrap().into_pointer_value();
        // iterate all fields but the first
        for i in 1..fieldn {
            let bind = get_type_deep(field_tps[i as usize - 1].clone());
            let field_pltp = &*bind.borrow();
            let visitor = f.get_nth_param(1).unwrap().into_pointer_value();
            let visit_ptr_f = get_nth_mark_fn(f, 2);
            // complex type needs to provide a visit function by itself
            // which is stored in the first field of the struct
            let visit_complex_f = get_nth_mark_fn(f, 3);
            let visit_trait_f = get_nth_mark_fn(f, 4);
            let f = self.builder.build_struct_gep(st, i, "gep").unwrap();
            // 指针类型，递归调用visit函数
            match field_pltp {
                PLType::Pointer(_) => {
                    let ptr = f;
                    let casted = self.builder.build_bitcast(ptr, i8ptrtp, "casted_arg");
                    self.builder
                        .build_call(visit_ptr_f, &[visitor.into(), casted.into()], "call");
                }
                PLType::Struct(_) | PLType::Arr(_) => {
                    let ptr = f;
                    let casted = self.builder.build_bitcast(ptr, i8ptrtp, "casted_arg");
                    self.builder.build_call(
                        visit_complex_f,
                        &[visitor.into(), casted.into()],
                        "call",
                    );
                }
                PLType::Trait(_) | PLType::Union(_) | PLType::Closure(_) => {
                    let ptr = f;
                    let casted = self.builder.build_bitcast(ptr, i8ptrtp, "casted_arg");
                    self.builder.build_call(
                        visit_trait_f,
                        &[visitor.into(), casted.into()],
                        "call",
                    );
                }
                PLType::Fn(_)
                | PLType::Primitive(_)
                | PLType::Void
                | PLType::Generic(_)
                | PLType::PlaceHolder(_) => (),
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

    fn cast_primitives(&self, handle: ValueHandle, tp: &PriType, target: &PriType) -> ValueHandle {
        let val = self.get_llvm_value(handle).unwrap();
        let signed = tp.signed();
        let tp = self.get_pri_basic_type(tp);
        let target = self.get_pri_basic_type(target);
        if tp.is_int_type() && target.is_int_type() {
            let val = val.into_int_value();
            let target = target.into_int_type();
            let val = self
                .builder
                .build_int_cast_sign_flag(val, target, signed, "cast");
            self.get_llvm_value_handle(&val.into())
        } else if tp.is_float_type() && target.is_float_type() {
            let val = val.into_float_value();
            let target = target.into_float_type();
            let val = self.builder.build_float_cast(val, target, "cast");
            self.get_llvm_value_handle(&val.into())
        } else if tp.is_int_type() && target.is_float_type() {
            let val = val.into_int_value();
            let target = target.into_float_type();
            if signed {
                let val = self.builder.build_signed_int_to_float(val, target, "cast");
                self.get_llvm_value_handle(&val.into())
            } else {
                let val = self
                    .builder
                    .build_unsigned_int_to_float(val, target, "cast");
                self.get_llvm_value_handle(&val.into())
            }
        } else if tp.is_float_type() && target.is_int_type() {
            let val = val.into_float_value();
            let target = target.into_int_type();
            if signed {
                let val = self.builder.build_float_to_signed_int(val, target, "cast");
                self.get_llvm_value_handle(&val.into())
            } else {
                let val = self
                    .builder
                    .build_float_to_unsigned_int(val, target, "cast");
                self.get_llvm_value_handle(&val.into())
            }
        } else {
            unreachable!()
        }
    }
    fn is_ptr(&self, v: ValueHandle) -> bool {
        let val = self.get_llvm_value(v).unwrap();
        val.get_type().is_pointer_type()
    }
    fn i8ptr_null(&self) -> ValueHandle {
        self.get_llvm_value_handle(
            &self
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .const_null()
                .into(),
        )
    }
    fn create_closure_parameter_variable(&self, i: u32, f: ValueHandle, alloca: ValueHandle) {
        let funcvalue = self.get_llvm_value(f).unwrap().into_function_value();
        self.build_store(
            alloca,
            self.get_llvm_value_handle(&funcvalue.get_nth_param(i).unwrap().as_any_value_enum()),
        );
    }
    fn create_closure_fn(
        &self,
        ctx: &mut Ctx<'a>,
        closure_name: &str,
        params: &[Arc<RefCell<PLType>>],
        ret: &PLType,
    ) -> ValueHandle {
        let i8ptr = self.context.i8_type().ptr_type(AddressSpace::default());
        let mut closure_param_tps: Vec<BasicMetadataTypeEnum> =
            vec![i8ptr.as_basic_type_enum().into()];
        closure_param_tps.extend(
            &params
                .iter()
                .map(|p| self.get_basic_type_op(&p.borrow(), ctx).unwrap().into())
                .collect::<Vec<_>>(),
        );
        let f_tp = self
            .get_basic_type_op(ret, ctx)
            .map(|ret| ret.fn_type(&closure_param_tps, false))
            .unwrap_or_else(|| self.context.void_type().fn_type(&closure_param_tps, false));
        let f_v = self
            .module
            .add_function(&format!("{}__fn", closure_name), f_tp, None);

        self.get_llvm_value_handle(&f_v.into())
    }
    /// # get_closure_trampoline
    ///
    /// 为指定函数创建一个用于构建闭包的跳板函数
    ///
    /// 闭包函数相比原函数多出一个参数（第一个），用于存放闭包的环境。在函数为纯函数的情况，
    /// 该值不会被使用，因此可以直接传入null。
    fn get_closure_trampoline(&self, f: ValueHandle) -> ValueHandle {
        let ori_f = self.get_llvm_value(f).unwrap().into_function_value();
        let name = ori_f.get_name();
        let trampoline_name = format!("{}__trampoline", name.to_str().unwrap());
        let closure_f = self.module.get_function(&trampoline_name);
        if let Some(closure_f) = closure_f {
            return self.get_llvm_value_handle(&closure_f.into());
        }
        let f_tp = ori_f.get_type();
        let i8ptr = self.context.i8_type().ptr_type(AddressSpace::default());
        let param_tps = f_tp.get_param_types();
        let mut closure_param_tps = vec![i8ptr.as_basic_type_enum()];
        closure_param_tps.extend(param_tps);
        let closure_param_tps = closure_param_tps
            .iter()
            .map(|v| v.to_owned().into())
            .collect::<Vec<_>>();
        let closure_ftp = if let Some(ret_tp) = f_tp.get_return_type() {
            ret_tp.fn_type(&closure_param_tps, false)
        } else {
            self.context.void_type().fn_type(&closure_param_tps, false)
        };
        let f = self
            .module
            .add_function(&trampoline_name, closure_ftp, None);
        let bb = self.context.append_basic_block(f, "entry");
        let old_bb = self.builder.get_insert_block();
        self.builder.position_at_end(bb);
        let args = f.get_params();
        let re = self.builder.build_call(
            ori_f,
            &args
                .iter()
                .skip(1)
                .map(|a| a.to_owned().into())
                .collect::<Vec<_>>(),
            "re",
        );
        if let Some(ret) = re.try_as_basic_value().left() {
            self.builder.build_return(Some(&ret));
        } else {
            self.builder.build_return(None);
        }
        if let Some(old_bb) = old_bb {
            self.builder.position_at_end(old_bb);
        }
        self.get_llvm_value_handle(&f.into())
    }

    fn get_nth_param(&self, f: ValueHandle, i: u32) -> ValueHandle {
        let funcvalue = self.get_llvm_value(f).unwrap().into_function_value();
        self.get_llvm_value_handle(&funcvalue.get_nth_param(i).unwrap().into())
    }
    fn add_closure_st_field(&self, st: ValueHandle, field: ValueHandle) {
        let st_v = self.handle_table.borrow().get(&st).copied().unwrap();
        let field_tp = self
            .handle_table
            .borrow()
            .get(&field)
            .copied()
            .unwrap()
            .get_type();
        add_field(st_v, field_tp);
    }

    fn add_generator_yield_fn(
        &self,
        ctx: &mut Ctx<'a>,
        ctx_name: &str,
        ret_tp: &PLType,
    ) -> ValueHandle {
        let tp = self
            .context
            .get_struct_type(ctx_name)
            .unwrap()
            .ptr_type(Default::default())
            .into();
        let ftp = self
            .get_basic_type_op(ret_tp, ctx)
            .unwrap()
            .fn_type(&[tp], false);
        let f = self
            .module
            .add_function(&format!("{}__yield", ctx_name), ftp, None);
        self.get_llvm_value_handle(&f.into())
    }

    fn get_block_address(&self, block: BlockHandle) -> ValueHandle {
        self.get_llvm_value_handle(unsafe {
            &self
                .get_llvm_block(block)
                .unwrap()
                .get_address()
                .unwrap()
                .into()
        })
    }
    fn build_indirect_br(&self, block: ValueHandle, ctx: &Ctx<'a>) {
        let block = self.get_llvm_value(block).unwrap();
        let bv = self.get_llvm_block(ctx.block.unwrap()).unwrap();
        self.builder.build_indirect_branch::<BasicValueEnum>(
            block.try_into().unwrap(),
            &bv.get_parent()
                .unwrap()
                .get_basic_blocks()
                .iter()
                .skip(1)
                .copied()
                .collect::<Vec<_>>(),
        );
    }
    unsafe fn store_with_aoto_cast(&self, ptr: ValueHandle, value: ValueHandle) {
        let v_ptr = self.get_llvm_value(ptr).unwrap();
        let v = self.get_llvm_value(value).unwrap();
        let v = if v.is_function_value() {
            v.into_function_value()
                .as_global_value()
                .as_basic_value_enum()
        } else {
            v.try_into().unwrap()
        };
        let ptr_tp = v_ptr.get_type().into_pointer_type();
        let value_tp = v.get_type();
        if ptr_tp.get_element_type() != value_tp.as_any_type_enum() {
            let casted = self.builder.build_bitcast::<_, BasicValueEnum>(
                v_ptr.try_into().unwrap(),
                value_tp.ptr_type(Default::default()),
                "cast",
            );
            self.builder.build_store(casted.into_pointer_value(), v);
        } else {
            self.build_store(ptr, value);
        }
    }
    fn stack_alloc(&self, name: &str, ctx: &mut Ctx<'a>, tp: &PLType) -> ValueHandle {
        let lb = self.builder.get_insert_block().unwrap();
        let llvmtp = self.get_basic_type_op(tp, ctx).unwrap();
        let alloca = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap()
            .get_first_basic_block()
            .unwrap();
        self.builder.position_at_end(alloca);
        let stack_ptr = self.builder.build_alloca(llvmtp, name);
        self.builder.position_at_end(lb);
        self.get_llvm_value_handle(&stack_ptr.as_any_value_enum())
    }
    fn correct_generator_ctx_malloc_inst(&self, ctx: &mut Ctx<'a>, name: &str) {
        let data = ctx.generator_data.as_ref().unwrap();
        let bb = data.borrow().entry_bb;
        let bb = self.get_llvm_block(bb).unwrap();
        let first_inst = bb.get_first_instruction().unwrap();
        let st = self.module.get_struct_type(name).unwrap();
        let size = self.targetmachine.get_target_data().get_store_size(&st);
        let cur_bb = self.builder.get_insert_block().unwrap();

        self.builder
            .position_at(first_inst.get_parent().unwrap(), &first_inst);
        let size = self.context.i64_type().const_int(size, false);
        let v = self
            .get_llvm_value(data.borrow().ctx_size_handle)
            .unwrap()
            .into_pointer_value();
        self.builder.build_store(v, size);

        self.builder.position_at_end(cur_bb);
    }
    fn build_memcpy(&self, from: ValueHandle, to: ValueHandle, len: ValueHandle) {
        let from = self
            .get_llvm_value(self.try_load2var_inner(from).unwrap())
            .unwrap()
            .into_pointer_value();
        let to = self
            .get_llvm_value(self.try_load2var_inner(to).unwrap())
            .unwrap()
            .into_pointer_value();
        let td = self.targetmachine.get_target_data();
        let unit_size = td.get_store_size(&from.get_type().get_element_type());
        let i64_size = self.context.i64_type().const_int(unit_size, true);
        let len = self
            .get_llvm_value(self.try_load2var_inner(len).unwrap())
            .unwrap()
            .into_int_value();
        let arg_len = self.builder.build_int_mul(len, i64_size, "arg_len");
        self.builder.build_memcpy(to, 8, from, 8, arg_len).unwrap();
    }
}

fn add_field(st_v: AnyValueEnum<'_>, field_tp: inkwell::types::AnyTypeEnum<'_>) -> u32 {
    let st_tp = st_v
        .get_type()
        .into_pointer_type()
        .get_element_type()
        .into_struct_type();
    let mut closure_data_tps = st_tp.get_field_types();
    closure_data_tps.push(field_tp.try_into().unwrap());
    set_body(&st_tp, &closure_data_tps, false);
    st_tp.count_fields()
}

fn set_body<'ctx>(s: &StructType<'ctx>, field_types: &[BasicTypeEnum<'ctx>], packed: bool) {
    let mut field_types: Vec<LLVMTypeRef> =
        field_types.iter().map(|val| val.as_type_ref()).collect();
    unsafe {
        LLVMStructSetBody(
            s.as_type_ref(),
            field_types.as_mut_ptr(),
            field_types.len() as u32,
            packed as i32,
        );
    }
}

impl From<super::FloatPredicate> for FloatPredicate {
    fn from(val: super::FloatPredicate) -> Self {
        match val {
            super::FloatPredicate::OEQ => FloatPredicate::OEQ,
            super::FloatPredicate::OGT => FloatPredicate::OGT,
            super::FloatPredicate::OGE => FloatPredicate::OGE,
            super::FloatPredicate::OLT => FloatPredicate::OLT,
            super::FloatPredicate::OLE => FloatPredicate::OLE,
            super::FloatPredicate::ONE => FloatPredicate::ONE,
            super::FloatPredicate::ORD => FloatPredicate::ORD,
            super::FloatPredicate::UNO => FloatPredicate::UNO,
            super::FloatPredicate::UEQ => FloatPredicate::UEQ,
            super::FloatPredicate::UGT => FloatPredicate::UGT,
            super::FloatPredicate::UGE => FloatPredicate::UGE,
            super::FloatPredicate::ULT => FloatPredicate::ULT,
            super::FloatPredicate::ULE => FloatPredicate::ULE,
            super::FloatPredicate::UNE => FloatPredicate::UNE,
            super::FloatPredicate::PredicateFalse => FloatPredicate::PredicateFalse,
            super::FloatPredicate::PredicateTrue => FloatPredicate::PredicateTrue,
        }
    }
}

impl From<super::IntPredicate> for IntPredicate {
    fn from(val: super::IntPredicate) -> Self {
        match val {
            super::IntPredicate::EQ => IntPredicate::EQ,
            super::IntPredicate::NE => IntPredicate::NE,
            super::IntPredicate::UGT => IntPredicate::UGT,
            super::IntPredicate::UGE => IntPredicate::UGE,
            super::IntPredicate::ULT => IntPredicate::ULT,
            super::IntPredicate::ULE => IntPredicate::ULE,
            super::IntPredicate::SGT => IntPredicate::SGT,
            super::IntPredicate::SGE => IntPredicate::SGE,
            super::IntPredicate::SLT => IntPredicate::SLT,
            super::IntPredicate::SLE => IntPredicate::SLE,
        }
    }
}

enum RetTypeEnum<'ctx> {
    Void(VoidType<'ctx>),
    Basic(BasicTypeEnum<'ctx>),
}

impl<'ctx> RetTypeEnum<'ctx> {
    fn fn_type(
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
