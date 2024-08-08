#![cfg(feature = "llvm")]
/// 此包代码应该遵循以下原则：
/// 1. 所有Builder的字段都应该private，不应该被外部直接访问
/// 2. 所有涉及llvm类型的函数（包括参数或返回值）都应该是private的
use std::{
    borrow::Cow,
    cell::{Cell, RefCell},
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    path::{Path, PathBuf},
    sync::{
        atomic::{AtomicI64, Ordering},
        Arc,
    },
};

use immix::{IntEnum, ObjectType};
use inkwell::{
    attributes::Attribute,
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    debug_info::*,
    module::{FlagBehavior, Linkage, Module},
    targets::{InitializationConfig, Target, TargetMachine},
    types::{
        AsTypeRef, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, PointerType,
        StructType, VoidType,
    },
    values::{
        AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue,
        PointerValue,
    },
    AddressSpace, FloatPredicate, IntPredicate, OptimizationLevel,
};
use llvm_sys::{core::LLVMStructSetBody, prelude::LLVMTypeRef};
use rustc_hash::FxHashMap;
use ustr::Ustr;

use crate::ast::{
    ctx::{PLSymbolData, STUCK_FNS},
    diag::PLDiag,
    node::{cast::get_option_type, function::generator::CtxFlag},
    pltype::{get_type_deep, ClosureType, TraitImplAble},
    tokens::TokenType,
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

const CALL_CONV: u32 = 0;
// const DW_TAG_REFERENCE_TYPE: u32 = 16;
fn get_dw_ate_encoding(pritp: &PriType) -> u32 {
    match pritp {
        PriType::I8 | PriType::I16 | PriType::I32 | PriType::I64 | PriType::I128 => DW_ATE_SIGNED,
        PriType::U8
        | PriType::U16
        | PriType::U32
        | PriType::U64
        | PriType::U128
        | PriType::CHAR => DW_ATE_UNSIGNED,
        PriType::F32 | PriType::F64 => DW_ATE_FLOAT,
        PriType::BOOL => DW_ATE_BOOLEAN,
    }
}

fn get_nth_mark_fn(f: FunctionValue, n: u32) -> PointerValue {
    f.get_nth_param(n).unwrap().into_pointer_value()
}

pub fn create_llvm_deps<'ctx>(
    context: &'ctx Context,
    dir: &str,
    file: &str,
    opt: OptimizationLevel,
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
    let tm = get_target_machine(opt);
    module.set_triple(&tm.get_triple());
    module.set_data_layout(&tm.get_target_data().get_data_layout());
    (module, builder, dibuilder, compile_unit, tm)
}

#[derive(Clone)]
pub struct LLVMBuilder<'a, 'ctx> {
    handle_table: Arc<RefCell<FxHashMap<ValueHandle, AnyValueEnum<'ctx>>>>,
    reserved_handle_table: Arc<RefCell<FxHashMap<AnyValueEnum<'ctx>, ValueHandle>>>,

    block_table: Arc<RefCell<FxHashMap<BlockHandle, BasicBlock<'ctx>>>>,
    block_reverse_table: Arc<RefCell<FxHashMap<BasicBlock<'ctx>, BlockHandle>>>,
    context: &'ctx Context,                // llvm context
    builder: &'a Builder<'ctx>,            // llvm builder
    module: &'a Module<'ctx>,              // llvm module
    dibuilder: &'a DebugInfoBuilder<'ctx>, // debug info builder
    diunit: &'a DICompileUnit<'ctx>,       // debug info unit
    targetmachine: &'a TargetMachine,      // might be used in debug info
    discope: Cell<DIScope<'ctx>>,          // debug info scope
    ditypes_placeholder: Arc<RefCell<FxHashMap<Ustr, RefCell<Vec<DIDerivedType<'ctx>>>>>>, // hold the generated debug info type place holder
    ditypes: Arc<RefCell<FxHashMap<Ustr, DIType<'ctx>>>>, // hold the generated debug info type
    optimized: Arc<RefCell<bool>>,
    used: Arc<RefCell<Vec<FunctionValue<'ctx>>>>,
    difile: Cell<DIFile<'ctx>>,
    optlevel: OptimizationLevel,
    debug: bool,
    print_escaped: bool,
}

pub fn get_target_machine(_level: OptimizationLevel) -> TargetMachine {
    // see issue https://github.com/llvm/llvm-project/issues/75162
    let level = OptimizationLevel::None;
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
    fn gc_ptr_ty(&self) -> PointerType<'ctx> {
        self.context.ptr_type(AddressSpace::from(1))
    }
    fn build_load_raw(&self, ptr: ValueHandle, name: &str, tp: BasicTypeEnum<'ctx>) -> ValueHandle {
        let llvm_type = tp;
        let ptr = self.get_llvm_value(ptr).unwrap();
        let ptr = ptr.into_pointer_value();
        let ptr = self.builder.build_load(llvm_type, ptr, name).unwrap();
        self.get_llvm_value_handle(&ptr.as_any_value_enum())
    }
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
        dibuilder: &'a DebugInfoBuilder<'ctx>,
        diunit: &'a DICompileUnit<'ctx>,
        tm: &'a TargetMachine,
        opt: OptimizationLevel,
        debug: bool,
        print_escaped: bool,
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
            reserved_handle_table: Arc::new(RefCell::new(FxHashMap::default())),
            block_table: Arc::new(RefCell::new(FxHashMap::default())),
            block_reverse_table: Arc::new(RefCell::new(FxHashMap::default())),
            optimized: Arc::new(RefCell::new(false)),
            used: Default::default(),
            difile: Cell::new(diunit.get_file()),
            optlevel: opt,
            debug,
            print_escaped,
        }
    }
    /// 分配内存使用的函数
    ///
    /// 需要自行选择分配函数
    fn alloc_with_f(
        &self,
        name: &str,
        pltype: &PLType,
        ctx: &mut Ctx<'a>,
        declare: Option<Pos>,
        fname: &str,
    ) -> ValueHandle {
        let mut ret_handle = self.alloc_raw(name, pltype, ctx, declare, fname);
        if ctx.ctx_flag == CtxFlag::InGeneratorYield
            && (declare.is_some() || fname != "DioGC__malloc")
        {
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

            // 我们现在在alloca block上（第一个block），gnerator yield函数每次进入都会执行这个
            // block，所以在这里我们要设置好所有的变量初始值，将他们从generator ctx中取出。
            let tp = self
                .get_basic_type_op(&data.borrow().ctx_tp.as_ref().unwrap().borrow(), ctx)
                .unwrap()
                .into_struct_type();
            let count = add_field(tp, self.gc_ptr_ty().into());
            let i = count - 1;
            let data_ptr = self
                .build_struct_gep(
                    data.borrow().yield_ctx_handle,
                    i,
                    name,
                    &data.borrow().ctx_tp.as_ref().unwrap().borrow(),
                    ctx,
                )
                .unwrap();

            self.builder.position_at_end(lb);

            // 到目前正在生成代码的block上，这里是malloc函数所在的地方。
            // malloc之后要将生成的内存保存到generator ctx中，以便下次进入时可以取出来。
            // 但是函数参数除外，在一般函数的逻辑中函数进入后要将参数保存到堆中，然而
            // generator yield函数的参数在generator init的时候就已经分配好了内存
            // 而且保存在了generator ctx中，所以这里的malloc其实是不需要的，而且不能回
            // 存到ctx里，如果回存会导致参数被覆盖。
            if !data.borrow_mut().is_para {
                self.build_store(data_ptr, ret_handle);
            }

            ret_handle = data_ptr;

            let id = data.borrow().table.len().to_string();
            data.borrow_mut().table.insert(
                format!("{}{}", name, id).into(),
                PLSymbolData {
                    value: data_ptr,
                    pltype: Arc::new(RefCell::new(pltype.clone())),
                    range: Default::default(),
                    refs: None,
                },
            );
        }

        ret_handle
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
        let (p, _) = self.gc_malloc(name, ctx, pltype, malloc_fn, declare);
        let llvm_tp = self.get_basic_type_op(pltype, ctx).unwrap();
        if let PLType::Struct(tp) = pltype {
            if !tp.atomic {
                let f = self.get_or_insert_st_visit_fn_handle(tp, ctx);
                let i = self
                    .builder
                    .build_ptr_to_int(
                        f.as_global_value().as_pointer_value(),
                        self.context.i64_type(),
                        "_rtti",
                    )
                    .unwrap();
                let vtable = self
                    .builder
                    .build_struct_gep(llvm_tp, p, 0, "rtti")
                    .unwrap();
                self.builder.build_store(vtable, i).unwrap();
            }
        } else if let PLType::Arr(tp) = pltype {
            let f = self.gen_or_get_arr_visit_function(ctx, tp);
            let i = self
                .builder
                .build_ptr_to_int(
                    f.as_global_value().as_pointer_value(),
                    self.context.i64_type(),
                    "_vtable",
                )
                .unwrap();
            let vtable = self
                .builder
                .build_struct_gep(llvm_tp, p, 0, "vtable")
                .unwrap();
            self.builder.build_store(vtable, i).unwrap();
        }
        if let Some(pos) = declare {
            if pos != Default::default() {
                self.build_dbg_location(pos);
                self.insert_var_declare(
                    name,
                    pos,
                    pltype,
                    self.get_llvm_value_handle(&p.as_any_value_enum()),
                    ctx,
                );
            }
        }
        let v_heap = self.get_llvm_value_handle(&p.as_any_value_enum());
        v_heap
    }

    fn gc_malloc(
        &self,
        name: &str,
        ctx: &mut Ctx<'a>,
        tp: &PLType,
        malloc_fn: &str,
        declare: Option<Pos>,
    ) -> (PointerValue<'ctx>, BasicTypeEnum<'ctx>) {
        let lb = self.builder.get_insert_block().unwrap();
        let alloca = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap()
            .get_first_basic_block()
            .unwrap();
        // let obj_type = tp.get_immix_type().int_value();
        let f = self.get_gc_mod_f(ctx, malloc_fn);
        let llvmtp = self.get_basic_type_op(tp, ctx).unwrap();
        let immix_tp = self
            .context
            .i8_type()
            .const_int(tp.get_immix_type().int_value() as u64, false);
        let td = self.targetmachine.get_target_data();
        let size = td.get_store_size(&llvmtp);
        if size == 0 {
            // return null
            let null = self.gc_ptr_ty().const_null();
            return (null, llvmtp);
        }
        let mut size = self.context.i64_type().const_int(size, false);
        if name == "___ctx" {
            // generator ctx, use stack variable as size
            self.builder.position_at_end(alloca);
            let stack_ptr = self
                .builder
                .build_alloca(self.context.i64_type(), "ctx_tp_ptr")
                .unwrap();
            ctx.generator_data
                .as_ref()
                .unwrap()
                .borrow_mut()
                .ctx_size_handle = self.get_llvm_value_handle(&stack_ptr.as_any_value_enum());

            self.builder.position_at_end(lb);

            size = self
                .builder
                .build_load(self.context.i64_type(), stack_ptr, "ctx_tp")
                .unwrap()
                .into_int_value();
        }
        if name == "retvalue_generator" {
            self.builder.position_at_end(alloca);
        }

        let rsp = self.get_sp();
        let call_site_value = self
            .builder
            .build_call(
                f,
                &if malloc_fn == "DioGC__malloc" {
                    [size.into(), immix_tp.into(), rsp.into()].to_vec()
                } else {
                    [size.into(), immix_tp.into()].to_vec()
                },
                &format!("heapptr_{}", name),
            )
            .unwrap();
        if !matches!(tp, PLType::Arr(_)) {
            let pos = match declare {
                Some(Pos {
                    line: 0,
                    column: 0,
                    offset: 0,
                })
                | None => Cow::Borrowed(""),
                Some(pos) => Cow::Owned(format!(
                    "`{}` at {}:{}:{}",
                    name, &ctx.plmod.path, pos.line, pos.column
                )),
            };
            call_site_value.add_attribute(
                inkwell::attributes::AttributeLoc::Return,
                self.context.create_string_attribute("pl_ordinary", &pos),
            );
        }
        let heapptr = call_site_value.try_as_basic_value().left().unwrap();
        self.builder.position_at_end(lb);

        let casted_result = heapptr;

        if !tp.is_atomic() {
            // TODO: force user to manually init all structs, so we can remove this memset
            self.builder
                .build_memset(
                    casted_result.into_pointer_value(),
                    td.get_abi_alignment(&llvmtp),
                    self.context.i8_type().const_zero(),
                    size,
                )
                .unwrap();
        }

        let cb = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(alloca);
        if alloca.get_terminator().is_some() {
            panic!("alloca block should not have terminator yet")
        }
        // self.builder
        // .build_memset(
        //     stack_ptr,
        //     td.get_abi_alignment(&self.i8ptr()),
        //     self.context.i8_type().const_zero(),
        //     self.context.i64_type().const_int(  td.get_store_size(&self.i8ptr()),false),
        // )
        // .unwrap();
        self.builder.position_at_end(lb);
        // self.set_root(self.get_llvm_value_handle( &casted_result.as_any_value_enum()), self.get_llvm_value_handle( &stack_ptr.as_any_value_enum()));
        self.builder.position_at_end(cb);

        if let PLType::Arr(arr) = tp {
            // init the array size
            if arr.size_handle != 0 {
                let mf = "DioGC__malloc";
                let f = self.get_gc_mod_f(ctx, mf);

                // let f = self.get_malloc_f(ctx, "DioGC__malloc_no_collect");
                let etp = self
                    .get_basic_type_op(&arr.element_type.borrow(), ctx)
                    .unwrap();
                let size = td.get_store_size(&etp);
                let size = self.context.i64_type().const_int(size, false);
                let arr_len = self
                    .get_llvm_value(arr.size_handle)
                    .unwrap()
                    .into_int_value();
                let arr_size = self
                    .builder
                    .build_int_mul(arr_len, size, "arr_size")
                    .unwrap();
                // add 16 bytes (rtti)
                let arr_size = self
                    .builder
                    .build_int_add(arr_size, self.context.i64_type().const_int(16, false), "")
                    .unwrap();
                let arr_size = self
                    .builder
                    .build_int_z_extend_or_bit_cast(arr_size, self.context.i64_type(), "arr_size")
                    .unwrap();
                let len_ptr = self
                    .builder
                    .build_struct_gep(llvmtp, casted_result.into_pointer_value(), 2, "arr_len")
                    .unwrap();
                self.builder.build_store(len_ptr, arr_len).unwrap();

                let rsp = self.get_sp();
                let arr_space = self
                    .builder
                    .build_call(
                        f,
                        &if mf == "DioGC__malloc" {
                            [
                                arr_size.into(),
                                self.context
                                    .i8_type()
                                    .const_int(immix::ObjectType::Trait.int_value() as u64, false)
                                    .into(),
                                rsp.into(),
                            ]
                            .to_vec()
                        } else {
                            [
                                arr_size.into(),
                                self.context
                                    .i8_type()
                                    .const_int(immix::ObjectType::Trait.int_value() as u64, false)
                                    .into(),
                            ]
                            .to_vec()
                        },
                        "arr_space",
                    )
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap();

                self.builder
                    .build_memset(
                        arr_space.into_pointer_value(),
                        8,
                        self.context.i8_type().const_zero(),
                        arr_size,
                    )
                    .unwrap();
                // store arr rtti
                // it's the arr pointer at offset 8 of arr_space
                let arr_space = unsafe {
                    self.builder
                        .build_in_bounds_gep(
                            self.context.i8_type(),
                            arr_space.into_pointer_value(),
                            &[self.context.i64_type().const_int(8, false)],
                            "arr_space",
                        )
                        .unwrap()
                };
                // rtti is the heap pointer
                let rtti = self
                    .builder
                    .build_ptr_to_int(
                        casted_result.into_pointer_value(),
                        self.context.i64_type(),
                        "rtti",
                    )
                    .unwrap();
                self.builder.build_store(arr_space, rtti).unwrap();

                let arr_ptr = self
                    .builder
                    .build_struct_gep(llvmtp, casted_result.into_pointer_value(), 1, "arr_ptr")
                    .unwrap();
                // offset arr_space 8 bytes (rtti)
                let arr_space = unsafe {
                    self.builder
                        .build_in_bounds_gep(
                            self.context.i8_type(),
                            arr_space,
                            &[self.context.i64_type().const_int(8, false)],
                            "arr_space",
                        )
                        .unwrap()
                };
                self.builder.build_store(arr_ptr, arr_space).unwrap();
            }
        }

        self.builder.position_at_end(lb);
        (casted_result.into_pointer_value(), llvmtp)
    }

    /// # get_sp
    ///
    /// get the stack pointer, return as i64
    fn get_sp(&self) -> inkwell::values::IntValue<'ctx> {
        let fp_asm_ftp = self
            .context
            .ptr_type(AddressSpace::from(0))
            .fn_type(&[], false);
        #[cfg(target_arch = "x86_64")]
        let rspf = self.context.create_inline_asm(
            fp_asm_ftp,
            "mov %rsp, $0".to_string(),
            "=r".to_string(),
            false,
            true,
            None,
            false,
        );
        #[cfg(target_arch = "aarch64")]
        let rspf = self.context.create_inline_asm(
            fp_asm_ftp,
            "mov $0, sp".to_string(),
            "=r".to_string(),
            false,
            true,
            None,
            false,
        );
        let rsp = self
            .builder
            .build_indirect_call(fp_asm_ftp, rspf, &[], "rsp")
            .unwrap();
        rsp.add_attribute(
            inkwell::attributes::AttributeLoc::Function,
            self.context.create_string_attribute("gc-leaf-function", ""),
        );
        rsp.add_attribute(
            inkwell::attributes::AttributeLoc::Function,
            self.context
                .create_enum_attribute(Attribute::get_named_enum_kind_id("allockind"), 1),
        );
        let rsp = self
            .builder
            .build_ptr_to_int(
                rsp.try_as_basic_value().unwrap_left().into_pointer_value(),
                self.context.i64_type(),
                "rspi",
            )
            .unwrap();
        rsp
    }

    fn get_gc_mod_f(&self, ctx: &mut Ctx<'a>, malloc_fn: &str) -> FunctionValue<'ctx> {
        let mut root_ctx = &*ctx;
        while let Some(f) = root_ctx.root {
            root_ctx = f
        }
        let gcmod = root_ctx
            .plmod
            .submods
            .get(&"gc".into())
            .map(|rc| rc.as_ref())
            .unwrap_or(&root_ctx.plmod);
        let f: FNValue = gcmod
            .types
            .get(&malloc_fn.into())
            .unwrap()
            .borrow()
            .clone()
            .try_into()
            .unwrap();
        let f = self.get_or_insert_fn(&f, ctx);
        f.0
    }

    fn get_cur_di_file(&self) -> DIFile<'ctx> {
        self.difile.get()
    }

    /// # get_llvm_value_handle
    ///
    /// get_llvm_value_handle tries to get the [ValueHandle] of a [AnyValueEnum][inkwell::values::AnyValueEnum] value,
    /// it returns the handle value if it finds the value appears inside the map already,
    /// otherwise it will insert the value into the map and create a new handle for it.
    fn get_llvm_value_handle(&self, value: &AnyValueEnum<'ctx>) -> ValueHandle {
        let len = self.handle_table.borrow().len();

        // it refers the handle value if we need to insert the value
        let should_insert_handle = len + 1;

        // whether the value exists inside the handle table
        let handle_value = match self.reserved_handle_table.borrow().get(value) {
            Some(handle) => *handle,
            None => should_insert_handle,
        };

        // if the value doesn't exist in current handle table, insert it in the tables
        if handle_value == should_insert_handle {
            self.handle_table.borrow_mut().insert(handle_value, *value);
            self.reserved_handle_table
                .borrow_mut()
                .insert(*value, handle_value);
        }
        handle_value
    }
    #[allow(dead_code)]
    fn get_or_insert_print_fn(&self, name: &str) -> FunctionValue<'ctx> {
        if let Some(f) = self.module.get_function(name) {
            return f;
        }
        let ftp = self
            .context
            .void_type()
            .fn_type(&[self.context.i64_type().into()], false);
        let f = self.module.add_function(name, ftp, None);
        f
    }

    fn visit_fn_tp(&self) -> FunctionType<'ctx> {
        let ptrtp = self.gc_ptr_ty();
        self.context.void_type().fn_type(
            &[
                ptrtp.into(),
                ptrtp.into(),
                ptrtp.into(),
                ptrtp.into(),
                ptrtp.into(),
            ],
            false,
        )
    }

    fn gen_or_get_arr_visit_function(&self, ctx: &mut Ctx<'a>, v: &ARRType) -> FunctionValue<'ctx> {
        let _i8ptrtp = self.gc_ptr_ty();
        let currentbb = self.builder.get_insert_block();
        self.builder.unset_current_debug_location();
        let ty = self.arr_type().into_struct_type();
        let ftp = self.visit_fn_tp();
        // windows linker won't recognize flags with special caracters (llvm.used will add linker flags
        // to prevent symbol trim), so we need to do a hash here to remove the special caracters
        let mut hasher = DefaultHasher::new();
        let name = v.get_full_name();

        (format!("{}@{}", name.clone(), ctx.plmod.path)).hash(&mut hasher);
        let fname = &format!("arr_visit{:x}_visitorf@", hasher.finish());
        // eprintln!("name: {}, hashname: {}", name, fname);
        if let Some(f) = self.module.get_function(fname) {
            return f;
        }
        let f = self.module.add_function(fname, ftp, Some(Linkage::Private));
        self.used.borrow_mut().push(f);
        // the array is a struct, the first field is the visit function,
        // the second field is the real array, the third field is it's length
        // array struct it self is the first parameter
        // the other three parameters are the visit function for different type
        let bb = self.context.append_basic_block(f, "entry");
        self.builder.position_at_end(bb);
        let arr = f.get_nth_param(0).unwrap().into_pointer_value();
        let real_arr_raw = self.builder.build_struct_gep(ty, arr, 1, "arr").unwrap();

        let visitor = f.get_nth_param(1).unwrap().into_pointer_value();
        let visit_ptr_f = get_nth_mark_fn(f, 2);
        // complex type needs to provide a visit function by itself
        // which is stored in the first field of the struct
        let visit_complex_f = get_nth_mark_fn(f, 3);
        let visit_trait_f = get_nth_mark_fn(f, 4);

        // call the visit_ptr function
        let casted = real_arr_raw;
        self.builder
            .build_indirect_call(
                self.context.void_type().fn_type(
                    &[visitor.get_type().into(), casted.get_type().into()],
                    false,
                ),
                get_nth_mark_fn(f, 2),
                &[visitor.into(), casted.into()],
                "call",
            )
            .unwrap();

        // mark real_arr_raw may evacuate, so we need to load after mark.
        let real_arr = self
            .builder
            .build_load(
                ty.get_field_type_at_index(1).unwrap(),
                real_arr_raw,
                "loaded_arr",
            )
            .unwrap()
            .into_pointer_value();

        let loop_var = self
            .builder
            .build_alloca(self.context.i64_type(), "i")
            .unwrap();
        self.builder
            .build_store(loop_var, self.context.i64_type().const_zero())
            .unwrap();
        // arr is the real array
        let arr_len = self
            .builder
            .build_struct_gep(ty, arr, 2, "arr_len")
            .unwrap();
        let arr_len = self
            .builder
            .build_load(self.context.i64_type(), arr_len, "arr_len")
            .unwrap()
            .into_int_value();

        // generate a loop, iterate the real array, and mark each element if needed
        let condbb = self.context.append_basic_block(f, "cond");
        self.builder.build_unconditional_branch(condbb).unwrap();
        self.builder.position_at_end(condbb);
        let i = self
            .builder
            .build_load(self.context.i64_type(), loop_var, "i")
            .unwrap()
            .into_int_value();
        let cond = self
            .builder
            .build_int_compare(IntPredicate::ULT, i, arr_len, "cond")
            .unwrap();
        let loopbb = self.context.append_basic_block(f, "loop");
        let endbb = self.context.append_basic_block(f, "end");
        self.builder
            .build_conditional_branch(cond, loopbb, endbb)
            .unwrap();
        self.builder.position_at_end(loopbb);
        let i = self
            .builder
            .build_load(self.context.i64_type(), loop_var, "i")
            .unwrap()
            .into_int_value();
        let elm_tp = get_type_deep(v.element_type.clone());
        let llvm_elm_tp = self.get_basic_type_op(&elm_tp.borrow(), ctx).unwrap();
        let elm = unsafe {
            self.builder
                .build_in_bounds_gep(llvm_elm_tp, real_arr, &[i], "elm")
        }
        .unwrap();

        match &*elm_tp.borrow() {
            PLType::Arr(_) | PLType::Struct(STType { atomic: false, .. }) => {
                let casted = elm;
                // call the visit_complex function
                self.builder
                    .build_indirect_call(
                        self.context.void_type().fn_type(
                            &[visitor.get_type().into(), casted.get_type().into()],
                            false,
                        ),
                        visit_complex_f,
                        &[visitor.into(), casted.into()],
                        "call",
                    )
                    .unwrap();
            }
            PLType::Pointer(_) => {
                // call the visit_ptr function
                let casted = elm;
                self.builder
                    .build_indirect_call(
                        self.context.void_type().fn_type(
                            &[visitor.get_type().into(), casted.get_type().into()],
                            false,
                        ),
                        visit_ptr_f,
                        &[visitor.into(), casted.into()],
                        "call",
                    )
                    .unwrap();
            }
            PLType::Trait(_) | PLType::Union(_) | PLType::Closure(_) => {
                // call the visit_trait function
                let casted = elm;
                self.builder
                    .build_indirect_call(
                        self.context.void_type().fn_type(
                            &[visitor.get_type().into(), casted.get_type().into()],
                            false,
                        ),
                        visit_trait_f,
                        &[visitor.into(), casted.into()],
                        "call",
                    )
                    .unwrap();
            }
            PLType::Fn(_)
            | PLType::Primitive(_)
            | PLType::Void
            | PLType::Generic(_)
            | PLType::PlaceHolder(_)
            | PLType::PartialInferred(_)
            | PLType::Struct(_)
            | PLType::Unknown => (),
        }
        let i = self
            .builder
            .build_load(self.context.i64_type(), loop_var, "i")
            .unwrap()
            .into_int_value();
        let i = self
            .builder
            .build_int_add(i, self.context.i64_type().const_int(1, false), "i")
            .unwrap();
        self.builder.build_store(loop_var, i).unwrap();
        self.builder.build_unconditional_branch(condbb).unwrap();
        self.builder.position_at_end(endbb);

        self.builder.build_return(None).unwrap();
        if let Some(currentbb) = currentbb {
            self.builder.position_at_end(currentbb);
        }
        f
    }

    fn get_llvm_value(&self, handle: ValueHandle) -> Option<AnyValueEnum<'ctx>> {
        self.handle_table.borrow().get(&handle).copied()
    }
    fn get_llvm_value_raw(&self, handle: ValueHandle) -> Option<AnyValueEnum<'ctx>> {
        self.handle_table.borrow().get(&handle).copied()
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

    /// # get_pri_basic_type
    ///
    /// get_pri_basic_type converts a pivot-lang primitive type into a llvm primitive type
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
            PriType::CHAR => self.context.i32_type().as_basic_type_enum(),
        }
    }
    fn get_or_insert_st_visit_fn_handle(
        &self,
        st: &STType,
        ctx: &mut Ctx<'a>,
    ) -> FunctionValue<'ctx> {
        let fields = ctx.run_in_type_mod(st, |ctx, st| {
            st.fields
                .iter()
                .map(|(_, f)| {
                    f.typenode
                        .get_type(ctx, &self.clone().into(), false)
                        .unwrap()
                })
                .collect::<Vec<_>>()
        });

        self.gen_st_visit_function(ctx, st, &fields);
        let llvmname = format!("{}_visitorf@", st.get_full_name());
        self.module.get_function(&llvmname).unwrap()
    }

    /// # get_fn_type
    ///
    /// get_fn_type returns the inkwell function type for a pivot-lang function value.
    fn get_fn_type(&self, fnvalue: &FNValue, ctx: &mut Ctx<'a>) -> FunctionType<'ctx> {
        ctx.protect_generic_context(&fnvalue.fntype.generic_map, |ctx| {
            ctx.run_in_type_mod(fnvalue, |ctx, fnvalue| {
                let mut param_types = vec![];
                for param_pltype in fnvalue.fntype.param_pltypes.iter() {
                    param_types.push(
                        self.get_param_type(
                            &param_pltype
                                .get_type(ctx, &self.clone().into(), true)
                                .unwrap()
                                .borrow(),
                            ctx,
                            !fnvalue.is_declare,
                        )
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
                        !fnvalue.is_declare && fnvalue.name != "main",
                    )
                    .fn_type(&param_types, false);

                fn_type
            })
        })
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
            PLType::Struct(s) => Some(self.struct_type(s, ctx).as_basic_type_enum()),
            PLType::Trait(s) => Some(self.struct_type(s, ctx).as_basic_type_enum()),
            PLType::Arr(_) => Some(self.arr_type()),
            PLType::Primitive(t) => Some(self.get_pri_basic_type(t)),
            PLType::Void => None,
            PLType::Pointer(_) | PLType::Fn(_) => Some(self.gc_ptr_ty().as_basic_type_enum()),
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
                let fields = vec![self.context.i64_type().into(), self.gc_ptr_ty().into()];
                Some(self.context.struct_type(&fields, false).into())
            }
            PLType::Closure(_) => {
                // all closures are represented as a struct with a function pointer and an i8ptr(point to closure data)
                let fields = vec![self.gc_ptr_ty().into(), self.gc_ptr_ty().into()];
                Some(self.context.struct_type(&fields, false).into())
            }
            PLType::Unknown => None,
            PLType::PartialInferred(_) => None,
        }
    }
    /// # get_ret_type
    ///
    /// get the return type
    ///
    /// ## About `is_gc_statepoint`
    ///
    /// Whether the function can be treated as a gc statepoint
    ///
    /// There's an issue with llvm statepoint api when returning large
    /// structures (larger than 24 bit) [[1]].
    /// To bypass this issue, we make function return a pointer
    /// instead if it's possible to be a gc statepoint.
    ///
    /// Most of the functions can be treated as a gc statepoint,
    /// except ffi functions and main function.
    ///
    ///
    /// [1]:https://github.com/llvm/llvm-project/issues/74612
    fn get_ret_type(
        &self,
        pltp: &PLType,
        ctx: &mut Ctx<'a>,
        is_gc_statepoint: bool,
    ) -> RetTypeEnum<'ctx> {
        match pltp {
            PLType::Void => RetTypeEnum::Void(self.context.void_type()),
            _ => RetTypeEnum::Basic({
                let tp = self.get_basic_type_op(pltp, ctx).unwrap();
                if is_gc_statepoint
                    && !matches!(
                        &*get_type_deep(Arc::new(RefCell::new(pltp.clone()))).borrow(),
                        PLType::Pointer(_) | PLType::Primitive(_)
                    )
                {
                    self.gc_ptr_ty().as_basic_type_enum()
                } else {
                    tp
                }
            }),
        }
    }

    fn get_param_type(
        &self,
        pltp: &PLType,
        ctx: &mut Ctx<'a>,
        is_gc_statepoint: bool,
    ) -> BasicTypeEnum<'ctx> {
        let tp = self.get_basic_type_op(pltp, ctx).unwrap();
        if is_gc_statepoint
            && !matches!(
                &*get_type_deep(Arc::new(RefCell::new(pltp.clone()))).borrow(),
                PLType::Pointer(_) | PLType::Primitive(_)
            )
        {
            self.gc_ptr_ty().as_basic_type_enum()
        } else {
            tp
        }
    }
    /// array type in fact is a struct with three fields,
    /// the first is a function pointer to the visit function(used in gc)
    /// the second is the array itself
    /// the third is the length of the array
    fn arr_type(&self) -> BasicTypeEnum<'ctx> {
        self.context
            .struct_type(
                &[
                    self.context.i64_type().as_basic_type_enum(),
                    self.gc_ptr_ty().as_basic_type_enum(),
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
            Err(_) => ctx.get_type(&"i64".into(), Default::default()).unwrap().typ,
        };
        let di_type = self.get_ditype(&field_pltype.borrow(), ctx);
        let debug_type = di_type.unwrap();
        let td = self.targetmachine.get_target_data();
        let (size, align, offset_1) =
            if matches!(*RefCell::borrow(&field_pltype), PLType::Pointer(_)) {
                let ptr = self.gc_ptr_ty();
                (td.get_bit_size(&ptr), td.get_abi_alignment(&ptr), 0)
            } else {
                (
                    debug_type.get_size_in_bits(),
                    debug_type.get_align_in_bits(),
                    debug_type.get_offset_in_bits(),
                )
            };
        (
            self.dibuilder
                .create_member_type(
                    self.get_cur_di_file().as_debug_info_scope(),
                    &field.name,
                    self.get_cur_di_file(),
                    field.range.start.line as u32,
                    size,
                    align,
                    offset + offset_1,
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
                let arr_st_tp = self.arr_type().into_struct_type();
                let align = td.get_preferred_alignment(etp);
                let st_size = td.get_bit_size(&arr_st_tp);
                let vtabledi = self.get_ditype(&PLType::Primitive(PriType::U64), ctx)?;
                let offset = td.offset_of_element(&arr_st_tp, 0).unwrap();
                let vtabletp = self.dibuilder.create_member_type(
                    self.get_cur_di_file().as_debug_info_scope(),
                    "_vtable",
                    self.get_cur_di_file(),
                    0,
                    vtabledi.get_size_in_bits(),
                    vtabledi.get_align_in_bits(),
                    offset * 8,
                    DIFlags::PUBLIC,
                    vtabledi,
                );
                let arrdi = self
                    .dibuilder
                    .create_pointer_type(
                        "",
                        elemdi,
                        64,
                        elemdi.get_align_in_bits(),
                        AddressSpace::from(1),
                    )
                    .as_type();
                let offset = td.offset_of_element(&arr_st_tp, 1).unwrap();
                let arrtp = self.dibuilder.create_member_type(
                    self.get_cur_di_file().as_debug_info_scope(),
                    "_arr",
                    self.get_cur_di_file(),
                    0,
                    arrdi.get_size_in_bits(),
                    arrdi.get_align_in_bits(),
                    offset * 8,
                    DIFlags::PUBLIC,
                    arrdi,
                );
                let offset = td.offset_of_element(&arr_st_tp, 2).unwrap();
                let lentp = self.dibuilder.create_member_type(
                    self.get_cur_di_file().as_debug_info_scope(),
                    "_len",
                    self.get_cur_di_file(),
                    0,
                    vtabledi.get_size_in_bits(),
                    vtabledi.get_align_in_bits(),
                    offset * 8,
                    DIFlags::PUBLIC,
                    vtabledi,
                );
                let st = self
                    .dibuilder
                    .create_struct_type(
                        self.get_cur_di_file().as_debug_info_scope(),
                        &format!("[{}]", arr.element_type.borrow().get_name()),
                        self.get_cur_di_file(),
                        0,
                        st_size,
                        align,
                        DIFlags::PUBLIC,
                        None,
                        &[vtabletp.as_type(), arrtp.as_type(), lentp.as_type()],
                        0,
                        None,
                        &format!("[{}]", arr.element_type.borrow().get_name()),
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
                        self.get_cur_di_file().as_debug_info_scope(),
                        &x.name,
                        self.get_cur_di_file(),
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
                        64,
                        st.get_align_in_bits(),
                        AddressSpace::from(1),
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
                let etp = &self.gc_ptr_ty().as_basic_type_enum();
                let align = td.get_preferred_alignment(etp);
                let di = self
                    .dibuilder
                    .create_pointer_type("", elemdi, 64, align, AddressSpace::from(1))
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
                                self.get_cur_di_file().as_debug_info_scope(),
                                &tp.get_name(),
                                self.get_cur_di_file(),
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
                let ptr = self.gc_ptr_ty();
                let tp = self.dibuilder.create_union_type(
                    self.get_cur_di_file().as_debug_info_scope(),
                    "data",
                    self.get_cur_di_file(),
                    u.range.start.line as u32 + 1,
                    td.get_bit_size(&ptr),
                    td.get_abi_alignment(&ptr),
                    DIFlags::PUBLIC,
                    &ditps,
                    0,
                    &format!("{}_data", u.name.clone()),
                );

                let tag_di = self
                    .dibuilder
                    .create_basic_type(
                        &format!("{}_tag", u.name.clone()),
                        td.get_bit_size(&self.context.i64_type()),
                        get_dw_ate_encoding(&PriType::I64),
                        0,
                    )
                    .unwrap()
                    .as_type();
                let tag = self.dibuilder.create_member_type(
                    self.get_cur_di_file().as_debug_info_scope(),
                    "tag",
                    self.get_cur_di_file(),
                    u.range.start.line as u32 + 1,
                    td.get_bit_size(&self.context.i64_type()),
                    td.get_abi_alignment(&self.context.i64_type()),
                    0,
                    DIFlags::PUBLIC,
                    tag_di,
                );
                let data = self.dibuilder.create_member_type(
                    self.get_cur_di_file().as_debug_info_scope(),
                    "data",
                    self.get_cur_di_file(),
                    u.range.start.line as u32 + 1,
                    td.get_bit_size(&ptr),
                    td.get_abi_alignment(&ptr),
                    td.get_bit_size(&ptr),
                    DIFlags::PUBLIC,
                    tp.as_type(),
                );
                let st = self.dibuilder.create_struct_type(
                    self.get_cur_di_file().as_debug_info_scope(),
                    &format!("union::{}", u.name),
                    self.get_cur_di_file(),
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
                    .unwrap_or_default()
                    .borrow()
                    .iter()
                {
                    let align = td.get_preferred_alignment(&utp);
                    let realtp = self.dibuilder.create_pointer_type(
                        "",
                        st.as_type(),
                        64,
                        align,
                        AddressSpace::from(1),
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
            PLType::Closure(_) => self.get_ditype(&PLType::Primitive(PriType::I64), ctx),
            PLType::Unknown => None, // TODO
            PLType::PartialInferred(_) => None,
        }
    }

    /// # get_or_insert_fn
    ///
    /// it returns the emitted function from llvm if it exists,
    /// otherwise it will emit the function value and the return the new value.
    ///
    /// The bool means whether the function exists in the llvm builder or not.
    fn get_or_insert_fn(&self, pltp: &FNValue, ctx: &mut Ctx<'a>) -> (FunctionValue<'ctx>, bool) {
        let final_llvm_name = pltp.append_name_with_generic(pltp.llvmname);

        if let Some(v) = self.module.get_function(&final_llvm_name) {
            return (v, v.count_basic_blocks() != 0);
        }
        let fn_type = self.get_fn_type(pltp, ctx);
        let linkage =
            if pltp.is_declare || pltp.is_modified_by(TokenType::PUB) || pltp.name == "main" {
                Linkage::External
            } else {
                Linkage::Private
            };
        let f = self
            .module
            .add_function(&final_llvm_name, fn_type, Some(linkage));
        if !pltp.is_declare {
            f.set_call_conventions(CALL_CONV);
        }

        if pltp.name.starts_with("DioGC__malloc") {
            f.add_attribute(
                inkwell::attributes::AttributeLoc::Function,
                self.context
                    .create_enum_attribute(Attribute::get_named_enum_kind_id("allockind"), 1),
            );
        }
        (f, false)
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
        constant: bool,
    ) -> PointerValue<'ctx> {
        let global = self.get_global_var_handle(name);
        if global.is_none() {
            let global = self.module.add_global(
                self.get_basic_type_op(&pltype.borrow(), ctx).unwrap(),
                Some(AddressSpace::from(1)),
                name,
            );
            if name.to_uppercase() == "__LLVM_STACKMAPS" {
                global.set_alignment(4);
                global.set_section(Some("__llvm_stackmaps"));
            }
            global.set_linkage(Linkage::External);
            global.set_constant(constant);
            return global.as_pointer_value();
        }
        self.get_llvm_value(global.unwrap())
            .unwrap()
            .into_pointer_value()
    }

    /// # try_load2var_inner
    ///
    /// it returns the element of a pointer,
    /// or array, int, float, struct, vector, and function value rirectly
    fn try_load2var_inner(&self, v: usize, tp: &PLType, ctx: &mut Ctx<'a>) -> Result<usize, ()> {
        let handle = v;
        let v = self.get_llvm_value(handle).unwrap();
        if !v.is_pointer_value() {
            Ok(match v {
                AnyValueEnum::ArrayValue(_)
                | AnyValueEnum::IntValue(_)
                | AnyValueEnum::FloatValue(_)
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
                tp,
                ctx,
            ))
        }
    }
    fn try_load2var_inner_raw(&self, v: usize, tp: BasicTypeEnum<'ctx>) -> Result<usize, ()> {
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
            Ok(self.build_load_raw(
                self.get_llvm_value_handle(&v.into_pointer_value().as_any_value_enum()),
                "loadtmp",
                tp,
            ))
        }
    }

    fn poll_task(
        &self,
        poll_fn: BasicValueEnum<'ctx>,
        self_p: BasicValueEnum<'ctx>,
        second_arg: PointerValue<'ctx>,
    ) -> PointerValue<'ctx> {
        let poll_fn_ty = self.gc_ptr_ty().fn_type(
            &[
                self.context.ptr_type(AddressSpace::from(1)).into(),
                self.context.ptr_type(AddressSpace::from(1)).into(),
            ],
            false,
        );
        let re = self
            .builder
            .build_indirect_call(
                poll_fn_ty,
                poll_fn.into_pointer_value(),
                &[self_p.into(), second_arg.into()],
                "poll_async",
            )
            .unwrap();
        re.as_any_value_enum().into_pointer_value()
    }
}
impl<'a, 'ctx> IRBuilder<'a, 'ctx> for LLVMBuilder<'a, 'ctx> {
    fn is_main(&self, f: ValueHandle) -> bool {
        let f = self.get_llvm_value(f).unwrap().into_function_value();
        f.get_name().to_str().unwrap() == "main"
    }
    fn tag_generator_ctx_as_root(&self, f: ValueHandle, ctx: &mut Ctx<'a>) {
        let f = self.get_llvm_value(f).unwrap().into_function_value();
        let allocab = f.get_first_basic_block().unwrap();
        let prev_bb = self.builder.get_insert_block().unwrap();
        self.builder.position_at_end(allocab);
        let ctx_param = f.get_nth_param(0).unwrap();
        let ctx_handle = self.get_llvm_value_handle(&ctx_param.as_any_value_enum());
        ctx.generator_data
            .as_ref()
            .unwrap()
            .borrow_mut()
            .yield_ctx_handle = ctx_handle;
        self.builder.position_at_end(prev_bb);
    }
    /// # bitcast
    ///
    /// it casts the origin handle into the pointer value
    fn bitcast(
        &self,
        _ctx: &mut Ctx<'a>,
        origin: ValueHandle,
        _to: &PLType,
        _name: &str,
    ) -> ValueHandle {
        let lv = self.get_llvm_value(origin).unwrap();
        let re = if lv.is_function_value() {
            lv.into_function_value()
                .as_global_value()
                .as_pointer_value()
        } else {
            lv.into_pointer_value()
        };
        let new_handle = self.get_llvm_value_handle(&re.as_any_value_enum());
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

        let re = self
            .builder
            .build_pointer_cast(
                lv.into_pointer_value(),
                self.get_basic_type_op(to, ctx).unwrap().into_pointer_type(),
                name,
            )
            .unwrap();
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
                    self.get_cur_di_file(),
                    start.line as u32,
                    start.column as u32,
                )
                .as_debug_info_scope(),
        );
    }

    /// # position_at_end_block
    ///
    /// Set the position of the builder to the end of a basic block refered
    /// by the block handle.
    fn position_at_end_block(&self, block: BlockHandle) {
        self.builder
            .position_at_end(self.block_table.borrow()[&block]);
    }
    /// 返回值的bool：函数是否已有函数体
    fn get_or_insert_fn_handle(&self, pltp: &FNValue, ctx: &mut Ctx<'a>) -> (ValueHandle, bool) {
        let (f, b) = self.get_or_insert_fn(pltp, ctx);
        if pltp.fntype.generic {
            f.set_linkage(Linkage::Private);
        }
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
        constant: bool,
    ) -> ValueHandle {
        self.get_llvm_value_handle(
            &self
                .get_or_add_global_value(name, pltype, ctx, constant)
                .as_any_value_enum(),
        )
    }

    fn build_load(
        &self,
        ptr: ValueHandle,
        name: &str,
        tp: &PLType,
        ctx: &mut Ctx<'a>,
    ) -> ValueHandle {
        let llvm_type = self.get_basic_type_op(tp, ctx).unwrap();
        self.build_load_raw(ptr, name, llvm_type)
    }
    fn try_load2var(
        &self,
        range: Range,
        v: ValueHandle,
        tp: &PLType,
        ctx: &mut Ctx<'a>,
    ) -> Result<ValueHandle, PLDiag> {
        match self.try_load2var_inner(v, tp, ctx) {
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
        pos: Option<Pos>,
    ) -> Option<ValueHandle> {
        let builder = self.builder;
        let f = self.get_llvm_value(f).unwrap();
        let mut f_tp: Option<FunctionType> = None;
        let mut fv = None;
        let f = if f.is_function_value() {
            let ff = f.into_function_value();
            fv = Some(ff);
            f_tp = Some(ff.get_type());
            ff.as_global_value().as_pointer_value()
        } else {
            f.into_pointer_value()
        };
        let (args, tys): (Vec<_>, Vec<_>) = args
            .iter()
            .map(|v| {
                let be: BasicValueEnum = self.get_llvm_value(*v).unwrap().try_into().unwrap();
                let ty: BasicMetadataTypeEnum = be.get_type().into();
                let bme: BasicMetadataValueEnum = be.into();
                (bme, ty)
            })
            .unzip();
        if let Some(pos) = pos {
            if builder
                .get_insert_block()
                .unwrap()
                .get_parent()
                .unwrap()
                .get_subprogram()
                .is_some()
            {
                self.build_dbg_location(pos)
            }
        }

        let fntp = f_tp.unwrap_or(match self.get_basic_type_op(ret_type, ctx) {
            Some(r) => {
                if matches!(
                    &*get_type_deep(Arc::new(RefCell::new(ret_type.clone()))).borrow(),
                    PLType::Pointer(_) | PLType::Primitive(_)
                ) {
                    r.fn_type(&tys, false)
                } else {
                    self.gc_ptr_ty().fn_type(&tys, false)
                }
            }
            None => self.context.void_type().fn_type(&tys, false),
        });
        let cc;
        if let Some(ff) = fv {
            cc = ff.get_call_conventions();
            let name = ff.get_name().to_str().unwrap();
            if STUCK_FNS.contains(&name) {
                // it is a stuck function, we need to add stuck fn before it
                let f = self.get_gc_mod_f(ctx, "DioGC__stuck_begin");
                let sp = self.get_sp();
                self.builder.build_call(f, &[sp.into()], "").unwrap();
            }
        } else {
            cc = CALL_CONV;
        }
        let c = builder
            .build_indirect_call(fntp, f, &args, "calltmp")
            .unwrap();
        c.set_call_convention(cc);
        let v = c.try_as_basic_value();
        if let Some(ff) = fv {
            let name = ff.get_name().to_str().unwrap();
            if STUCK_FNS.contains(&name) {
                // unstuck
                let f = self.get_gc_mod_f(ctx, "DioGC__stuck_end");
                self.builder.build_call(f, &[], "").unwrap();
            }
        }
        if v.right().is_some() {
            return None;
        }
        let ret = v.left().unwrap();

        builder.unset_current_debug_location();
        if matches!(
            &*get_type_deep(Arc::new(RefCell::new(ret_type.clone()))).borrow(),
            PLType::Pointer(_)
        ) {
            let alloca = self.alloc("call_ret", ret_type, ctx, None);
            builder
                .build_store(
                    self.get_llvm_value(alloca).unwrap().into_pointer_value(),
                    ret,
                )
                .unwrap();
            Some(alloca)
        } else {
            Some(self.get_llvm_value_handle(&ret.as_any_value_enum()))
        }
        // return Some(self.get_llvm_value_handle(&ret.as_any_value_enum()));
        // if alloca == 0 {
        //     return Some(self.get_llvm_value_handle(&ret.as_any_value_enum()));
        // }
        // self.builder
        //     .build_store(
        //         self.get_llvm_value(alloca).unwrap().into_pointer_value(),
        //         ret,
        //     )
        //     .unwrap();
        // Some(alloca)
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
        let fn_type = self
            .get_ret_type(&ret, ctx, true)
            .fn_type(&param_types, false);
        let fn_value = self
            .module
            .add_function(name, fn_type, Some(Linkage::External));
        fn_value.set_call_conventions(CALL_CONV);
        self.get_llvm_value_handle(&fn_value.as_any_value_enum())
    }

    /// # opaque_struct_type
    ///
    /// it creates an opaque StructType with no type definition yet defined.
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
        self.alloc_with_f(name, pltype, ctx, declare, "DioGC__malloc")
    }
    fn alloc_2(
        &self,
        name: &str,
        pltype: &PLType,
        ctx: &mut Ctx<'a>,
        declare: Option<Pos>,
    ) -> ValueHandle {
        self.alloc_with_f(name, pltype, ctx, declare, "DioGC__malloc_no_collect")
    }

    /// # build_struct_gep
    ///
    /// it builds a GEP(GetElementPtr) instructions and returns the value handle of the instruction.
    fn build_struct_gep(
        &self,
        structv: ValueHandle,
        index: u32,
        name: &str,
        tp: &PLType,
        ctx: &mut Ctx<'a>,
    ) -> Result<ValueHandle, String> {
        let structv = self.get_llvm_value(structv).unwrap();
        let struct_val_ptr = structv.into_pointer_value();
        let sttp = self.get_basic_type_op(tp, ctx).unwrap();
        let gep = self
            .builder
            .build_struct_gep(sttp, struct_val_ptr, index, name);
        if let Ok(gep) = gep {
            return Ok(self.get_llvm_value_handle(&gep.as_any_value_enum()));
        } else {
            Err(format!("{:?}\ntp: {:?}\nindex: {}", gep, tp, index))
        }
    }
    fn place_safepoint(&self, _ctx: &mut Ctx<'a>) {
        // FIXME
        // let f = self.get_gc_mod_f(ctx, "DioGC__safepoint");
        // let rsp = self.get_sp();
        // self.builder
        //     .build_call(f, &[rsp.into()], "safepoint")
        //     .unwrap();
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
        self.builder.build_store(ptr, value).unwrap();
    }
    fn build_const_in_bounds_gep(
        &self,
        ptr: ValueHandle,
        index: &[u64],
        name: &str,
        tp: &PLType,
        ctx: &mut Ctx<'a>,
    ) -> ValueHandle {
        let tp = self.get_basic_type_op(tp, ctx).unwrap();
        let ptr = self.get_llvm_value(ptr).unwrap();
        let ptr = ptr.into_pointer_value();
        let gep = unsafe {
            self.builder
                .build_in_bounds_gep(
                    tp,
                    ptr,
                    &index
                        .iter()
                        .map(|i| self.context.i64_type().const_int(*i, false))
                        .collect::<Vec<_>>(),
                    name,
                )
                .unwrap()
        };
        // same reason as build_struct_gep
        return self.get_llvm_value_handle(&gep.as_any_value_enum());
    }
    fn build_in_bounds_gep(
        &self,
        ptr: ValueHandle,
        index: &[ValueHandle],
        name: &str,
        tp: &PLType,
        ctx: &mut Ctx<'a>,
    ) -> ValueHandle {
        let ptr = self.get_llvm_value(ptr).unwrap();
        let ptr = ptr.into_pointer_value();
        let tp = self.get_basic_type_op(tp, ctx).unwrap();
        let gep = unsafe {
            self.builder
                .build_in_bounds_gep(
                    tp,
                    ptr,
                    &index
                        .iter()
                        .map(|i| self.get_llvm_value(*i).unwrap().try_into().unwrap())
                        .collect::<Vec<_>>(),
                    name,
                )
                .unwrap()
        };

        self.get_llvm_value_handle(&gep.as_any_value_enum())
    }
    fn const_string(&self, s: &str) -> ValueHandle {
        let s = self
            .builder
            .build_global_string_ptr(
                s,
                format!(".str_{}", ID.fetch_add(1, Ordering::Relaxed)).as_str(),
            )
            .unwrap();
        self.get_llvm_value_handle(&s.as_any_value_enum())
    }

    fn global_const(&self, name: &str, pltype: &PLType, ctx: &mut Ctx<'a>) -> ValueHandle {
        let global = self.get_global_var_handle(name);
        if global.is_none() {
            let global = self.module.add_global(
                self.get_basic_type_op(pltype, ctx).unwrap(),
                Some(AddressSpace::from(1)),
                name,
            );
            global.set_linkage(Linkage::External);
            global.set_constant(true);
            return self.get_llvm_value_handle(&global.as_any_value_enum());
        }
        global.unwrap()
    }
    fn build_dbg_location(&self, pos: Pos) {
        assert_ne!(pos.line, 0, "debug location's line number should not be 0");
        if self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap()
            .get_subprogram()
            .is_none()
        {
            return;
        }
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
            self.get_cur_di_file(),
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
            self.builder.get_insert_block().unwrap(),
        );
        // dbg.map(|d| self.builder.set_current_debug_location(d));
    }

    /// # build_phi
    ///
    /// it emits the phi node of LLVM and sets up the phi node.
    /// it returns the value handle of emitted phi node
    ///
    /// the pltype specifies the type of variable charged by the phi node.
    /// for example, if a F64 is manupulated in different branches,
    ///
    /// the pltype will be F64.
    /// the vbs holds a list of tuple which maps the block and value.
    fn build_phi(
        &self,
        pltype: &PLType,
        ctx: &mut Ctx<'a>,
        vbs: &[(ValueHandle, BlockHandle)],
    ) -> ValueHandle {
        let phi = self
            .builder
            .build_phi(self.get_basic_type_op(pltype, ctx).unwrap(), "")
            .unwrap();
        for (value, block) in vbs {
            let value = self.get_llvm_value_raw(*value).unwrap().into_int_value();
            let block = self.get_llvm_block(*block).unwrap();
            phi.add_incoming(&[(&value, block)]);
        }
        self.get_llvm_value_handle(&phi.as_any_value_enum())
    }

    /// # build_unconditional_branch
    ///
    /// builds a unconditional branch by the value refered by the block handle to
    /// terminates the current block of the builder, which adds a new block in the
    /// builder as well.
    fn build_unconditional_branch(&self, bb: BlockHandle) {
        let bb = self.get_llvm_block(bb).unwrap();
        self.builder.build_unconditional_branch(bb).unwrap();
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
        // if let Err(s) = self.module.print_to_file(file) {
        //     return Err(s.to_string());
        // }
        self.optimize();
        // self.module.strip_debug_info();
        if let Err(s) = self.module.print_to_file(file) {
            return Err(s.to_string());
        }
        Ok(())
    }
    fn optimize(&self) {
        self.module
            .set_triple(&get_target_machine(self.optlevel).get_triple());
        if *self.optimized.borrow() {
            return;
        }
        if !self.debug {
            self.module.strip_debug_info();
        }
        self.module.verify().unwrap_or_else(|e| {
            self.module.print_to_file(Path::new("err.ll")).unwrap();
            panic!(
                "module {} is not valid, err msg: {}",
                self.module.get_name().to_str().unwrap(),
                e
            )
        });
        let used = self.used.borrow();
        let used_arr = self.context.ptr_type(AddressSpace::from(0)).const_array(
            &used
                .iter()
                .map(|v| v.as_global_value().as_pointer_value())
                .collect::<Vec<_>>(),
        );
        if !used.is_empty() {
            // see https://llvm.org/docs/LangRef.html#the-llvm-used-global-variable
            let used_global = self.module.add_global(
                used_arr.get_type(),
                Some(AddressSpace::from(1)),
                "llvm.used",
            );
            used_global.set_linkage(Linkage::Appending);
            used_global.set_initializer(&used_arr);
        }
        if self.debug {
            // disable gc evacuation
            self.module
                .get_function("main")
                .and_then(FunctionValue::get_first_basic_block)
                .map(|bb| {
                    self.builder
                        .position_before(&bb.get_first_instruction().unwrap());
                    let f = self.module.add_function(
                        "DioGC__set_eva",
                        self.context
                            .void_type()
                            .fn_type(&[self.context.i32_type().into()], false),
                        None,
                    );
                    self.builder
                        .build_call(f, &[self.context.i32_type().const_int(0, false).into()], "")
                        .unwrap();
                });
        }

        unsafe {
            immix::run_module_pass(
                self.module.as_mut_ptr() as _,
                self.optlevel as i32,
                self.debug as i32,
                self.print_escaped as i32,
            );
        }
        *self.optimized.borrow_mut() = true;
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
        let v = self.builder.build_int_z_extend(v, ty, name).unwrap();
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_or(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_int_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_int_value();
        let v = self.builder.build_or(lhs, rhs, name).unwrap();
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn get_sp_handle(&self) -> ValueHandle {
        self.get_llvm_value_handle(&self.get_sp().as_any_value_enum())
    }
    fn build_and(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_int_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_int_value();
        let v = self.builder.build_and(lhs, rhs, name).unwrap();
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

        let v = self
            .builder
            .build_float_compare(op.into(), lhs, rhs, name)
            .unwrap();
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
        let v = self
            .builder
            .build_int_compare(op.into(), lhs, rhs, name)
            .unwrap();
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_int_neg(&self, v: ValueHandle, name: &str) -> ValueHandle {
        let v = self.get_llvm_value(v).unwrap().into_int_value();
        let v = self.builder.build_int_neg(v, name).unwrap();
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_int_add(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_int_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_int_value();
        let v = self.builder.build_int_add(lhs, rhs, name).unwrap();
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_int_sub(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_int_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_int_value();
        let v = self.builder.build_int_sub(lhs, rhs, name).unwrap();
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_int_mul(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_int_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_int_value();
        let v = self.builder.build_int_mul(lhs, rhs, name).unwrap();
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_int_signed_div(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_int_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_int_value();
        let v = self.builder.build_int_signed_div(lhs, rhs, name).unwrap();
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }

    fn build_int_unsigned_div(
        &self,
        lhs: ValueHandle,
        rhs: ValueHandle,
        name: &str,
    ) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_int_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_int_value();
        let v = self.builder.build_int_unsigned_div(lhs, rhs, name).unwrap();
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }

    fn build_int_signed_srem(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_int_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_int_value();
        let v = self.builder.build_int_signed_rem(lhs, rhs, name).unwrap();
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }

    fn build_int_unsigned_srem(
        &self,
        lhs: ValueHandle,
        rhs: ValueHandle,
        name: &str,
    ) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_int_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_int_value();
        let v = self.builder.build_int_unsigned_rem(lhs, rhs, name).unwrap();
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }

    fn build_float_neg(&self, v: ValueHandle, name: &str) -> ValueHandle {
        let v = self.get_llvm_value(v).unwrap().into_float_value();
        let v = self.builder.build_float_neg(v, name).unwrap();
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_float_add(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_float_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_float_value();
        let v = self.builder.build_float_add(lhs, rhs, name).unwrap();
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_float_sub(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_float_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_float_value();
        let v = self.builder.build_float_sub(lhs, rhs, name).unwrap();
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_float_mul(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_float_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_float_value();
        let v = self.builder.build_float_mul(lhs, rhs, name).unwrap();
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }
    fn build_float_div(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap().into_float_value();
        let rhs = self.get_llvm_value(rhs).unwrap().into_float_value();
        let v = self.builder.build_float_div(lhs, rhs, name).unwrap();
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
        let v = self.builder.build_int_truncate(v, dest_ty, name).unwrap();
        self.get_llvm_value_handle(&v.as_any_value_enum())
    }

    /// # build_conditional_branch
    ///
    /// it helps to build a conditional branch by the condition, then bacis block and else block
    /// refered by the value handle and block handles.
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
            .build_conditional_branch(cond, then_bb, else_bb)
            .unwrap();
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
            self.build_dbg_location(pos)
        }
    }
    fn set_di_file(&self, f: &str) {
        let f = PathBuf::from(f);

        let f = self.dibuilder.create_file(
            f.file_name().unwrap().to_str().unwrap(),
            f.parent().unwrap().to_str().unwrap(),
        );
        self.difile.set(f);
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

        let f = self.get_cur_di_file();
        // debug info
        let subroutine_type = self.dibuilder.create_subroutine_type(
            f,
            self.get_ditype(
                &ret.get_type(child, &self.clone().into(), true)?.borrow(),
                child,
            ),
            &param_ditypes,
            DIFlags::PUBLIC,
        );
        let subprogram = self.dibuilder.create_function(
            f.as_debug_info_scope(),
            &fntype.append_name_with_generic(fntype.name),
            None,
            f,
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

    /// # build_sub_program_by_pltp
    ///
    /// it creates a subprogram for a closure, which helps to debug the closure
    fn build_sub_program_by_pltp(
        &self,
        param_tps: &[Arc<RefCell<PLType>>],
        ret_tp: Arc<RefCell<PLType>>,
        name: &str,
        start_line: u32,
        closure_fn_value: ValueHandle,
        child: &mut Ctx<'a>,
    ) {
        // debug information about types
        let mut param_di_types = vec![];
        for pltype in param_tps.iter() {
            param_di_types.push(self.get_ditype(&pltype.borrow(), child).unwrap());
        }
        // debug info
        let subroutine_type = self.dibuilder.create_subroutine_type(
            self.get_cur_di_file(),
            self.get_ditype(&ret_tp.borrow(), child),
            &param_di_types,
            DIFlags::PUBLIC,
        );
        let subprogram = self.dibuilder.create_function(
            self.get_cur_di_file().as_debug_info_scope(),
            &format!("{}__fn", name),
            None,
            self.get_cur_di_file(),
            start_line,
            subroutine_type,
            false,
            true,
            start_line,
            DIFlags::PUBLIC,
            false,
        );
        let funcvalue = self
            .get_llvm_value(closure_fn_value)
            .unwrap()
            .into_function_value();
        funcvalue.set_subprogram(subprogram);
        self.discope.set(subprogram.as_debug_info_scope());
    }
    fn build_return(&self, v: Option<ValueHandle>) {
        if let Some(v) = v {
            let v = self.get_llvm_value(v).unwrap();
            let v: BasicValueEnum = v.try_into().unwrap();
            self.builder.build_return(Some(&v)).unwrap();
        } else {
            self.builder.build_return(None).unwrap();
        }
    }

    /// # create_closure_variable_dbg
    ///
    /// it creates the debug information for the closure variables
    #[allow(clippy::too_many_arguments)]
    fn create_closure_variable_dbg(
        &self,
        pltp: &PLType,
        pos: Pos,
        i: usize,
        child: &mut Ctx<'a>,
        value_handle: ValueHandle,
        allocab: BlockHandle,
        name: &str,
    ) {
        let di_var = self.dibuilder.create_parameter_variable(
            self.discope.get(),
            name,
            i as u32,
            self.get_cur_di_file(),
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
        let alloca = self.builder.build_alloca(raw_tp, "para").unwrap();
        self.builder.build_store(alloca, v).unwrap();
        self.dibuilder.insert_declare_at_end(
            alloca,
            Some(di_var),
            None,
            self.builder.get_current_debug_location().unwrap(),
            allocab,
        );
    }

    fn create_params_roots(
        &self,
        _f: ValueHandle,
        _allocab: ValueHandle,
        _params: &[Arc<RefCell<PLType>>],
    ) {
        // let cubb = self.get_cur_basic_block();
        // self.position_at_end_block(allocab);
        // let funcvalue = self.get_llvm_value(f).unwrap().into_function_value();
        // for (i, nv) in funcvalue.get_param_iter().enumerate() {
        //     let alloca_stack = self
        //         .builder
        //         .build_alloca(nv.get_type(), "param_alloca")
        //         .unwrap();
        //     self.builder.build_store(alloca_stack, nv).unwrap();
        //     let t = if nv.get_type().is_pointer_type() {
        //         ObjectType::Pointer
        //     } else {
        //         params[i].borrow().get_immix_type()
        //     };

        //     let nv_handle = self.get_llvm_value_handle(&nv.as_any_value_enum());
        // }
        // self.position_at_end_block(cubb);
    }
    #[allow(clippy::too_many_arguments)]
    fn create_parameter_variable_dbg(
        &self,
        fnvalue: &FNValue,
        pos: Pos,
        i: usize,
        child: &mut Ctx<'a>,
    ) {
        let divar = self.dibuilder.create_parameter_variable(
            self.discope.get(),
            &fnvalue.param_names[i],
            i as u32 + 1,
            self.get_cur_di_file(),
            pos.line as u32,
            self.get_ditype(
                &fnvalue.fntype.param_pltypes[i]
                    .get_type(child, &self.clone().into(), true)
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
            self.builder
                .get_insert_block()
                .unwrap()
                .get_parent()
                .unwrap()
                .get_nth_param(i as u32)
                .unwrap()
                .into_pointer_value(),
            Some(divar),
            None,
            self.builder.get_current_debug_location().unwrap(),
            self.builder.get_insert_block().unwrap(),
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
            i as u32 + 1,
            self.get_cur_di_file(),
            pos.line as u32,
            self.get_ditype(
                &fnvalue.fntype.param_pltypes[i]
                    .get_type(child, &self.clone().into(), true)
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
            self.builder.get_insert_block().unwrap(),
        );

        if child.ctx_flag == CtxFlag::InGeneratorYield {
            let data = child.generator_data.as_ref().unwrap().clone();
            let bb_v = data.borrow().entry_bb;
            let bb = self.get_llvm_block(bb_v).unwrap();
            let funcvalue = bb.get_parent().unwrap();
            let origin_bb = self.get_cur_basic_block();
            self.position_at_end_block(bb_v);
            child.ctx_flag = CtxFlag::Normal;
            let ptr = self.alloc("param_ptr", tp, child, None);
            child.ctx_flag = CtxFlag::InGeneratorYield;
            let ctx_v = data.borrow().ctx_handle;
            let para_ptr = self
                .build_struct_gep(
                    ctx_v,
                    (i + 2) as u32,
                    "para",
                    &data.borrow().ctx_tp.as_ref().unwrap().borrow(),
                    child,
                )
                .unwrap();

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

            return;
        }
        let funcvalue = self
            .get_llvm_value(value_handle)
            .unwrap()
            .into_function_value();

        let cubb = self.get_cur_basic_block();
        self.position_at_end_block(allocab);
        let nv = funcvalue
            .get_nth_param(i as _)
            .unwrap()
            .as_basic_value_enum();

        let nv_handle = self.get_llvm_value_handle(&nv.as_any_value_enum());

        self.position_at_end_block(cubb);
        self.build_store(alloca, nv_handle);
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
        let global = self
            .module
            .add_global(base_type, Some(AddressSpace::from(1)), name);
        let ditype = self.get_ditype(pltp, ctx);
        let exp = self.dibuilder.create_global_variable_expression(
            self.diunit.as_debug_info_scope(),
            name,
            "",
            self.get_cur_di_file(),
            line,
            ditype.unwrap(),
            false,
            None,
            None,
            ditype.unwrap().get_align_in_bits(),
        );
        global.set_initializer(&base_type.const_zero());
        global.set_metadata(exp.as_metadata_value(self.context), 0);
        let gctp = pltp.get_immix_type();
        let f = self.get_gc_mod_f(ctx, "DioGC__register_global");
        let ptrtoint = self
            .builder
            .build_ptr_to_int(global.as_pointer_value(), self.context.i64_type(), "")
            .unwrap();
        let gc_tp_int = self
            .get_llvm_value(self.int_value(&PriType::U8, gctp as u64, false))
            .unwrap()
            .into_int_value();

        self.builder
            .build_call(f, &[ptrtoint.into(), gc_tp_int.into()], "register_global")
            .unwrap();
        self.get_llvm_value_handle(&global.as_any_value_enum())
    }

    /// # gen_st_visit_function
    ///
    /// it generates the visit function for each structure field for GC use.
    fn gen_st_visit_function(
        &self,
        ctx: &mut Ctx<'a>,
        v: &STType,
        field_tps: &[Arc<RefCell<PLType>>],
    ) {
        let currentbb = self.builder.get_insert_block();
        self.builder.unset_current_debug_location();
        let ty = self.struct_type(v, ctx);
        let ftp = self.visit_fn_tp();
        let name = format!("{}_visitorf@", v.get_full_name());

        let linkage = Linkage::Internal;

        let f = match self.module.get_function(&name) {
            Some(f) => f,
            None => self.module.add_function(&name, ftp, Some(linkage)),
        };
        self.used.borrow_mut().push(f);

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
            let f = self.builder.build_struct_gep(ty, st, i, "gep").unwrap();
            // 指针类型，递归调用visit函数
            match field_pltp {
                PLType::Pointer(_) => {
                    let ptr = f;
                    let casted = ptr;
                    self.builder
                        .build_indirect_call(
                            self.context.void_type().fn_type(
                                &[visitor.get_type().into(), casted.get_type().into()],
                                false,
                            ),
                            visit_ptr_f,
                            &[visitor.into(), casted.into()],
                            "call",
                        )
                        .unwrap();
                }
                PLType::Struct(STType { atomic: false, .. }) | PLType::Arr(_) => {
                    let ptr = f;
                    let casted = ptr;
                    self.builder
                        .build_indirect_call(
                            self.context.void_type().fn_type(
                                &[visitor.get_type().into(), casted.get_type().into()],
                                false,
                            ),
                            visit_complex_f,
                            &[visitor.into(), casted.into()],
                            "call",
                        )
                        .unwrap();
                }
                PLType::Trait(_) | PLType::Union(_) | PLType::Closure(_) => {
                    let ptr = f;
                    let casted = ptr;
                    self.builder
                        .build_indirect_call(
                            self.context.void_type().fn_type(
                                &[visitor.get_type().into(), casted.get_type().into()],
                                false,
                            ),
                            visit_trait_f,
                            &[visitor.into(), casted.into()],
                            "call",
                        )
                        .unwrap();
                }
                PLType::Fn(_)
                | PLType::Primitive(_)
                | PLType::Void
                | PLType::Generic(_)
                | PLType::PlaceHolder(_)
                | PLType::PartialInferred(_)
                | PLType::Struct(_)
                | PLType::Unknown => (),
            }
            // 其他为原子类型，跳过
        }
        self.builder.build_return(None).unwrap();
        if let Some(bb) = currentbb {
            self.builder.position_at_end(bb);
        } else {
            self.builder.clear_insertion_position();
        }
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
                .build_int_cast_sign_flag(val, target, signed, "cast")
                .unwrap();
            self.get_llvm_value_handle(&val.into())
        } else if tp.is_float_type() && target.is_float_type() {
            let val = val.into_float_value();
            let target = target.into_float_type();
            let val = self.builder.build_float_cast(val, target, "cast").unwrap();
            self.get_llvm_value_handle(&val.into())
        } else if tp.is_int_type() && target.is_float_type() {
            let val = val.into_int_value();
            let target = target.into_float_type();
            if signed {
                let val = self
                    .builder
                    .build_signed_int_to_float(val, target, "cast")
                    .unwrap();
                self.get_llvm_value_handle(&val.into())
            } else {
                let val = self
                    .builder
                    .build_unsigned_int_to_float(val, target, "cast")
                    .unwrap();
                self.get_llvm_value_handle(&val.into())
            }
        } else if tp.is_float_type() && target.is_int_type() {
            let val = val.into_float_value();
            let target = target.into_int_type();
            if signed {
                let val = self
                    .builder
                    .build_float_to_signed_int(val, target, "cast")
                    .unwrap();
                self.get_llvm_value_handle(&val.into())
            } else {
                let val = self
                    .builder
                    .build_float_to_unsigned_int(val, target, "cast")
                    .unwrap();
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
        self.get_llvm_value_handle(&self.gc_ptr_ty().const_null().into())
    }

    fn create_closure_parameter_variable(
        &self,
        i: u32,
        f: ValueHandle,
        alloca: ValueHandle,
        allocab: BlockHandle,
        tp: &PLType,
    ) {
        let funcvalue = self.get_llvm_value(f).unwrap().into_function_value();

        let cubb = self.get_cur_basic_block();
        self.position_at_end_block(allocab);
        let nv = funcvalue
            .get_nth_param(i as _)
            .unwrap()
            .as_basic_value_enum();
        let alloca_stack = self
            .builder
            .build_alloca(nv.get_type(), "param_alloca")
            .unwrap();
        self.builder.build_store(alloca_stack, nv).unwrap();
        let _t = if nv.get_type().is_pointer_type() {
            ObjectType::Pointer
        } else {
            tp.get_immix_type()
        };
        let _nv_handle = self.get_llvm_value_handle(&nv.as_any_value_enum());

        self.position_at_end_block(cubb);
        self.build_store(
            alloca,
            self.get_llvm_value_handle(&funcvalue.get_nth_param(i).unwrap().as_any_value_enum()),
        );
    }

    /// # create_closure_fn
    ///
    /// create_closure_fn creates a function type in LLVM format and then emit it by the builder.
    /// The first parameter of the function is an i8 pointer points to the captured data.
    fn create_closure_fn(
        &self,
        ctx: &mut Ctx<'a>,
        closure_name: &str,
        params: &[Arc<RefCell<PLType>>],
        ret: &PLType,
    ) -> ValueHandle {
        let i8ptr = self.gc_ptr_ty();
        let mut closure_param_tps: Vec<BasicMetadataTypeEnum> =
            vec![i8ptr.as_basic_type_enum().into()];
        closure_param_tps.extend(
            &params
                .iter()
                .map(|p| self.get_param_type(&p.borrow(), ctx, true).into())
                .collect::<Vec<_>>(),
        );
        let f_tp = self
            .get_ret_type(ret, ctx, true)
            .fn_type(&closure_param_tps, false);
        let f_v = self
            .module
            .add_function(&format!("{}__fn", closure_name), f_tp, None);

        f_v.set_call_conventions(CALL_CONV);
        self.get_llvm_value_handle(&f_v.into())
    }

    /// # get_closure_trampoline
    ///
    /// 为指定函数创建一个用于构建闭包的跳板函数
    ///
    /// 闭包函数相比原函数多出一个参数（第一个），用于存放闭包的环境。在函数为纯函数的情况，
    /// 该值不会被使用，因此可以直接传入null。
    fn get_closure_trampoline(&self, f: ValueHandle) -> ValueHandle {
        if !self.get_llvm_value(f).unwrap().is_function_value() {
            // already a closure
            return f;
        }
        let ori_f = self.get_llvm_value(f).unwrap().into_function_value();
        let name = ori_f.get_name();
        let trampoline_name = format!("{}__trampoline", name.to_str().unwrap());
        let closure_f = self.module.get_function(&trampoline_name);
        if let Some(closure_f) = closure_f {
            return self.get_llvm_value_handle(&closure_f.into());
        }
        let f_tp = ori_f.get_type();
        let i8ptr = self.gc_ptr_ty();
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
        f.set_call_conventions(CALL_CONV);
        let bb = self.context.append_basic_block(f, "entry");
        let old_bb = self.builder.get_insert_block();
        self.builder.position_at_end(bb);
        let args = f.get_params();
        let re = self
            .builder
            .build_call(
                ori_f,
                &args
                    .iter()
                    .skip(1)
                    .map(|a| a.to_owned().into())
                    .collect::<Vec<_>>(),
                "re",
            )
            .unwrap();
        re.set_call_convention(ori_f.get_call_conventions());
        if let Some(ret) = re.try_as_basic_value().left() {
            self.builder.build_return(Some(&ret)).unwrap();
        } else {
            self.builder.build_return(None).unwrap();
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
    fn add_closure_st_field(&self, st: &STType, field: ValueHandle, ctx: &mut Ctx<'a>) {
        let st_tp = self.struct_type(st, ctx);
        let field_tp = self
            .handle_table
            .borrow()
            .get(&field)
            .copied()
            .unwrap()
            .get_type();
        add_field(st_tp, field_tp);
    }

    fn add_generator_yield_fn(
        &self,
        ctx: &mut Ctx<'a>,
        ctx_name: &str,
        ret_tp: &PLType,
    ) -> ValueHandle {
        let tp = self.context.ptr_type(AddressSpace::from(1)).into();
        let ftp = if ctx
            .generator_data
            .as_ref()
            .unwrap()
            .borrow()
            .generator_type
            .is_async()
        {
            // async poll fn's second argument is a closure
            let ty = PLType::Closure(ClosureType {
                arg_types: vec![],
                ret_type: Arc::new(RefCell::new(PLType::Void)),
                range: Default::default(),
            });
            let tp2 = self.get_param_type(&ty, ctx, true).into();
            self.get_ret_type(ret_tp, ctx, true)
                .fn_type(&[tp, tp2], false)
        } else {
            self.get_ret_type(ret_tp, ctx, true).fn_type(&[tp], false)
        };
        let f = self
            .module
            .add_function(&format!("{}__yield", ctx_name), ftp, None);
        f.set_call_conventions(CALL_CONV);
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
    fn build_indirect_br(&self, block: ValueHandle, possible_blocks: &[BlockHandle]) {
        let block = self.get_llvm_value(block).unwrap();
        self.builder
            .build_indirect_branch::<BasicValueEnum>(
                block.try_into().unwrap(),
                &possible_blocks
                    .iter()
                    .map(|b| self.get_llvm_block(*b).unwrap())
                    .collect::<Vec<_>>(),
            )
            .unwrap();
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
        let _ptr_tp = v_ptr.get_type().into_pointer_type();
        let _value_tp = v.get_type();
        self.build_store(ptr, value);
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
        self.builder.build_store(v, size).unwrap();

        self.builder.position_at_end(cur_bb);
    }
    fn build_memcpy(
        &self,
        from: ValueHandle,
        from_tp: &PLType,
        to: ValueHandle,
        len: ValueHandle,
        ctx: &mut Ctx<'a>,
    ) {
        let from = self.get_llvm_value(from).unwrap().into_pointer_value();
        let to = self.get_llvm_value(to).unwrap().into_pointer_value();
        let td = self.targetmachine.get_target_data();
        let unit_size = td.get_store_size(&self.get_basic_type_op(from_tp, ctx).unwrap());
        let i64_size = self.context.i64_type().const_int(unit_size, true);
        let len = self
            .get_llvm_value(
                self.try_load2var_inner_raw(len, self.context.i64_type().as_basic_type_enum())
                    .unwrap(),
            )
            .unwrap()
            .into_int_value();
        let arg_len = self
            .builder
            .build_int_mul(len, i64_size, "arg_len")
            .unwrap();
        self.builder.build_memcpy(to, 8, from, 8, arg_len).unwrap();
    }
    fn build_bit_not(&self, v: ValueHandle) -> ValueHandle {
        let v = self.get_llvm_value(v).unwrap();
        let v = v.into_int_value();
        let v = self
            .builder
            .build_xor(v, v.get_type().const_all_ones(), "not")
            .unwrap();
        self.get_llvm_value_handle(&v.into())
    }
    fn build_bit_and(&self, lhs: ValueHandle, rhs: ValueHandle) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap();
        let rhs = self.get_llvm_value(rhs).unwrap();
        let lhs = lhs.into_int_value();
        let rhs = rhs.into_int_value();
        let v = self.builder.build_and(lhs, rhs, "and").unwrap();
        self.get_llvm_value_handle(&v.into())
    }
    fn build_bit_or(&self, lhs: ValueHandle, rhs: ValueHandle) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap();
        let rhs = self.get_llvm_value(rhs).unwrap();
        let lhs = lhs.into_int_value();
        let rhs = rhs.into_int_value();
        let v = self.builder.build_or(lhs, rhs, "or").unwrap();
        self.get_llvm_value_handle(&v.into())
    }
    fn build_bit_xor(&self, lhs: ValueHandle, rhs: ValueHandle) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap();
        let rhs = self.get_llvm_value(rhs).unwrap();
        let lhs = lhs.into_int_value();
        let rhs = rhs.into_int_value();
        let v = self.builder.build_xor(lhs, rhs, "xor").unwrap();
        self.get_llvm_value_handle(&v.into())
    }
    fn build_bit_left_shift(&self, lhs: ValueHandle, rhs: ValueHandle) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap();
        let rhs = self.get_llvm_value(rhs).unwrap();
        let lhs = lhs.into_int_value();
        let rhs = rhs.into_int_value();
        let v = self
            .builder
            .build_left_shift(lhs, rhs, "left_shift")
            .unwrap();
        self.get_llvm_value_handle(&v.into())
    }
    fn build_bit_right_shift(&self, lhs: ValueHandle, rhs: ValueHandle) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap();
        let rhs = self.get_llvm_value(rhs).unwrap();
        let lhs = lhs.into_int_value();
        let rhs = rhs.into_int_value();
        let v = self
            .builder
            .build_right_shift(lhs, rhs, false, "right_shift")
            .unwrap();
        self.get_llvm_value_handle(&v.into())
    }
    fn build_bit_right_shift_arithmetic(&self, lhs: ValueHandle, rhs: ValueHandle) -> ValueHandle {
        let lhs = self.get_llvm_value(lhs).unwrap();
        let rhs = self.get_llvm_value(rhs).unwrap();
        let lhs = lhs.into_int_value();
        let rhs = rhs.into_int_value();
        let v = self
            .builder
            .build_right_shift(lhs, rhs, true, "right_shift")
            .unwrap();
        self.get_llvm_value_handle(&v.into())
    }

    fn await_task(&self, ctx: &mut Ctx<'a>, task: ValueHandle) -> ValueHandle {
        let data = ctx.generator_data.as_ref().unwrap().clone();

        // generator ctx需要记录我们的task，以让他存活过这次yield

        let task_ptr = self
            .build_struct_gep(
                data.borrow().yield_ctx_handle,
                2,
                "task_ctx_slot",
                &data.borrow().ctx_tp.as_ref().unwrap().borrow(),
                ctx,
            )
            .unwrap();
        if !self.is_ptr(task) {
            let t = self.alloc_raw("taskptr", &PLType::new_i8_ptr(), ctx, None, "DioGC__malloc");
            self.build_store(t, task);
            self.build_store(task_ptr, t);
        } else {
            self.build_store(task_ptr, task);
        }

        // append cond block
        let current_bb = self.builder.get_insert_block().unwrap();
        let current_fn = current_bb.get_parent().unwrap();
        let cond_bb = self.context.append_basic_block(current_fn, "await_cond");
        self.builder.build_unconditional_branch(cond_bb).unwrap();
        self.builder.position_at_end(cond_bb);

        // load from generator ctx
        let task_ptr = self
            .get_llvm_value(
                self.build_struct_gep(
                    data.borrow().yield_ctx_handle,
                    2,
                    "task_ctx_slot",
                    &data.borrow().ctx_tp.as_ref().unwrap().borrow(),
                    ctx,
                )
                .unwrap(),
            )
            .unwrap()
            .into_pointer_value();
        let real_task = self
            .builder
            .build_load(self.gc_ptr_ty(), task_ptr, "task")
            .unwrap()
            .into_pointer_value();

        // its type can be seen as 3 pointers
        let task_type = self.context.struct_type(
            &[
                self.context.ptr_type(AddressSpace::from(1)).into(),
                self.context.ptr_type(AddressSpace::from(1)).into(),
                self.context.ptr_type(AddressSpace::from(1)).into(),
            ],
            false,
        );
        // gep third field
        let poll_ptr = self
            .builder
            .build_struct_gep(task_type, real_task, 2, "taskPtr")
            .unwrap();
        // gep second field
        let self_ptr = self
            .builder
            .build_struct_gep(task_type, real_task, 1, "selfPtr")
            .unwrap();
        // get second arg for current function
        let second_arg = current_fn.get_nth_param(1).unwrap().into_pointer_value();
        // invoke poll function, with arguments
        let poll_fn = self
            .builder
            .build_load(
                self.context.ptr_type(AddressSpace::from(1)),
                poll_ptr,
                "poll_f",
            )
            .unwrap();
        let self_p = self
            .builder
            .build_load(
                self.context.ptr_type(AddressSpace::from(1)),
                self_ptr,
                "self",
            )
            .unwrap();

        let polled = self.poll_task(poll_fn, self_p, second_arg);

        // check if polled is none
        let tag = self
            .builder
            .build_load(self.context.i64_type(), polled, "tag")
            .unwrap()
            .into_int_value();
        let is_none = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                tag,
                self.context.i64_type().const_int(1, false),
                "is_none",
            )
            .unwrap();
        // if none, into loop logic
        let loop_bb = self.context.append_basic_block(current_fn, "await_loop");
        let end_bb = self.context.append_basic_block(current_fn, "await_end");
        self.builder
            .build_conditional_branch(is_none, loop_bb, end_bb)
            .unwrap();
        self.builder.position_at_end(loop_bb);
        // return None
        let ret_handle = ctx.return_block.unwrap().1.unwrap();
        let ret_v = self
            .get_llvm_value(ret_handle)
            .unwrap()
            .into_pointer_value();
        // store 1 to its start (means tag None)
        self.builder
            .build_store(ret_v, self.context.i64_type().const_int(1, false))
            .unwrap();
        let curbb = self.get_cur_basic_block();

        ctx.generator_data
            .as_ref()
            .unwrap()
            .borrow_mut()
            .prev_yield_bb = Some(curbb);
        // resume to cond block, so when enter again it will poll again
        ctx.add_term_to_previous_yield_and_ret(
            &self.clone().into(),
            self.get_llvm_block_handle(cond_bb),
        );

        // emit end block
        self.builder.position_at_end(end_bb);

        // option is a struct contains two pointers
        let opt_st = self.context.struct_type(
            &[
                self.context.ptr_type(AddressSpace::from(1)).into(),
                self.context.ptr_type(AddressSpace::from(1)).into(),
            ],
            false,
        );

        let value = self
            .builder
            .build_struct_gep(opt_st, polled, 1, "polled_value")
            .unwrap();
        let value = self
            .builder
            .build_load(
                self.context.ptr_type(AddressSpace::from(1)),
                value,
                "polled_value",
            )
            .unwrap()
            .into_pointer_value();
        // return
        self.get_llvm_value_handle(&value.into())
    }
    fn await_ret(&self, ctx: &mut Ctx<'a>, ret: ValueHandle) {
        let ret = if !self.is_ptr(ret) {
            let t = self.alloc_raw("taskptr", &PLType::new_i8_ptr(), ctx, None, "DioGC__malloc");
            self.build_store(t, ret);
            t
        } else {
            ret
        };
        let data = ctx.generator_data.as_ref().unwrap().clone();

        // generator ctx需要记录我们的ret，以让他存活过这次yield

        let task_ptr = self
            .build_struct_gep(
                data.borrow().yield_ctx_handle,
                3,
                "ret_ctx_slot",
                &data.borrow().ctx_tp.as_ref().unwrap().borrow(),
                ctx,
            )
            .unwrap();
        self.build_store(task_ptr, ret);

        let end_bb = self.context.append_basic_block(
            self.builder
                .get_insert_block()
                .unwrap()
                .get_parent()
                .unwrap(),
            "await_end_ret",
        );

        let curbb = self.get_cur_basic_block();

        ctx.generator_data
            .as_ref()
            .unwrap()
            .borrow_mut()
            .prev_yield_bb = Some(curbb);
        // resume to end block, so when enter again it will poll again
        ctx.add_term_to_previous_yield(&self.clone().into(), self.get_llvm_block_handle(end_bb));
        self.builder.build_unconditional_branch(end_bb).unwrap();

        // emit end block
        self.builder.position_at_end(end_bb);

        // load ret
        let ret_ptr = self
            .get_llvm_value(
                self.build_struct_gep(
                    data.borrow().yield_ctx_handle,
                    3,
                    "ret_ctx_slot",
                    &data.borrow().ctx_tp.as_ref().unwrap().borrow(),
                    ctx,
                )
                .unwrap(),
            )
            .unwrap()
            .into_pointer_value();
        let ret = self
            .builder
            .build_load(self.gc_ptr_ty(), ret_ptr, "ret")
            .unwrap()
            .into_pointer_value();
        let opt_ty = get_option_type(
            ctx,
            &self.clone().into(),
            Arc::new(RefCell::new(PLType::Primitive(PriType::BOOL))),
        )
        .unwrap();
        let mem = self
            .get_llvm_value(self.alloc_raw("ret_opt", &opt_ty.borrow(), ctx, None, "DioGC__malloc"))
            .unwrap()
            .into_pointer_value();
        // store ret to opt's second field
        let opt_ty = self.get_basic_type_op(&opt_ty.borrow(), ctx).unwrap();
        let value = self
            .builder
            .build_struct_gep(opt_ty, mem, 1, "value")
            .unwrap();
        self.builder.build_store(value, ret).unwrap();
        // store 0 to its start (means tag Some)
        self.builder
            .build_store(mem, self.context.i64_type().const_int(0, false))
            .unwrap();
        // return
        self.builder.build_return(Some(&mem)).unwrap();
    }
}

fn add_field(st_tp: StructType, field_tp: inkwell::types::AnyTypeEnum<'_>) -> u32 {
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
