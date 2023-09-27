/// 为以后codegen逻辑完全分离作准备，此包代码应该遵循以下原则：
/// 1. 所有Builder的字段都应该private，不应该被外部直接访问
/// 2. 所有涉及llvm类型的函数（包括参数或返回值）都应该是private的
pub mod llvmbuilder;
pub mod no_op_builder;
use std::{cell::RefCell, path::Path, sync::Arc};

#[cfg(feature = "llvm")]
use self::llvmbuilder::LLVMBuilder;
use self::no_op_builder::NoOpBuilder;
use enum_dispatch::enum_dispatch;

use super::{
    ctx::Ctx,
    diag::PLDiag,
    node::{types::TypedIdentifierNode, TypeNodeEnum},
    pltype::{FNValue, PLType, PriType, STType},
    range::{Pos, Range},
};

#[enum_dispatch]
pub trait IRBuilder<'a, 'ctx> {
    fn bitcast(&self, ctx: &mut Ctx<'a>, from: ValueHandle, to: &PLType, name: &str)
        -> ValueHandle;
    fn pointer_cast(
        &self,
        ctx: &mut Ctx<'a>,
        from: ValueHandle,
        to: &PLType,
        name: &str,
    ) -> ValueHandle;
    fn get_global_var_handle(&self, name: &str) -> Option<ValueHandle>;
    fn new_subscope(&self, start: Pos);
    fn add_global(
        &self,
        name: &str,
        pltype: Arc<RefCell<PLType>>,
        ctx: &mut Ctx<'a>,
        line: u32,
        pltp: &PLType,
    ) -> ValueHandle;
    fn alloc(
        &self,
        name: &str,
        pltype: &PLType,
        ctx: &mut Ctx<'a>,
        declare: Option<Pos>,
    ) -> ValueHandle;
    fn build_conditional_branch(
        &self,
        cond: ValueHandle,
        then_bb: BlockHandle,
        else_bb: BlockHandle,
    );
    fn build_const_in_bounds_gep(&self, ptr: ValueHandle, index: &[u64], name: &str)
        -> ValueHandle;
    fn build_dbg_location(&self, pos: Pos);
    fn build_in_bounds_gep(
        &self,
        ptr: ValueHandle,
        index: &[ValueHandle],
        name: &str,
    ) -> ValueHandle;
    fn build_return(&self, v: Option<ValueHandle>);
    fn build_store(&self, ptr: ValueHandle, value: ValueHandle);
    fn build_struct_gep(
        &self,
        structv: ValueHandle,
        index: u32,
        name: &str,
    ) -> Result<ValueHandle, ()>;
    fn build_sub_program(
        &self,
        paralist: Vec<Box<TypedIdentifierNode>>,
        ret: Box<TypeNodeEnum>,
        fntype: &FNValue,
        fnvalue: ValueHandle,
        child: &mut Ctx<'a>,
    ) -> Result<(), PLDiag>;
    fn clear_insertion_position(&self);
    fn const_string(&self, s: &str) -> ValueHandle;
    #[allow(clippy::too_many_arguments)]
    fn create_parameter_variable(
        &self,
        fntype: &FNValue,
        pos: Pos,
        i: usize,
        child: &mut Ctx<'a>,
        fnvalue: ValueHandle,
        alloca: ValueHandle,
        allocab: BlockHandle,
        tp: &PLType,
    );
    fn delete_block(&self, b: BlockHandle);
    fn finalize_debug(&self);
    fn float_value(&self, ty: &PriType, v: f64) -> ValueHandle;
    fn get_cur_basic_block(&self) -> BlockHandle;
    fn get_first_basic_block(&self, v: ValueHandle) -> BlockHandle;
    fn get_first_instruction(&self, bb: BlockHandle) -> Option<ValueHandle>;
    fn get_last_basic_block(&self, v: ValueHandle) -> BlockHandle;
    fn insert_var_declare(
        &self,
        name: &str,
        pos: Pos,
        pltype: &PLType,
        v: ValueHandle,
        ctx: &mut Ctx<'a>,
    );
    fn int_value(&self, ty: &PriType, v: u64, sign_ext: bool) -> ValueHandle;
    fn position_at(&self, v: ValueHandle);
    fn print_to_file(&self, file: &Path) -> Result<(), String>;
    fn rm_curr_debug_location(&self);
    fn try_set_fn_dbg(&self, pos: Pos, f: ValueHandle);
    fn write_bitcode_to_path(&self, path: &Path) -> bool;
    fn build_phi(
        &self,
        pltype: &PLType,
        ctx: &mut Ctx<'a>,
        vbs: &[(ValueHandle, BlockHandle)],
    ) -> ValueHandle;
    fn build_unconditional_branch(&self, bb: BlockHandle);
    fn position_at_end_block(&self, block: BlockHandle);
    fn add_body_to_struct_type(&self, name: &str, sttype: &STType, ctx: &mut Ctx<'a>);
    fn get_or_insert_fn_handle(&self, pltp: &FNValue, ctx: &mut Ctx<'a>) -> (ValueHandle, bool);
    fn get_or_add_global(
        &self,
        name: &str,
        pltype: Arc<RefCell<PLType>>,
        ctx: &mut Ctx<'a>,
        constant: bool,
    ) -> ValueHandle;
    fn build_load(&self, ptr: ValueHandle, name: &str) -> ValueHandle;
    fn try_load2var(
        &self,
        range: Range,
        v: ValueHandle,
        ctx: &mut Ctx<'a>,
    ) -> Result<ValueHandle, PLDiag>;
    fn get_function(&self, name: &str) -> Option<ValueHandle>;
    fn build_call(
        &self,
        f: ValueHandle,
        args: &[ValueHandle],
        ret_type: &PLType,
        ctx: &mut Ctx<'a>,
    ) -> Option<ValueHandle>;
    fn add_function(
        &self,
        name: &str,
        paramtps: &[PLType],
        ret: PLType,
        ctx: &mut Ctx<'a>,
    ) -> ValueHandle;
    fn opaque_struct_type(&self, name: &str);
    fn build_int_z_extend(&self, v: ValueHandle, ty: &PriType, name: &str) -> ValueHandle;
    fn build_or(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle;
    fn build_and(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle;
    fn build_float_compare(
        &self,
        op: FloatPredicate,
        lhs: ValueHandle,
        rhs: ValueHandle,
        name: &str,
    ) -> ValueHandle;
    fn build_int_compare(
        &self,
        op: IntPredicate,
        lhs: ValueHandle,
        rhs: ValueHandle,
        name: &str,
    ) -> ValueHandle;
    fn build_int_add(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle;
    fn build_int_sub(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle;
    fn build_int_mul(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle;
    fn build_int_signed_div(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle;
    fn build_int_signed_srem(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle;
    fn build_int_unsigned_div(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str)
        -> ValueHandle;
    fn build_int_unsigned_srem(
        &self,
        lhs: ValueHandle,
        rhs: ValueHandle,
        name: &str,
    ) -> ValueHandle;
    fn build_float_neg(&self, v: ValueHandle, name: &str) -> ValueHandle;
    fn build_float_add(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle;
    fn build_float_sub(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle;
    fn build_float_mul(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle;
    fn build_float_div(&self, lhs: ValueHandle, rhs: ValueHandle, name: &str) -> ValueHandle;
    fn append_basic_block(&self, func: ValueHandle, name: &str) -> BlockHandle;
    fn build_int_truncate(&self, v: ValueHandle, dest_ty: &PriType, name: &str) -> ValueHandle;
    fn build_int_neg(&self, v: ValueHandle, name: &str) -> ValueHandle;
    fn gen_st_visit_function(
        &self,
        ctx: &mut Ctx<'a>,
        v: &STType,
        field_tps: &[Arc<RefCell<PLType>>],
    );
    fn get_stack_root(&self, v: ValueHandle) -> ValueHandle;
    fn cast_primitives(&self, handle: ValueHandle, tp: &PriType, target: &PriType) -> ValueHandle;
    fn is_ptr(&self, v: ValueHandle) -> bool;
    fn get_or_insert_helper_fn_handle(&self, name: &str) -> ValueHandle;
    fn create_closure_fn(
        &self,
        ctx: &mut Ctx<'a>,
        closure_name: &str,
        params: &[Arc<RefCell<PLType>>],
        ret: &PLType,
    ) -> ValueHandle;
    fn i8ptr_null(&self) -> ValueHandle;
    fn get_closure_trampoline(&self, f: ValueHandle) -> ValueHandle;
    fn create_closure_parameter_variable(&self, i: u32, f: ValueHandle, alloca: ValueHandle);
    fn get_nth_param(&self, f: ValueHandle, i: u32) -> ValueHandle;
    fn add_closure_st_field(&self, st: ValueHandle, field: ValueHandle);
    fn build_sub_program_by_pltp(
        &self,
        paralist: &[Arc<RefCell<PLType>>],
        ret: Arc<RefCell<PLType>>,
        name: &str,
        start_line: u32,
        fnvalue: ValueHandle,
        child: &mut Ctx<'a>,
    );
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
    );
    fn add_generator_yield_fn(
        &self,
        ctx: &mut Ctx<'a>,
        ctx_name: &str,
        ret_tp: &PLType,
    ) -> ValueHandle;
    fn get_block_address(&self, block: BlockHandle) -> ValueHandle;
    fn build_indirect_br(&self, block: ValueHandle, ctx: &Ctx<'a>);
    // only used in special case, as it does not add gc root
    unsafe fn store_with_aoto_cast(&self, ptr: ValueHandle, value: ValueHandle);
    fn stack_alloc(&self, name: &str, ctx: &mut Ctx<'a>, tp: &PLType) -> ValueHandle;
    fn correct_generator_ctx_malloc_inst(&self, ctx: &mut Ctx<'a>, name: &str);
    fn sizeof(&self, pltype: &PLType, ctx: &mut Ctx<'a>) -> u64;
    fn build_memcpy(&self, from: ValueHandle, to: ValueHandle, len: ValueHandle);
    fn build_bit_not(&self, v: ValueHandle) -> ValueHandle;
    fn build_bit_and(&self, lhs: ValueHandle, rhs: ValueHandle) -> ValueHandle;
    fn build_bit_or(&self, lhs: ValueHandle, rhs: ValueHandle) -> ValueHandle;
    fn build_bit_xor(&self, lhs: ValueHandle, rhs: ValueHandle) -> ValueHandle;
    fn build_bit_left_shift(&self, lhs: ValueHandle, rhs: ValueHandle) -> ValueHandle;
    fn build_bit_right_shift(&self, lhs: ValueHandle, rhs: ValueHandle) -> ValueHandle;
    fn build_bit_right_shift_arithmetic(&self, lhs: ValueHandle, rhs: ValueHandle) -> ValueHandle;
    fn global_const(&self, name: &str, pltype: &PLType, ctx: &mut Ctx<'a>) -> ValueHandle;
    fn set_di_file(&self, f: &str);
}

pub type ValueHandle = usize;
pub type BlockHandle = usize;
#[allow(clippy::upper_case_acronyms)]
#[enum_dispatch(IRBuilder)]
pub enum BuilderEnum<'a, 'ctx> {
    #[cfg(feature = "llvm")]
    LLVM(LLVMBuilder<'a, 'ctx>),
    NoOp(NoOpBuilder<'a, 'ctx>),
}

// REVIEW: Maybe this belongs in some sort of prelude?
/// This enum defines how to compare a `left` and `right` `IntValue`.
#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum IntPredicate {
    /// Equal
    EQ,

    /// Not Equal
    NE,

    /// Unsigned Greater Than
    UGT,

    /// Unsigned Greater Than or Equal
    UGE,

    /// Unsigned Less Than
    ULT,

    /// Unsigned Less Than or Equal
    ULE,

    /// Signed Greater Than
    SGT,

    /// Signed Greater Than or Equal
    SGE,

    /// Signed Less Than
    SLT,

    /// Signed Less Than or Equal
    SLE,
}

// REVIEW: Maybe this belongs in some sort of prelude?
/// Defines how to compare a `left` and `right` `FloatValue`.
#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FloatPredicate {
    /// Returns true if `left` == `right` and neither are NaN
    OEQ,

    /// Returns true if `left` >= `right` and neither are NaN
    OGE,

    /// Returns true if `left` > `right` and neither are NaN
    OGT,

    /// Returns true if `left` <= `right` and neither are NaN
    OLE,

    /// Returns true if `left` < `right` and neither are NaN
    OLT,

    /// Returns true if `left` != `right` and neither are NaN
    ONE,

    /// Returns true if neither value is NaN
    ORD,

    /// Always returns false
    PredicateFalse,

    /// Always returns true
    PredicateTrue,

    /// Returns true if `left` == `right` or either is NaN
    UEQ,

    /// Returns true if `left` >= `right` or either is NaN
    UGE,

    /// Returns true if `left` > `right` or either is NaN
    UGT,

    /// Returns true if `left` <= `right` or either is NaN
    ULE,

    /// Returns true if `left` < `right` or either is NaN
    ULT,

    /// Returns true if `left` != `right` or either is NaN
    UNE,

    /// Returns true if either value is NaN
    UNO,
}
