use std::{cell::RefCell, sync::Arc};

use crate::ast::{
    ctx::Ctx,
    pltype::{PLType, PriType, STType},
};

use super::{IRBuilder, ValueHandle};

/// 什么都不干的builder，用来测试和lsp模式
#[derive(Default)]
pub struct NoOpBuilder<'a, 'ctx> {
    _phantom: std::marker::PhantomData<(&'a (), &'ctx ())>,
}

impl<'a, 'ctx> IRBuilder<'a, 'ctx> for NoOpBuilder<'a, 'ctx> {
    fn bitcast(
        &self,
        _ctx: &mut Ctx<'a>,
        _from: ValueHandle,
        _to: &PLType,
        _name: &str,
    ) -> ValueHandle {
        0
    }
    fn pointer_cast(
        &self,
        _ctx: &mut Ctx<'a>,
        _from: ValueHandle,
        _to: &PLType,
        _name: &str,
    ) -> ValueHandle {
        0
    }
    fn get_global_var_handle(&self, _name: &str) -> Option<super::ValueHandle> {
        Some(0)
    }

    fn new_subscope(&self, _start: crate::ast::range::Pos) {}

    fn add_global(
        &self,
        _name: &str,
        _pltype: Arc<std::cell::RefCell<crate::ast::pltype::PLType>>,
        _ctx: &mut crate::ast::ctx::Ctx<'a>,
        _line: u32,
        _pltp: &crate::ast::pltype::PLType,
    ) -> super::ValueHandle {
        0
    }

    fn alloc(
        &self,
        _name: &str,
        _pltype: &crate::ast::pltype::PLType,
        _ctx: &mut crate::ast::ctx::Ctx<'a>,
        _declare: Option<crate::ast::range::Pos>,
    ) -> super::ValueHandle {
        0
    }
    fn get_stack_root(&self, _v: ValueHandle) -> ValueHandle {
        0
    }

    fn build_conditional_branch(
        &self,
        _cond: super::ValueHandle,
        _then_bb: super::BlockHandle,
        _else_bb: super::BlockHandle,
    ) {
    }

    fn build_const_in_bounds_gep(
        &self,
        _ptr: super::ValueHandle,
        _index: &[u64],
        _name: &str,
        _tp: &PLType,
        _ctx: &mut Ctx<'a>,
    ) -> super::ValueHandle {
        0
    }

    fn build_dbg_location(&self, _pos: crate::ast::range::Pos) {}

    fn build_in_bounds_gep(
        &self,
        _ptr: super::ValueHandle,
        _index: &[super::ValueHandle],
        _name: &str,
        _tp: &PLType,
        _ctx: &mut Ctx<'a>,
    ) -> super::ValueHandle {
        0
    }

    fn build_return(&self, _v: Option<super::ValueHandle>) {}

    fn build_store(&self, _ptr: super::ValueHandle, _value: super::ValueHandle) {}

    fn build_struct_gep(
        &self,
        _structv: super::ValueHandle,
        _index: u32,
        _name: &str,
        _tp: &PLType,
        _ctx: &mut Ctx<'a>,
    ) -> Result<ValueHandle, String> {
        Ok(0)
    }

    fn build_sub_program(
        &self,
        _paralist: Vec<Box<crate::ast::node::types::TypedIdentifierNode>>,
        _ret: Box<crate::ast::node::TypeNodeEnum>,
        _fntype: &crate::ast::pltype::FNValue,
        _fnvalue: super::ValueHandle,
        _child: &mut crate::ast::ctx::Ctx<'a>,
    ) -> Result<(), crate::ast::diag::PLDiag> {
        Ok(())
    }

    fn clear_insertion_position(&self) {}

    fn const_string(&self, _s: &str) -> super::ValueHandle {
        0
    }

    fn create_parameter_variable(
        &self,
        _fntype: &crate::ast::pltype::FNValue,
        _pos: crate::ast::range::Pos,
        _i: usize,
        _child: &mut crate::ast::ctx::Ctx<'a>,
        _fnvalue: super::ValueHandle,
        _alloca: super::ValueHandle,
        _allocab: super::BlockHandle,
        _tp: &PLType,
    ) {
    }

    fn delete_block(&self, _b: super::BlockHandle) {}

    fn finalize_debug(&self) {}

    fn float_value(&self, _ty: &crate::ast::pltype::PriType, _v: f64) -> super::ValueHandle {
        0
    }
    fn get_cur_basic_block(&self) -> super::BlockHandle {
        0
    }
    fn get_first_basic_block(&self, _v: super::ValueHandle) -> super::BlockHandle {
        0
    }

    fn get_first_instruction(&self, _bb: super::BlockHandle) -> Option<super::ValueHandle> {
        Some(0)
    }

    fn get_last_basic_block(&self, _v: super::ValueHandle) -> super::BlockHandle {
        0
    }

    fn insert_var_declare(
        &self,
        _name: &str,
        _pos: crate::ast::range::Pos,
        _pltype: &crate::ast::pltype::PLType,
        _v: super::ValueHandle,
        _ctx: &mut crate::ast::ctx::Ctx<'a>,
    ) {
    }

    fn int_value(
        &self,
        _ty: &crate::ast::pltype::PriType,
        _v: u64,
        _sign_ext: bool,
    ) -> super::ValueHandle {
        0
    }

    fn position_at(&self, _v: super::ValueHandle) {}

    fn print_to_file(&self, _file: &std::path::Path) -> Result<(), String> {
        Ok(())
    }

    fn rm_curr_debug_location(&self) {}

    fn try_set_fn_dbg(&self, _pos: crate::ast::range::Pos, _f: super::ValueHandle) {}

    fn write_bitcode_to_path(&self, _path: &std::path::Path) -> bool {
        true
    }
    fn build_phi(
        &self,
        _pltype: &PLType,
        _ctx: &mut Ctx<'a>,
        _vbs: &[(ValueHandle, super::BlockHandle)],
    ) -> ValueHandle {
        0
    }
    fn build_unconditional_branch(&self, _bb: super::BlockHandle) {}

    fn position_at_end_block(&self, _block: super::BlockHandle) {}

    fn add_body_to_struct_type(
        &self,
        _name: &str,
        _sttype: &STType,
        _ctx: &mut crate::ast::ctx::Ctx<'a>,
    ) {
    }

    fn get_or_insert_fn_handle(
        &self,
        _pltp: &crate::ast::pltype::FNValue,
        _ctx: &mut crate::ast::ctx::Ctx<'a>,
    ) -> (super::ValueHandle, bool) {
        (0, false)
    }

    fn get_or_add_global(
        &self,
        _name: &str,
        _pltype: Arc<std::cell::RefCell<crate::ast::pltype::PLType>>,
        _ctx: &mut crate::ast::ctx::Ctx<'a>,
        _constant: bool,
    ) -> super::ValueHandle {
        0
    }

    fn build_load(
        &self,
        _ptr: super::ValueHandle,
        _name: &str,
        _tp: &PLType,
        _ctx: &mut Ctx<'a>,
    ) -> super::ValueHandle {
        0
    }

    fn try_load2var(
        &self,
        _range: crate::ast::range::Range,
        _v: super::ValueHandle,
        _tp: &PLType,
        _ctx: &mut Ctx<'a>,
    ) -> Result<super::ValueHandle, crate::ast::diag::PLDiag> {
        Ok(0)
    }

    fn get_function(&self, _name: &str) -> Option<super::ValueHandle> {
        Some(0)
    }

    fn build_call(
        &self,
        _f: super::ValueHandle,
        _args: &[super::ValueHandle],
        _ret_type: &PLType,
        _ctx: &mut Ctx<'a>,
    ) -> Option<super::ValueHandle> {
        Some(0)
    }

    fn add_function(
        &self,
        _name: &str,
        _paramtps: &[crate::ast::pltype::PLType],
        _ret: crate::ast::pltype::PLType,
        _ctx: &mut crate::ast::ctx::Ctx<'a>,
    ) -> super::ValueHandle {
        0
    }

    fn opaque_struct_type(&self, _name: &str) {}

    fn build_int_z_extend(
        &self,
        _v: super::ValueHandle,
        _ty: &crate::ast::pltype::PriType,
        _name: &str,
    ) -> super::ValueHandle {
        0
    }

    fn build_or(
        &self,
        _lhs: super::ValueHandle,
        _rhs: super::ValueHandle,
        _name: &str,
    ) -> super::ValueHandle {
        0
    }

    fn build_and(
        &self,
        _lhs: super::ValueHandle,
        _rhs: super::ValueHandle,
        _name: &str,
    ) -> super::ValueHandle {
        0
    }

    fn build_float_compare(
        &self,
        _op: super::FloatPredicate,
        _lhs: super::ValueHandle,
        _rhs: super::ValueHandle,
        _name: &str,
    ) -> super::ValueHandle {
        0
    }

    fn build_int_compare(
        &self,
        _op: super::IntPredicate,
        _lhs: super::ValueHandle,
        _rhs: super::ValueHandle,
        _name: &str,
    ) -> super::ValueHandle {
        0
    }

    fn build_int_add(
        &self,
        _lhs: super::ValueHandle,
        _rhs: super::ValueHandle,
        _name: &str,
    ) -> super::ValueHandle {
        0
    }

    fn build_int_sub(
        &self,
        _lhs: super::ValueHandle,
        _rhs: super::ValueHandle,
        _name: &str,
    ) -> super::ValueHandle {
        0
    }

    fn build_int_mul(
        &self,
        _lhs: super::ValueHandle,
        _rhs: super::ValueHandle,
        _name: &str,
    ) -> super::ValueHandle {
        0
    }

    fn build_int_signed_div(
        &self,
        _lhs: super::ValueHandle,
        _rhs: super::ValueHandle,
        _name: &str,
    ) -> super::ValueHandle {
        0
    }

    fn build_float_neg(&self, _v: super::ValueHandle, _name: &str) -> super::ValueHandle {
        0
    }

    fn build_float_add(
        &self,
        _lhs: super::ValueHandle,
        _rhs: super::ValueHandle,
        _name: &str,
    ) -> super::ValueHandle {
        0
    }

    fn build_float_sub(
        &self,
        _lhs: super::ValueHandle,
        _rhs: super::ValueHandle,
        _name: &str,
    ) -> super::ValueHandle {
        0
    }

    fn build_float_mul(
        &self,
        _lhs: super::ValueHandle,
        _rhs: super::ValueHandle,
        _name: &str,
    ) -> super::ValueHandle {
        0
    }

    fn build_float_div(
        &self,
        _lhs: super::ValueHandle,
        _rhs: super::ValueHandle,
        _name: &str,
    ) -> super::ValueHandle {
        0
    }

    fn append_basic_block(&self, _func: super::ValueHandle, _name: &str) -> super::BlockHandle {
        0
    }

    fn build_int_truncate(
        &self,
        _v: super::ValueHandle,
        _dest_ty: &crate::ast::pltype::PriType,
        _name: &str,
    ) -> super::ValueHandle {
        0
    }

    fn build_int_neg(&self, _v: super::ValueHandle, _name: &str) -> super::ValueHandle {
        0
    }

    fn gen_st_visit_function(
        &self,
        _ctx: &mut Ctx<'a>,
        _v: &STType,
        _field_tps: &[Arc<RefCell<PLType>>],
    ) {
    }
    fn cast_primitives(
        &self,
        _handle: ValueHandle,
        _tp: &PriType,
        _target: &PriType,
    ) -> ValueHandle {
        0
    }
    fn is_ptr(&self, _v: ValueHandle) -> bool {
        false
    }
    fn get_or_insert_helper_fn_handle(&self, _name: &str) -> ValueHandle {
        0
    }

    fn create_closure_fn(
        &self,
        _ctx: &mut Ctx<'a>,
        _closure_name: &str,
        _params: &[Arc<RefCell<PLType>>],
        _ret: &PLType,
    ) -> ValueHandle {
        0
    }

    fn i8ptr_null(&self) -> ValueHandle {
        0
    }

    fn get_closure_trampoline(&self, f: ValueHandle) -> ValueHandle {
        f
    }

    fn create_closure_parameter_variable(
        &self,
        _i: u32,
        _f: ValueHandle,
        _alloca: ValueHandle,
        _allocab: super::BlockHandle,
        _tp: &PLType,
    ) {
    }

    fn get_nth_param(&self, f: ValueHandle, _i: u32) -> ValueHandle {
        f
    }

    fn add_closure_st_field(&self, _st: &STType, _field: ValueHandle, _ctx: &mut Ctx<'a>) {}

    fn build_sub_program_by_pltp(
        &self,
        _paralist: &[Arc<RefCell<PLType>>],
        _ret: Arc<RefCell<PLType>>,
        _name: &str,
        _start_line: u32,
        _fnvalue: ValueHandle,
        _child: &mut Ctx<'a>,
    ) {
    }
    #[allow(clippy::too_many_arguments)]
    fn create_parameter_variable_dbg(
        &self,
        _pltp: &PLType,
        _pos: crate::ast::range::Pos,
        _i: usize,
        _child: &mut Ctx<'a>,
        _value_handle: ValueHandle,
        _allocab: super::BlockHandle,
        _name: &str,
    ) {
    }

    fn add_generator_yield_fn(
        &self,
        _ctx: &mut Ctx<'a>,
        _ctx_name: &str,
        _ret_tp: &PLType,
    ) -> ValueHandle {
        0
    }

    fn get_block_address(&self, _block: super::BlockHandle) -> ValueHandle {
        0
    }

    fn build_indirect_br(&self, _block: ValueHandle, _ctx: &Ctx<'a>) {}

    unsafe fn store_with_aoto_cast(&self, _ptr: ValueHandle, _value: ValueHandle) {}

    fn stack_alloc(&self, _name: &str, _ctx: &mut Ctx<'a>, _tp: &PLType) -> ValueHandle {
        0
    }

    fn correct_generator_ctx_malloc_inst(&self, _ctx: &mut Ctx<'a>, _name: &str) {}

    fn sizeof(&self, _pltype: &PLType, _ctx: &mut Ctx<'a>) -> u64 {
        0
    }
    fn build_memcpy(
        &self,
        _from: ValueHandle,
        _from_tp: &PLType,
        _to: ValueHandle,
        _len: ValueHandle,
        _ctx: &mut Ctx<'a>,
    ) {
    }

    fn build_bit_not(&self, _v: ValueHandle) -> ValueHandle {
        0
    }

    fn build_bit_and(&self, _lhs: ValueHandle, _rhs: ValueHandle) -> ValueHandle {
        0
    }

    fn build_bit_or(&self, _lhs: ValueHandle, _rhs: ValueHandle) -> ValueHandle {
        0
    }

    fn build_bit_xor(&self, _lhs: ValueHandle, _rhs: ValueHandle) -> ValueHandle {
        0
    }

    fn build_bit_left_shift(&self, _lhs: ValueHandle, _rhs: ValueHandle) -> ValueHandle {
        0
    }

    fn build_bit_right_shift(&self, _lhs: ValueHandle, _rhs: ValueHandle) -> ValueHandle {
        0
    }

    fn build_bit_right_shift_arithmetic(
        &self,
        _lhs: ValueHandle,
        _rhs: ValueHandle,
    ) -> ValueHandle {
        0
    }

    fn global_const(&self, _name: &str, _pltype: &PLType, _ctx: &mut Ctx<'a>) -> ValueHandle {
        0
    }

    fn build_int_signed_srem(
        &self,
        _lhs: ValueHandle,
        _rhs: ValueHandle,
        _name: &str,
    ) -> ValueHandle {
        0
    }

    fn build_int_unsigned_div(
        &self,
        _lhs: ValueHandle,
        _rhs: ValueHandle,
        _name: &str,
    ) -> ValueHandle {
        0
    }

    fn build_int_unsigned_srem(
        &self,
        _lhs: ValueHandle,
        _rhs: ValueHandle,
        _name: &str,
    ) -> ValueHandle {
        0
    }
    fn set_di_file(&self, _f: &str) {}
    fn tag_generator_ctx_as_root(&self, _f: ValueHandle, _ctx: &mut Ctx<'a>) {}
    fn create_params_roots(
        &self,
        _f: ValueHandle,
        _allocab: ValueHandle,
        _params: &[Arc<RefCell<PLType>>],
    ) {
    }
}
