use super::IRBuilder;

/// 什么都不干的builder，用来测试和lsp模式
pub struct NoOpBuilder {}

impl NoOpBuilder {
    pub fn new() -> Self {
        Self {}
    }
}

impl<'a, 'ctx> IRBuilder<'a, 'ctx> for NoOpBuilder {
    fn get_global_var_handle(&self, _name: &str) -> Option<super::ValueHandle> {
        Some(0)
    }

    fn new_subscope(&self, _start: crate::ast::range::Pos) {}

    fn add_global(
        &self,
        _name: &str,
        _pltype: std::rc::Rc<std::cell::RefCell<crate::ast::pltype::PLType>>,
        _ctx: &mut crate::ast::ctx::Ctx<'a>,
        _line: u32,
        _pltp: &crate::ast::pltype::PLType,
    ) -> super::ValueHandle {
        0
    }

    fn alloc_vtp(&self, _name: &str, _v: super::ValueHandle) -> super::ValueHandle {
        0
    }

    fn alloc(
        &self,
        _name: &str,
        _pltype: &crate::ast::pltype::PLType,
        _ctx: &mut crate::ast::ctx::Ctx<'a>,
    ) -> super::ValueHandle {
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
    ) -> super::ValueHandle {
        0
    }

    fn build_dbg_location(&self, _pos: crate::ast::range::Pos) {}

    fn build_in_bounds_gep(
        &self,
        _ptr: super::ValueHandle,
        _index: &[super::ValueHandle],
        _name: &str,
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
    ) -> Result<super::ValueHandle, ()> {
        Ok(0)
    }

    fn build_sub_program(
        &self,
        _paralist: Vec<Box<crate::ast::node::types::TypedIdentifierNode>>,
        _ret: Box<crate::ast::node::TypeNodeEnum>,
        _fntype: &crate::ast::pltype::FNType,
        _fnvalue: super::ValueHandle,
        _child: &mut crate::ast::ctx::Ctx<'a>,
    ) -> Result<(), crate::ast::ctx::PLDiag> {
        Ok(())
    }

    fn clear_insertion_position(&self) {}

    fn const_string(&self, _s: &str) -> super::ValueHandle {
        0
    }

    fn create_parameter_variable(
        &self,
        _fntype: &crate::ast::pltype::FNType,
        _pos: crate::ast::range::Pos,
        _i: usize,
        _child: &mut crate::ast::ctx::Ctx<'a>,
        _fnvalue: super::ValueHandle,
        _alloca: super::ValueHandle,
        _allocab: super::BlockHandle,
    ) {
    }

    fn delete_block(&self, _b: super::BlockHandle) {}

    fn finalize_debug(&self) {}

    fn float_value(&self, _ty: &crate::ast::pltype::PriType, _v: f64) -> super::ValueHandle {
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

    fn build_unconditional_branch(&self, _bb: super::BlockHandle) {}

    fn position_at_end_block(&self, _block: super::BlockHandle) {}

    fn add_body_to_struct_type(
        &self,
        _name: &str,
        _order_fields: &[crate::ast::pltype::Field],
        _ctx: &mut crate::ast::ctx::Ctx<'a>,
    ) {
    }

    fn get_or_insert_fn_handle(
        &self,
        _pltp: &crate::ast::pltype::FNType,
        _ctx: &mut crate::ast::ctx::Ctx<'a>,
    ) -> super::ValueHandle {
        0
    }

    fn mv2heap(
        &self,
        _val: super::ValueHandle,
        _ctx: &mut crate::ast::ctx::Ctx<'a>,
    ) -> super::ValueHandle {
        0
    }

    fn gc_add_root(
        &self,
        _stackptr: inkwell::values::BasicValueEnum<'ctx>,
        _ctx: &mut crate::ast::ctx::Ctx<'a>,
    ) {
    }

    fn gc_rm_root(&self, _stackptr: super::ValueHandle, _ctx: &mut crate::ast::ctx::Ctx<'a>) {}

    fn gc_rm_root_current(
        &self,
        _stackptr: super::ValueHandle,
        _ctx: &mut crate::ast::ctx::Ctx<'a>,
    ) {
    }

    fn gc_collect(&self, _ctx: &mut crate::ast::ctx::Ctx<'a>) {}

    fn get_or_add_global(
        &self,
        _name: &str,
        _pltype: std::rc::Rc<std::cell::RefCell<crate::ast::pltype::PLType>>,
        _ctx: &mut crate::ast::ctx::Ctx<'a>,
    ) -> super::ValueHandle {
        0
    }

    fn build_load(&self, _ptr: super::ValueHandle, _name: &str) -> super::ValueHandle {
        0
    }

    fn try_load2var(
        &self,
        _range: crate::ast::range::Range,
        _v: super::ValueHandle,
        tp: std::rc::Rc<std::cell::RefCell<crate::ast::pltype::PLType>>,
        _ctx: &mut crate::ast::ctx::Ctx<'a>,
    ) -> Result<
        (
            super::ValueHandle,
            std::rc::Rc<std::cell::RefCell<crate::ast::pltype::PLType>>,
        ),
        crate::ast::ctx::PLDiag,
    > {
        Ok((0, tp))
    }

    fn get_function(&self, _name: &str) -> Option<super::ValueHandle> {
        Some(0)
    }

    fn build_call(
        &self,
        _f: super::ValueHandle,
        _args: &[super::ValueHandle],
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
        _op: inkwell::FloatPredicate,
        _lhs: super::ValueHandle,
        _rhs: super::ValueHandle,
        _name: &str,
    ) -> super::ValueHandle {
        0
    }

    fn build_int_compare(
        &self,
        _op: inkwell::IntPredicate,
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
}
