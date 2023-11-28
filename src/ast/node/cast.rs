use std::{cell::RefCell, sync::Arc};

use super::super::builder::IntPredicate;
use internal_macro::node;

use crate::{
    ast::{
        builder::{BuilderEnum, IRBuilder, ValueHandle},
        ctx::Ctx,
        diag::{ErrorCode, PLDiag},
        node::{deal_line, tab, TypeNode},
        pltype::{get_type_deep, PLType, PriType},
        range::{Pos, Range},
        tokens::TokenType,
    },
    format_label,
    utils::get_hash_code,
};

use super::{
    node_result::NodeResultBuilder, Node, NodeEnum, NodeResult, PrintTrait, TypeNodeEnum,
    TypeNodeResult,
};
use crate::ast::node::RangeTrait;

#[node]
pub struct AsNode {
    pub expr: Box<NodeEnum>,
    pub ty: Box<TypeNodeEnum>,
    pub tail: Option<(TokenType, Range)>,
}

impl Node for AsNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        let re = self.expr.emit(ctx, builder);
        self.ty.emit_highlight(ctx);
        let v = re?.get_value();
        let v = v.unwrap();
        let target_tp = self.ty.get_type(ctx, builder, true)?;
        let (val, target_tp) = ctx.force_cast_safe(
            v.get_value(),
            &v.get_ty().borrow(),
            get_type_deep(target_tp),
            builder,
            self,
        )?;
        val.new_output(target_tp).to_result()
    }
}

impl PrintTrait for AsNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("AsNode");
        self.expr.print(tabs + 1, false, line.clone());
        self.ty.print(tabs + 1, true, line.clone());
    }
}

/// traitA{_vtable} _vtable -> global_traitA_StructA{fn1,fn2}
impl<'a, 'ctx> Ctx<'a> {
    /// # force_cast_safe
    ///
    /// Doing a safe cast.
    ///
    /// All primary types will be casted directly to the target type.
    ///
    /// All union types will be casted to the `Option` of the target type if the target type is
    /// the member of the union type
    pub fn force_cast_safe<'b>(
        &mut self,
        val: ValueHandle,
        ty: &PLType,
        target_ty: Arc<RefCell<PLType>>,
        builder: &'b BuilderEnum<'a, 'ctx>,
        node: &AsNode,
    ) -> Result<(ValueHandle, Arc<RefCell<PLType>>), PLDiag> {
        let target_rc = target_ty.clone();
        match (ty, &*target_ty.clone().borrow()) {
            (PLType::Primitive(tyi), PLType::Primitive(target_ty)) => {
                let val = builder.try_load2var(node.expr.range(), val, ty, self)?;
                Ok((builder.cast_primitives(val, tyi, target_ty), target_rc))
            }
            (PLType::Union(union), target_ty) => {
                if node.tail.is_none() {
                    let pos = node.ty.range().end;
                    let end_range = Range {
                        start: pos,
                        end: pos,
                    };
                    return Err(node.range.new_err(ErrorCode::INVALID_DIRECT_UNION_CAST)
                        .add_label(node.expr.range(), self.get_file(), format_label!("type of the expression is `{}`", &union.name))
                        .add_label(node.ty.range(), self.get_file(), format_label!("target type is `{}`", target_ty.get_name()))
                        .add_label(end_range, self.get_file(), format_label!("add `{}` or `{}` to make it legal", "?", "!"))
                        .add_help("cast a union to its member type directly is not allowed, use `?` or `!` after the cast expression")
                        .add_to_ctx(self));
                }
                if let Some(tag) = union.has_type(target_ty, self, builder) {
                    let (token, _) = node.tail.unwrap();
                    if token == TokenType::QUESTION {
                        Ok(self.cast_union_to(builder, val, tag, ty, target_rc))
                    } else {
                        Ok(self.force_cast_union_to(
                            builder,
                            val,
                            tag,
                            ty,
                            target_rc,
                            node.range.start,
                        ))
                    }
                } else {
                    Err(node
                        .range()
                        .new_err(ErrorCode::INVALID_UNION_CAST)
                        .add_label(
                            node.expr.range(),
                            self.get_file(),
                            format_label!("type of the expression is `{}`", &union.name),
                        )
                        .add_label(
                            node.ty.range(),
                            self.get_file(),
                            format_label!("target type is `{}`", target_ty.get_name()),
                        )
                        .add_help(
                            "either add a new member to the \
                        union type or cast to a member of the union type",
                        )
                        .add_to_ctx(self))
                }
            }
            (PLType::Trait(t), target_ty) => {
                if node.tail.is_none() {
                    let pos = node.ty.range().end;
                    let end_range = Range {
                        start: pos,
                        end: pos,
                    };
                    return Err(node.range.new_err(ErrorCode::INVALID_DIRECT_TRAIT_CAST)
                        .add_label(node.expr.range(), self.get_file(), format_label!("type of the expression is `{}`", &t.name))
                        .add_label(node.ty.range(), self.get_file(), format_label!("target type is `{}`", target_ty.get_name()))
                        .add_label(end_range, self.get_file(), format_label!("add `{}` or `{}` to make it legal", "?", "!"))
                        .add_help("cast a trait to specific type directly is not allowed, use `?` or `!` after the cast expression")
                        .add_to_ctx(self));
                }
                let (token, _) = node.tail.unwrap();
                if token == TokenType::QUESTION {
                    Ok(self.cast_trait_to(builder, val, ty, target_rc))
                } else {
                    Ok(self.force_cast_trait_to(builder, val, ty, target_rc, node.range.start))
                }
            }
            _ => {
                let re = self
                    .up_cast(
                        target_ty.clone(),
                        Arc::new(RefCell::new(ty.clone())),
                        node.ty.range(),
                        node.expr.range(),
                        val,
                        builder,
                    )
                    .map_err(|_| {
                        node.range()
                            .new_err(ErrorCode::INVALID_CAST)
                            .add_label(
                                node.expr.range(),
                                self.get_file(),
                                format_label!("type of the expression is `{}`", ty.get_name()),
                            )
                            .add_label(
                                node.ty.range(),
                                self.get_file(),
                                format_label!("target type is `{}`", target_ty.borrow().get_name()),
                            )
                            .add_help("`as` cast cannnot be performed between these types")
                            .add_to_ctx(self)
                    })?;
                Ok((re, target_rc))
            }
        }
    }
    fn cast_trait_to<'b>(
        &mut self,
        builder: &'b BuilderEnum<'a, 'ctx>,
        val: ValueHandle,
        ori_ty: &PLType,
        target_ty: Arc<RefCell<PLType>>,
    ) -> (ValueHandle, Arc<RefCell<PLType>>) {
        let hash = builder
            .build_struct_gep(val, 0, "tp_hash", ori_ty, self)
            .unwrap();
        let hash = builder.build_load(hash, "tp_hash", &PLType::new_i64(), self);
        let hasn_code = get_hash_code(target_ty.borrow().get_full_elm_name());
        let hash_code = builder.int_value(&PriType::U64, hasn_code, false);
        let cond_block = builder.append_basic_block(self.function.unwrap(), "if.cond");
        let then_block = builder.append_basic_block(self.function.unwrap(), "if.then");
        let else_block = builder.append_basic_block(self.function.unwrap(), "if.else");
        let after_block = builder.append_basic_block(self.function.unwrap(), "if.after");

        builder.build_unconditional_branch(cond_block);
        self.position_at_end(cond_block, builder);
        let cond = builder.build_int_compare(IntPredicate::EQ, hash, hash_code, "hash.eq");
        let cond = builder
            .try_load2var(
                Default::default(),
                cond,
                &PLType::Primitive(PriType::BOOL),
                self,
            )
            .unwrap();
        let cond = builder.build_int_truncate(cond, &PriType::BOOL, "trunctemp");

        self.build_ret_opt(
            builder,
            target_ty,
            then_block,
            val,
            cond,
            after_block,
            else_block,
            ori_ty,
        )
    }
    /// -> Option<_>
    fn cast_union_to<'b>(
        &mut self,
        builder: &'b BuilderEnum<'a, 'ctx>,
        val: ValueHandle,
        union_tag: usize,
        ori_ty: &PLType,
        target_ty: Arc<RefCell<PLType>>,
    ) -> (ValueHandle, Arc<RefCell<PLType>>) {
        let tag = builder
            .build_struct_gep(val, 0, "tag", ori_ty, self)
            .unwrap();

        // check if the tag is the same
        let tag = builder.build_load(tag, "tag", &PLType::new_i64(), self);
        let cond_block = builder.append_basic_block(self.function.unwrap(), "if.cond");
        let then_block = builder.append_basic_block(self.function.unwrap(), "if.then");
        let else_block = builder.append_basic_block(self.function.unwrap(), "if.else");
        let after_block = builder.append_basic_block(self.function.unwrap(), "if.after");
        builder.build_unconditional_branch(cond_block);
        self.position_at_end(cond_block, builder);
        let cond = builder.build_int_compare(
            IntPredicate::EQ,
            tag,
            builder.int_value(&PriType::U64, union_tag as u64, false),
            "tag.eq",
        );
        let cond = builder
            .try_load2var(
                Default::default(),
                cond,
                &PLType::Primitive(PriType::BOOL),
                self,
            )
            .unwrap();
        let cond = builder.build_int_truncate(cond, &PriType::BOOL, "trunctemp");

        self.build_ret_opt(
            builder,
            target_ty,
            then_block,
            val,
            cond,
            after_block,
            else_block,
            ori_ty,
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn build_ret_opt<'b>(
        &mut self,
        builder: &'b BuilderEnum<'a, 'ctx>,
        target_ty: Arc<RefCell<PLType>>,
        then_block: usize,
        val: usize,
        cond: usize,
        after_block: usize,
        else_block: usize,
        ori_ty: &PLType,
    ) -> (usize, Arc<RefCell<PLType>>) {
        let result_tp = get_option_type(self, builder, target_ty.clone()).unwrap();
        let result = builder.alloc("cast_result", &result_tp.borrow(), self, None);
        let result_tag_field = builder
            .build_struct_gep(result, 0, "tag", &result_tp.borrow(), self)
            .unwrap();
        let result_data_field = builder
            .build_struct_gep(result, 1, "data", &result_tp.borrow(), self)
            .unwrap();
        builder.build_conditional_branch(cond, then_block, else_block);
        // then block
        self.position_at_end(then_block, builder);
        let data = builder
            .build_struct_gep(val, 1, "data", ori_ty, self)
            .unwrap();
        let data = builder.build_load(data, "data", &target_ty.borrow(), self);
        builder.build_store(result_data_field, data);
        builder.build_store(result_tag_field, builder.int_value(&PriType::U64, 0, false));
        builder.build_unconditional_branch(after_block);
        // else block
        self.position_at_end(else_block, builder);
        // builder.build_store(result_data_field, builder.int_value(&PriType::U64, 0, false)); TODO store null
        builder.build_store(result_tag_field, builder.int_value(&PriType::U64, 1, false));
        builder.build_unconditional_branch(after_block);
        // after block
        self.position_at_end(after_block, builder);
        (result, result_tp)
    }

    fn force_cast_trait_to<'b>(
        &mut self,
        builder: &'b BuilderEnum<'a, 'ctx>,
        val: ValueHandle,
        ori_ty: &PLType,
        target_ty: Arc<RefCell<PLType>>,
        pos: Pos,
    ) -> (ValueHandle, Arc<RefCell<PLType>>) {
        let hash = builder
            .build_struct_gep(val, 0, "tp_hash", ori_ty, self)
            .unwrap();
        let hash = builder.build_load(hash, "tp_hash", &PLType::new_i64(), self);
        let hasn_code = get_hash_code(target_ty.borrow().get_full_elm_name());
        let hash_code = builder.int_value(&PriType::U64, hasn_code, false);
        let cond_block = builder.append_basic_block(self.function.unwrap(), "if.cond");
        let then_block = builder.append_basic_block(self.function.unwrap(), "if.then");
        let else_block = builder.append_basic_block(self.function.unwrap(), "if.else");
        let after_block = builder.append_basic_block(self.function.unwrap(), "if.after");

        let result_tp = target_ty.clone();
        let result = builder.alloc("cast_result", &result_tp.borrow(), self, None);

        builder.build_unconditional_branch(cond_block);
        self.position_at_end(cond_block, builder);
        let cond = builder.build_int_compare(IntPredicate::EQ, hash, hash_code, "hash.eq");
        let cond = builder
            .try_load2var(
                Default::default(),
                cond,
                &PLType::Primitive(PriType::BOOL),
                self,
            )
            .unwrap();
        let cond = builder.build_int_truncate(cond, &PriType::BOOL, "trunctemp");
        builder.build_conditional_branch(cond, then_block, else_block);
        self.build_cast_ret(
            ori_ty,
            target_ty,
            builder,
            then_block,
            val,
            after_block,
            else_block,
            pos,
            result,
        );
        (result, result_tp)
    }
    fn force_cast_union_to<'b>(
        &mut self,
        builder: &'b BuilderEnum<'a, 'ctx>,
        val: ValueHandle,
        union_tag: usize,
        ori_ty: &PLType,
        target_ty: Arc<RefCell<PLType>>,
        pos: Pos,
    ) -> (ValueHandle, Arc<RefCell<PLType>>) {
        let tag = builder
            .build_struct_gep(val, 0, "tag", ori_ty, self)
            .unwrap();
        // check if the tag is the same
        let tag = builder.build_load(tag, "tag", &PLType::new_i64(), self);
        let cond_block = builder.append_basic_block(self.function.unwrap(), "force.if.cond");
        let then_block = builder.append_basic_block(self.function.unwrap(), "force.if.then");
        let else_block = builder.append_basic_block(self.function.unwrap(), "force.if.else");
        let after_block = builder.append_basic_block(self.function.unwrap(), "force.if.after");

        let result_tp = target_ty.clone();
        let result = builder.alloc("cast_result", &result_tp.borrow(), self, None);

        builder.build_unconditional_branch(cond_block);
        self.position_at_end(cond_block, builder);
        let cond = builder.build_int_compare(
            IntPredicate::EQ,
            tag,
            builder.int_value(&PriType::U64, union_tag as u64, false),
            "tag.eq",
        );
        let cond = builder
            .try_load2var(
                Default::default(),
                cond,
                &PLType::Primitive(PriType::BOOL),
                self,
            )
            .unwrap();
        let cond = builder.build_int_truncate(cond, &PriType::BOOL, "trunctemp");
        builder.build_conditional_branch(cond, then_block, else_block);
        self.build_cast_ret(
            ori_ty,
            target_ty,
            builder,
            then_block,
            val,
            after_block,
            else_block,
            pos,
            result,
        );
        (result, result_tp)
    }

    #[allow(clippy::too_many_arguments)]
    fn build_cast_ret<'b>(
        &mut self,
        ori_ty: &PLType,
        target_ty: Arc<RefCell<PLType>>,
        builder: &'b BuilderEnum<'a, 'ctx>,
        then_block: usize,
        val: usize,
        after_block: usize,
        else_block: usize,
        pos: Pos,
        result: usize,
    ) {
        // then block

        self.position_at_end(then_block, builder);
        let data = builder
            .build_struct_gep(val, 1, "data", ori_ty, self)
            .unwrap();
        let data = builder.build_load(data, "data", &PLType::new_i8_ptr(), self);
        let data = builder.bitcast(
            self,
            data,
            &PLType::Pointer(target_ty.clone()),
            "bitcasttemp",
        );
        let data = builder.build_load(data, "data", &target_ty.borrow(), self);
        builder.build_store(result, data);
        builder.build_unconditional_branch(after_block);
        // else block
        self.position_at_end(else_block, builder);
        // builder.build_store(result_data_field, builder.int_value(&PriType::U64, 0, false)); TODO store null
        let cp = builder.get_or_insert_helper_fn_handle("__cast_panic");
        builder.try_set_fn_dbg(pos, cp);
        builder.build_call(cp, &[], &PLType::Void, self,None);
        builder.try_set_fn_dbg(pos, self.function.unwrap());
        builder.build_unconditional_branch(after_block);
        // after block
        self.position_at_end(after_block, builder);
    }
}

pub fn get_option_type<'a, 'b>(
    ctx: &'b mut Ctx<'a>,
    builder: &'b BuilderEnum<'a, '_>,
    target_ty: Arc<RefCell<PLType>>,
) -> TypeNodeResult {
    let pltype = ctx.get_type("Option", Default::default()).unwrap();
    if let PLType::Union(union) = &*pltype.borrow() {
        let mut union = union.clone();
        if let PLType::Generic(g) = &mut *union.generic_map.get("T").unwrap().borrow_mut() {
            g.set_type(target_ty);
        }
        if union.need_gen_code() {
            union = ctx
                .protect_generic_context(&union.generic_map, |ctx| union.gen_code(ctx, builder))?;
            let pltype = Arc::new(RefCell::new(PLType::Union(union)));
            return Ok(pltype);
        } else {
            unreachable!()
        }
    };
    unreachable!()
}

#[node]
pub struct IsNode {
    pub expr: Box<NodeEnum>,
    pub ty: Box<TypeNodeEnum>,
}

impl Node for IsNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        let re = self.expr.emit(ctx, builder);
        self.ty.emit_highlight(ctx);
        let v = re?.get_value().unwrap();
        let target_tp = self.ty.get_type(ctx, builder, true)?;
        let val = v.get_value();
        let binding = v.get_ty();
        let tp = &*binding.borrow();
        match tp {
            PLType::Union(u) => {
                if let Some(tag) = u.has_type(&target_tp.borrow(), ctx, builder) {
                    let tag_v = builder.build_struct_gep(val, 0, "tag", tp, ctx).unwrap();
                    let tag_v = builder.build_load(tag_v, "tag", &PLType::new_i64(), ctx);
                    let cond = builder.build_int_compare(
                        IntPredicate::EQ,
                        tag_v,
                        builder.int_value(&PriType::U64, tag as u64, false),
                        "tag.eq",
                    );
                    let cond = builder
                        .try_load2var(
                            Default::default(),
                            cond,
                            &PLType::Primitive(PriType::BOOL),
                            ctx,
                        )
                        .unwrap();
                    let cond = builder.build_int_truncate(cond, &PriType::BOOL, "trunctemp");
                    cond.new_output(ctx.get_type("bool", Default::default()).unwrap().tp)
                        .set_const()
                        .to_result()
                } else {
                    Err(self
                        .ty
                        .range()
                        .new_err(ErrorCode::UNION_DOES_NOT_CONTAIN_TYPE)
                        .add_to_ctx(ctx))
                }
            }
            PLType::Trait(_) => {
                let name = target_tp.borrow().get_full_elm_name();
                let hash_code = get_hash_code(name);
                let hash_code = builder.int_value(&PriType::U64, hash_code, false);
                let hash = builder
                    .build_struct_gep(val, 0, "tp_hash", tp, ctx)
                    .unwrap();
                let hash = builder.build_load(hash, "tp_hash", &PLType::new_i64(), ctx);
                let cond = builder.build_int_compare(IntPredicate::EQ, hash, hash_code, "hash.eq");
                let cond = builder
                    .try_load2var(
                        Default::default(),
                        cond,
                        &PLType::Primitive(PriType::BOOL),
                        ctx,
                    )
                    .unwrap();
                let cond = builder.build_int_truncate(cond, &PriType::BOOL, "trunctemp");
                cond.new_output(ctx.get_type("bool", Default::default()).unwrap().tp)
                    .set_const()
                    .to_result()
            }
            _ => Err(self
                .range()
                .new_err(ErrorCode::INVALID_IS_EXPR)
                .add_help("`is` can only be used on union or trait types")
                .add_to_ctx(ctx)),
        }
    }
}
impl PrintTrait for IsNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("IsNode");
        self.expr.print(tabs + 1, false, line.clone());
        self.ty.print(tabs + 1, true, line.clone());
    }
}
