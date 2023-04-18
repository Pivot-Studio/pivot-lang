use std::{cell::RefCell, sync::Arc};

use inkwell::IntPredicate;
use internal_macro::node;

use crate::{
    ast::{
        builder::{BuilderEnum, IRBuilder, ValueHandle},
        ctx::Ctx,
        diag::{ErrorCode, PLDiag},
        node::{deal_line, tab, TypeNode},
        pltype::{PLType, PriType},
        range::{Pos, Range},
        tokens::TokenType,
    },
    format_label,
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
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        let re = self.expr.emit(ctx, builder);
        self.ty.emit_highlight(ctx);
        let v = re?.get_value();
        let v = v.unwrap();
        let target_tp = self.ty.get_type(ctx, builder)?;
        let (val, target_tp) = ctx.force_cast_safe(
            v.get_value(),
            &v.get_ty().borrow(),
            target_tp,
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
        match (ty, &*target_ty.borrow()) {
            (PLType::Primitive(ty), PLType::Primitive(target_ty)) => {
                let val = builder.try_load2var(node.expr.range(), val, self)?;
                Ok((builder.cast_primitives(val, ty, target_ty), target_rc))
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
                        Ok(self.cast_union_to(builder, val, tag, target_rc))
                    } else {
                        Ok(
                            self.force_cast_union_to(
                                builder,
                                val,
                                tag,
                                target_rc,
                                node.range.start,
                            ),
                        )
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
            _ => Err(node
                .range()
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
                .add_help(
                    "`as` cast can only be performed between primitives \
                or from union types.",
                )
                .add_to_ctx(self)),
        }
    }
    /// Option<i128>
    fn cast_union_to<'b>(
        &mut self,
        builder: &'b BuilderEnum<'a, 'ctx>,
        val: ValueHandle,
        union_tag: usize,
        target_ty: Arc<RefCell<PLType>>,
    ) -> (ValueHandle, Arc<RefCell<PLType>>) {
        let tag = builder.build_struct_gep(val, 0, "tag").unwrap();
        let result_tp = get_option_type(self, builder, target_ty).unwrap();
        let result = builder.alloc("cast_result", &result_tp.borrow(), self, None);
        let result_tag_field = builder.build_struct_gep(result, 0, "tag").unwrap();
        let result_data_field = builder.build_struct_gep(result, 1, "data").unwrap();
        // check if the tag is the same
        let tag = builder.build_load(tag, "tag");
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
            .try_load2var(Default::default(), cond, self)
            .unwrap();
        let cond = builder.build_int_truncate(cond, &PriType::BOOL, "trunctemp");
        builder.build_conditional_branch(cond, then_block, else_block);
        // then block
        self.position_at_end(then_block, builder);
        let data = builder.build_struct_gep(val, 1, "data").unwrap();
        let data = builder.build_load(data, "data");
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
    fn force_cast_union_to<'b>(
        &mut self,
        builder: &'b BuilderEnum<'a, 'ctx>,
        val: ValueHandle,
        union_tag: usize,
        target_ty: Arc<RefCell<PLType>>,
        pos: Pos,
    ) -> (ValueHandle, Arc<RefCell<PLType>>) {
        let tag = builder.build_struct_gep(val, 0, "tag").unwrap();
        let result_tp = target_ty.clone();
        let result = builder.alloc("cast_result", &result_tp.borrow(), self, None);
        // check if the tag is the same
        let tag = builder.build_load(tag, "tag");
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
            .try_load2var(Default::default(), cond, self)
            .unwrap();
        let cond = builder.build_int_truncate(cond, &PriType::BOOL, "trunctemp");
        builder.build_conditional_branch(cond, then_block, else_block);
        // then block
        self.position_at_end(then_block, builder);
        let data = builder.build_struct_gep(val, 1, "data").unwrap();
        let data = builder.build_load(data, "data");
        let data = builder.bitcast(self, data, &PLType::Pointer(target_ty), "bitcasttemp");
        let data = builder.build_load(data, "data");
        builder.build_store(result, data);
        builder.build_unconditional_branch(after_block);
        // else block
        self.position_at_end(else_block, builder);
        // builder.build_store(result_data_field, builder.int_value(&PriType::U64, 0, false)); TODO store null
        let cp = builder.get_or_insert_helper_fn_handle("__cast_panic");
        builder.try_set_fn_dbg(pos, cp);
        builder.build_call(cp, &[], &PLType::Void, self);
        builder.build_unconditional_branch(after_block);
        // after block
        self.position_at_end(after_block, builder);
        (result, result_tp)
    }
}

fn get_option_type<'a, 'ctx, 'b>(
    ctx: &'b mut Ctx<'a>,
    builder: &'b BuilderEnum<'a, 'ctx>,
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
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        let re = self.expr.emit(ctx, builder);
        self.ty.emit_highlight(ctx);
        let v = re?.get_value().unwrap();
        let target_tp = self.ty.get_type(ctx, builder)?;
        let val = v.get_value();
        let binding = v.get_ty();
        let tp = &*binding.borrow();
        match tp {
            PLType::Union(u) => {
                if let Some(tag) = u.has_type(&target_tp.borrow(), ctx, builder) {
                    let tag_v = builder.build_struct_gep(val, 0, "tag").unwrap();
                    let tag_v = builder.build_load(tag_v, "tag");
                    let cond = builder.build_int_compare(
                        IntPredicate::EQ,
                        tag_v,
                        builder.int_value(&PriType::U64, tag as u64, false),
                        "tag.eq",
                    );
                    let cond = builder.try_load2var(Default::default(), cond, ctx).unwrap();
                    let cond = builder.build_int_truncate(cond, &PriType::BOOL, "trunctemp");
                    cond.new_output(ctx.get_type("bool", Default::default()).unwrap())
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
            _ => Err(self
                .range()
                .new_err(ErrorCode::INVALID_IS_EXPR)
                .add_help("`is` can only be used on union types")
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
