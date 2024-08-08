use super::node_result::TerminatorEnum;
use super::*;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::ast::tokens::TokenType;
use crate::ast::{ctx::Ctx, diag::ErrorCode};
use crate::format_label;
use internal_macro::node;

#[node(comment)]
pub struct RetNode {
    /// the value returned by the keyword `return`
    pub value: Option<Box<NodeEnum>>,

    /// yield_identifier refers whether the keyword 'yield' exists and the range of it
    pub yield_identifier: Option<(TokenType, Range)>,
}

impl PrintTrait for RetNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("RetNode");
        if let Some(value) = &self.value {
            value.print(tabs + 1, true, line.clone());
        }
    }
}

impl Node for RetNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        let ret_pltype = ctx.rettp.as_ref().unwrap().clone();
        if self.yield_identifier.is_some() {
            if self.value.is_none() {
                return Err(self
                    .range
                    .new_err(ErrorCode::GENERATOR_FUNCTION_CANNOT_RETURN_VOID)
                    .add_to_ctx(ctx));
            }
            let ret_node = self.value.as_mut().unwrap();
            let v = ret_node.emit(ctx, builder)?.get_value().unwrap();
            if ctx.generator_data.is_none() {
                return Err(self
                    .range
                    .new_err(ErrorCode::YIELD_RETURN_MUST_BE_IN_GENERATOR)
                    .add_to_ctx(ctx));
            }
            ctx.emit_comment_highlight(&self.comments[0]);
            let value_pltype = v.get_ty();
            let v_tp = if let PLType::Union(u) = &*ret_pltype.borrow() {
                u.sum_types[0].get_type(ctx, builder, false)?
            } else {
                Arc::new(RefCell::new(PLType::Unknown))
            };
            if get_type_deep(v_tp.clone()) != get_type_deep(value_pltype.clone()) {
                let err = self
                    .range
                    .new_err(ErrorCode::RETURN_TYPE_MISMATCH)
                    .add_label(
                        self.range.start_point(),
                        ctx.get_file(),
                        format_label!("expected type {}", v_tp.borrow().get_name()),
                    )
                    .add_label(
                        ret_node.range().start_point(),
                        ctx.get_file(),
                        format_label!("found type {}", value_pltype.borrow().get_name()),
                    )
                    .add_to_ctx(ctx);
                return Err(err);
            }
            // let value = ctx.generator_data.as_ref().unwrap().borrow().ret_handle;
            let value = ctx.up_cast(
                ret_pltype.clone(),
                value_pltype,
                ret_node.range(),
                ret_node.range(),
                v.get_value(),
                builder,
            )?;
            let value = ctx.try_load2var(self.range, value, builder, &ret_pltype.borrow())?;
            builder.build_store(ctx.return_block.unwrap().1.unwrap(), value);
            let curbb = builder.get_cur_basic_block();

            let yield_bb = builder.append_basic_block(ctx.function.unwrap(), "yield");

            ctx.generator_data
                .as_ref()
                .unwrap()
                .borrow_mut()
                .prev_yield_bb = Some(curbb);
            ctx.add_term_to_previous_yield_and_ret(builder, yield_bb);
            ctx.position_at_end(yield_bb, builder);
            return NodeOutput::new_term(TerminatorEnum::YieldReturn).to_result();
        }
        if ctx.generator_data.is_some() {
            if ctx
                .generator_data
                .as_ref()
                .unwrap()
                .borrow()
                .generator_type
                .is_async()
            {
                if self.value.is_none() {
                    return Err(self
                        .range
                        .new_err(ErrorCode::GENERATOR_FUNCTION_CANNOT_RETURN_VOID)
                        .add_to_ctx(ctx));
                }
                let ret_node = self.value.as_mut().unwrap();

                let v = ret_node.emit(ctx, builder)?.get_value().unwrap();
                let value_pltype = v.get_ty();
                let v_tp = if let PLType::Union(u) = &*ret_pltype.borrow() {
                    u.sum_types[0].get_type(ctx, builder, false)?
                } else {
                    Arc::new(RefCell::new(PLType::Unknown))
                };
                if get_type_deep(v_tp.clone()) != get_type_deep(value_pltype.clone()) {
                    let err = self
                        .range
                        .new_err(ErrorCode::RETURN_TYPE_MISMATCH)
                        .add_label(
                            self.range.start_point(),
                            ctx.get_file(),
                            format_label!("expected type {}", v_tp.borrow().get_name()),
                        )
                        .add_label(
                            ret_node.range().start_point(),
                            ctx.get_file(),
                            format_label!("found type {}", value_pltype.borrow().get_name()),
                        )
                        .add_to_ctx(ctx);
                    return Err(err);
                }

                builder.await_ret(ctx, v.get_value());
                return NodeOutput::new_term(TerminatorEnum::Return).to_result();
            }
            return Err(self
                .range
                .new_err(ErrorCode::INVALID_RET_IN_GENERATOR_FUNCTION)
                .add_label(
                    self.range.start_point(),
                    ctx.get_file(),
                    format_label!("add keyword {} here", "yield"),
                )
                .add_to_ctx(ctx));
        }

        if let Some(ret_node) = &mut self.value {
            // TODO implicit cast && type infer
            let v = ret_node.emit(ctx, builder)?.get_value().unwrap();
            ctx.emit_comment_highlight(&self.comments[0]);
            let value_pltype = v.get_ty();
            let eqres = ctx.eq(ret_pltype.clone(), value_pltype.clone());
            if !eqres.eq {
                let err = ctx.add_diag(self.range.new_err(ErrorCode::RETURN_TYPE_MISMATCH));
                return Err(err);
            }
            let value = if eqres.need_up_cast {
                let ptr2v = builder.alloc("tmp_up_cast_ptr", &value_pltype.borrow(), ctx, None);
                let mut value =
                    ctx.try_load2var(self.range, v.get_value(), builder, &v.get_ty().borrow())?;
                builder.build_store(ptr2v, value);
                value = ctx.up_cast(
                    ret_pltype.clone(),
                    value_pltype.clone(),
                    ret_node.range(),
                    ret_node.range(),
                    ptr2v,
                    builder,
                )?;
                ctx.try_load2var(self.range, value, builder, &ret_pltype.borrow())?
            } else {
                // if builder.is_ptr(v.get_value()) && !builder.is_main(ctx.function.unwrap()) {
                //     builder.build_return(Some(v.get_value()));
                //     return NodeOutput::new_term(TerminatorEnum::Return).to_result();
                // }
                ctx.try_load2var(self.range, v.get_value(), builder, &v.get_ty().borrow())?
            };
            if ctx.return_block.unwrap().1.is_none() {
                return Err(self
                    .range()
                    .new_err(ErrorCode::NO_RETURN_VALUE_EXPECTED_IN_VOID_FUNCTION)
                    .add_to_ctx(ctx));
            }
            if matches!(
                &*get_type_deep(ctx.rettp.clone().unwrap()).borrow(),
                PLType::Primitive(_) | PLType::Pointer(_)
            ) {
                builder.build_return(Some(value));
                return NodeOutput::new_term(TerminatorEnum::Return).to_result();
            }
            builder.build_store(ctx.return_block.unwrap().1.unwrap(), value);
        } else if *ret_pltype.borrow() != PLType::Void {
            ctx.emit_comment_highlight(&self.comments[0]);
            return Err(ctx.add_diag(
                self.range
                    .new_err(ErrorCode::NO_RETURN_VALUE_IN_NON_VOID_FUNCTION),
            ));
        }
        builder.build_unconditional_branch(ctx.return_block.unwrap().0);
        builder.position_at(
            builder
                .get_first_instruction(ctx.return_block.unwrap().0)
                .unwrap(),
        );
        NodeOutput::new_term(TerminatorEnum::Return).to_result()
    }
}
