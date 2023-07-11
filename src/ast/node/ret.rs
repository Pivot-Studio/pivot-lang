use super::node_result::TerminatorEnum;
use super::*;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::ast::tokens::TokenType;
use crate::ast::{ctx::Ctx, diag::ErrorCode};
use internal_macro::node;

#[node(comment)]
pub struct RetNode {
    pub value: Option<Box<NodeEnum>>,
    pub yiel:Option<(TokenType,Range)>
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
        if let Some(ret_node) = &mut self.value {
            // TODO implicit cast && type infer
            let v = ret_node.emit(ctx, builder)?.get_value().unwrap();
            ctx.emit_comment_highlight(&self.comments[0]);
            let value_pltype = v.get_ty();
            let mut value = ctx.try_load2var(self.range, v.get_value(), builder)?;
            let eqres = ctx.eq(ret_pltype.clone(), value_pltype.clone());
            if !eqres.eq {
                let err = ctx.add_diag(self.range.new_err(ErrorCode::RETURN_TYPE_MISMATCH));
                return Err(err);
            }
            if eqres.need_up_cast {
                let ptr2v = builder.alloc("tmp_up_cast_ptr", &value_pltype.borrow(), ctx, None);
                builder.build_store(ptr2v, value);
                value = ctx.up_cast(
                    ret_pltype,
                    value_pltype.clone(),
                    ret_node.range(),
                    ret_node.range(),
                    ptr2v,
                    builder,
                )?;
                value = ctx.try_load2var(self.range, value, builder)?;
            }
            if ctx.return_block.unwrap().1.is_none() {
                return Err(self
                    .range()
                    .new_err(ErrorCode::NO_RETURN_VALUE_EXPECTED_IN_VOID_FUNCTION)
                    .add_to_ctx(ctx));
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
