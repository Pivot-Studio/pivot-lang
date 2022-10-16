use super::*;
use crate::ast::{ctx::Ctx, diag::ErrorCode};
use internal_macro::range;

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct RetNode {
    pub value: Option<Box<NodeEnum>>,
}

impl Node for RetNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("RetNode");
        if let Some(value) = &self.value {
            value.print(tabs + 1, true, line.clone());
        }
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let rettp = ctx.function.unwrap().get_type().get_return_type();
        if let Some(ret) = &mut self.value {
            if rettp.is_none() {
                let err = ctx.add_err(self.range, ErrorCode::RETURN_VALUE_IN_VOID_FUNCTION);
                return Err(err);
            }
            let (ret, _, _) = ret.emit(ctx)?;
            let ret = if rettp.unwrap().is_pointer_type() {
                ctx.try_load2ptr(ret)
            } else {
                ctx.try_load2var(ret)
            };
            if ret.as_basic_value_enum().get_type() != rettp.unwrap() {
                let err = ctx.add_err(self.range, ErrorCode::RETURN_TYPE_MISMATCH);
                return Err(err);
            }
            ctx.nodebug_builder.build_store(
                ctx.return_block.unwrap().1.unwrap(),
                ret.as_basic_value_enum(),
            );
            ctx.builder
                .build_unconditional_branch(ctx.return_block.unwrap().0);
        } else {
            if rettp.is_some() {
                let err = ctx.add_err(self.range, ErrorCode::NO_RETURN_VALUE_IN_NON_VOID_FUNCTION);
                return Err(err);
            }
            ctx.builder
                .build_unconditional_branch(ctx.return_block.unwrap().0);
        }
        Ok((Value::None, None, TerminatorEnum::RETURN))
    }
}
