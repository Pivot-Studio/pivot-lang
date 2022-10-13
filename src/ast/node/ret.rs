use super::*;
use crate::ast::{ctx::Ctx, error::ErrorCode};
use internal_macro::range;

#[range]
pub struct RetNode {
    pub value: Option<Box<dyn Node>>,
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
            let (ret, _) = ret.emit(ctx)?;
            let ret = if rettp.unwrap().is_pointer_type() {
                ctx.try_load1(ret)
            } else {
                ctx.try_load2(ret)
            };
            if ret.as_basic_value_enum().get_type() != rettp.unwrap() {
                let err = ctx.add_err(self.range, ErrorCode::RETURN_TYPE_MISMATCH);
                return Err(err);
            }
            ctx.builder.build_return(Some(&ret.as_basic_value_enum()));
        } else {
            if rettp.is_some() {
                let err = ctx.add_err(self.range, ErrorCode::NO_RETURN_VALUE_IN_NON_VOID_FUNCTION);
                return Err(err);
            }
            ctx.builder.build_return(None);
        }
        position_at_end(
            ctx,
            ctx.context
                .append_basic_block(ctx.function.unwrap(), "retdead"),
        );
        Ok((Value::None, None))
    }
}
