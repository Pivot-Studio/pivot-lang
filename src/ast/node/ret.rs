use super::*;
use crate::ast::{ctx::Ctx, diag::ErrorCode};
use internal_macro::range;

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct RetNode {
    pub value: Option<Box<NodeEnum>>,
}

impl Node for RetNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        let mut format_res = String::new();
        if let Some(value) = &self.value {
            format_res.push_str("return ");
            format_res.push_str(&value.format(tabs, prefix));
        } else {
            format_res.push_str("return");
        }
        return format_res;
    }

    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("RetNode");
        if let Some(value) = &self.value {
            value.print(tabs + 1, true, line.clone());
        }
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let rettp = ctx.function.unwrap().get_type().get_return_type();
        if let Some(ret) = &mut self.value {
            if rettp.is_none() {
                let err = ctx.add_err(self.range, ErrorCode::RETURN_VALUE_IN_VOID_FUNCTION);
                return Err(err);
            }
            let (ret, _, _) = ret.emit(ctx)?;
            let ret = ctx.try_load2var(self.range, ret.unwrap())?;
            if ret.as_basic_value_enum().get_type() != rettp.unwrap() {
                let err = ctx.add_err(self.range, ErrorCode::RETURN_TYPE_MISMATCH);
                return Err(err);
            }

            ctx.builder.build_store(
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
        let ret = ctx.return_block.unwrap().0;
        let inst = ret.get_first_instruction().unwrap();
        ctx.builder.position_at(ret, &inst);
        Ok((None, None, TerminatorEnum::RETURN))
    }
}
