use super::*;
use crate::ast::builder::llvmbuilder::LLVMBuilder;use crate::ast::builder::IRBuilder;
use crate::ast::{ctx::Ctx, diag::ErrorCode};
use internal_macro::{comments, fmt, range};

#[range]
#[fmt]
#[comments]
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
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b LLVMBuilder<'a, 'ctx>,
    ) -> NodeResult {
        let rettp = ctx.rettp.clone();
        if let Some(ret) = &mut self.value {
            if rettp.is_none() {
                let err = ctx.add_err(self.range, ErrorCode::RETURN_VALUE_IN_VOID_FUNCTION);
                return Err(err);
            }
            let (ret, tp, _) = ret.emit(ctx, builder)?;
            let (ret, v) = ctx.try_load2var(self.range, ret.unwrap(), tp.unwrap(), builder)?;
            if v != rettp.unwrap() {
                let err = ctx.add_err(self.range, ErrorCode::RETURN_TYPE_MISMATCH);
                return Err(err);
            }

            builder.build_store(ctx.return_block.unwrap().1.unwrap(), ret);
            builder.build_unconditional_branch(ctx.return_block.unwrap().0);
        } else {
            if rettp.is_some() && &*rettp.clone().unwrap().borrow() != &PLType::VOID {
                let _s = format!("return type is {:?}", &*rettp.unwrap().borrow());
                let err = ctx.add_err(self.range, ErrorCode::NO_RETURN_VALUE_IN_NON_VOID_FUNCTION);
                return Err(err);
            }
            builder.build_unconditional_branch(ctx.return_block.unwrap().0);
        }
        let ret = ctx.return_block.unwrap().0;
        let inst = builder.get_first_instruction(ret).unwrap();
        builder.position_at(inst);
        Ok((None, None, TerminatorEnum::RETURN))
    }
}
