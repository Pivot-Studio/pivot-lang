use std::sync::Arc;

use super::*;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::{
    ast::{ctx::Ctx, diag::ErrorCode},
    plv,
};
use internal_macro::{comments, fmt, range};

#[range]
#[comments]
#[fmt]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct PointerOpNode {
    pub value: Box<NodeEnum>,
    pub op: PointerOpEnum,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum PointerOpEnum {
    DEREF,
    ADDR,
}

impl PrintTrait for PointerOpNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("PointerOpNode");
        self.value.print(tabs + 1, true, line.clone());
    }
}

impl Node for PointerOpNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        let (value, mut tp, _) = self.value.emit(ctx, builder)?;
        let value = value.unwrap();
        let value = match self.op {
            PointerOpEnum::DEREF => {
                if tp.is_none() {
                    return Err(ctx.add_diag(self.range.new_err(ErrorCode::NOT_A_POINTER)));
                }
                if let PLType::POINTER(tp1) = &*tp.unwrap().borrow() {
                    tp = Some(tp1.clone());
                    builder.build_load(value.value, "deref")
                } else {
                    return Err(ctx.add_diag(self.range.new_err(ErrorCode::NOT_A_POINTER)));
                }
            }
            PointerOpEnum::ADDR => {
                let old_tp = tp.clone().unwrap();
                tp = Some(Arc::new(RefCell::new(PLType::POINTER(tp.unwrap()))));
                if value.is_const {
                    return Err(ctx.add_diag(self.range.new_err(ErrorCode::CAN_NOT_REF_CONSTANT)));
                }
                let val = value.value;
                let v = builder.alloc("addr", &tp.clone().unwrap().borrow(), ctx);
                builder.build_store(v, builder.mv2heap(val, ctx, &old_tp.borrow()));
                v.into()
            }
        };
        Ok((Some(plv!(value)), tp, TerminatorEnum::NONE))
    }
}
