use std::sync::Arc;

use super::node_result::NodeResultBuilder;
use super::*;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::ast::{ctx::Ctx, diag::ErrorCode};
use internal_macro::node;

#[node(comment)]
pub struct PointerOpNode {
    pub value: Box<NodeEnum>,
    pub op: PointerOpEnum,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum PointerOpEnum {
    Deref,
    Addr,
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
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        let v = self.value.emit(ctx, builder)?.get_value();
        let v = v.unwrap();
        let value = v.get_value();
        let mut tp = v.get_ty();
        let btp = tp.clone();
        let value = match self.op {
            PointerOpEnum::Deref => {
                if let PLType::Pointer(tp1) = &*btp.borrow() {
                    tp = tp1.clone();
                    builder.build_load(value, "deref")
                } else {
                    return Err(ctx.add_diag(self.range.new_err(ErrorCode::NOT_A_POINTER)));
                }
            }
            PointerOpEnum::Addr => {
                // let old_tp = tp.clone().unwrap();
                tp = Arc::new(RefCell::new(PLType::Pointer(tp)));
                if v.is_const() {
                    return Err(ctx.add_diag(self.range.new_err(ErrorCode::CAN_NOT_REF_CONSTANT)));
                }
                let val = value;
                let v = builder.alloc("addr", &tp.borrow(), ctx, None);
                builder.build_store(v, val);
                v
            }
        };
        value.new_output(tp).to_result()
    }
}
