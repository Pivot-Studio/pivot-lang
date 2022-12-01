use super::*;
use crate::ast::{ctx::Ctx, diag::ErrorCode};
use internal_macro::{range, comments};

#[range]
#[comments]
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

impl Node for PointerOpNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        let mut op = String::new();
        if self.op == PointerOpEnum::ADDR {
            op.push_str("&");
        } else {
            op.push_str("*");
        }
        format!("{}{}", op, self.value.format(tabs, prefix))
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("PointerOpNode");
        self.value.print(tabs + 1, true, line.clone());
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let (value, mut tp, _) = self.value.emit(ctx)?;
        let value = value.unwrap();
        let value = match self.op {
            PointerOpEnum::DEREF => {
                if tp.is_none() {
                    return Err(ctx.add_err(self.range, ErrorCode::NOT_A_POINTER));
                }
                if let PLType::POINTER(tp1) = &*tp.unwrap().borrow() {
                    tp = Some(tp1.clone());
                    ctx.builder.build_load(value.into_pointer_value(), "deref")
                } else {
                    return Err(ctx.add_err(self.range, ErrorCode::NOT_A_POINTER));
                }
            }
            PointerOpEnum::ADDR => {
                if tp.is_some() {
                    tp = Some(Rc::new(RefCell::new(PLType::POINTER(tp.unwrap()))));
                }
                if value.is_const {
                    return Err(ctx.add_err(self.range, ErrorCode::CAN_NOT_REF_CONSTANT));
                }
                let val: BasicValueEnum = value.value.try_into().unwrap();
                if !val.is_pointer_value() {
                    return Err(ctx.add_err(self.range, ErrorCode::CAN_NOT_REF_CONSTANT));
                }
                let v = alloc(ctx, val.get_type(), "addr");
                ctx.builder.build_store(v, ctx.mv2heap(val));
                v.into()
            }
        };
        Ok((Some(value.into()), tp, TerminatorEnum::NONE))
    }
}
