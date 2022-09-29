use super::primary::*;
use super::*;
use crate::ast::ctx::Ctx;

use internal_macro::range;

#[range]
pub struct DefNode {
    pub var: VarNode,
    pub exp: Box<dyn Node>,
}
impl Node for DefNode {
    fn print(&self) {
        println!("DefNode:");
        self.var.print();
        println!("=");
        self.exp.print();
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        let v = self.exp.emit(ctx);
        let e = v.as_basic_value_enum();
        let tp = e.get_type();
        let p = alloc(ctx, tp, &self.var.name);
        ctx.builder.build_store(p, e);

        ctx.add_symbol(self.var.name.clone(), p);
        Value::None
    }
}
#[range]
pub struct AssignNode {
    pub var: VarNode,
    pub exp: Box<dyn Node>,
}
impl Node for AssignNode {
    fn print(&self) {
        println!("AssignNode:");
        self.var.print();
        println!("=");
        self.exp.print();
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        let pt = self.var.emit(ctx);
        let value = self.exp.emit(ctx);
        if let Value::VarValue(ptr) = pt {
            let e = value.as_basic_value_enum();
            ctx.builder.build_store(ptr, e);
            return Value::None;
        }

        todo!()
    }
}

#[range]
pub struct NLNode {}

impl Node for NLNode {
    fn print(&self) {
        println!("NLNode");
    }

    fn emit<'a, 'ctx>(&'a mut self, _: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        Value::None
    }
}

#[range]
pub struct StatementsNode {
    pub statements: Vec<Box<dyn Node>>,
}
impl Node for StatementsNode {
    fn print(&self) {
        println!("StatementsNode:");
        for e in self.statements.iter() {
            e.print();
        }
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        let child = &mut ctx.new_child();
        for m in self.statements.iter_mut() {
            m.emit(child);
        }
        Value::None
    }
}
