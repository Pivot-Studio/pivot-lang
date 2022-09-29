use super::*;
use crate::ast::ctx::Ctx;

use internal_macro::range;

#[range]
pub struct ProgramNode {
    pub statements: Vec<Box<dyn Node>>,
}
impl Node for ProgramNode {
    fn print(&self) {
        println!("ProgramNode:");
        for e in self.statements.iter() {
            e.print();
        }
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        for m in self.statements.iter_mut() {
            m.emit(ctx);
        }
        Value::None
    }
}
