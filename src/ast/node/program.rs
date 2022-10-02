use super::*;
use crate::ast::ctx::Ctx;
use crate::utils::tabs;
use string_builder::Builder;

use internal_macro::range;

#[range]
pub struct ProgramNode {
    pub statements: Vec<Box<dyn Node>>,
}
impl Node for ProgramNode {
    fn string(&self, tabs: usize) -> String {
        let mut builder = Builder::default();
        tabs::print_tabs(&mut builder, tabs);
        builder.append("(ProgramNode");
        for statement in &self.statements {
            builder.append(statement.string(tabs + 1));
        }
        tabs::print_tabs(&mut builder, tabs);
        builder.append(")");
        builder.string().unwrap()
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        for m in self.statements.iter_mut() {
            m.emit(ctx);
        }
        Value::None
    }
}
