use super::primary::*;
use super::*;
use crate::ast::ctx::Ctx;
use crate::utils::tabs;
use string_builder::Builder;

use internal_macro::range;

#[range]
pub struct DefNode {
    pub var: VarNode,
    pub exp: Box<dyn Node>,
}
impl Node for DefNode {
    fn string(&self, tabs: usize) -> String {
        let mut builder = Builder::default();
        tabs::print_tabs(&mut builder, tabs);
        builder.append("(DefNode");
        builder.append(self.var.string(tabs + 1));
        builder.append(self.exp.string(tabs + 1));
        tabs::print_tabs(&mut builder, tabs);
        builder.append(")");
        builder.string().unwrap()
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
    fn string(&self, tabs: usize) -> String {
        let mut builder = Builder::default();
        tabs::print_tabs(&mut builder, tabs);
        builder.append("(AssignNode");
        builder.append(self.var.string(tabs + 1));
        builder.append(self.exp.string(tabs + 1));
        tabs::print_tabs(&mut builder, tabs);
        builder.append(")");
        builder.string().unwrap()
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
    fn string(&self, tabs: usize) -> String {
        let mut builder = Builder::default();
        tabs::print_tabs(&mut builder, tabs);
        builder.append("(NLNode");
        tabs::print_tabs(&mut builder, tabs);
        builder.append(")");
        builder.string().unwrap()
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
    fn string(&self, tabs: usize) -> String {
        let mut builder = Builder::default();
        tabs::print_tabs(&mut builder, tabs);
        builder.append("(StatementsNode");
        for statement in &self.statements {
            builder.append(statement.string(tabs + 1));
        }
        tabs::print_tabs(&mut builder, tabs);
        builder.append(")");
        builder.string().unwrap()
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        let child = &mut ctx.new_child();
        for m in self.statements.iter_mut() {
            m.emit(child);
        }
        Value::None
    }
}
