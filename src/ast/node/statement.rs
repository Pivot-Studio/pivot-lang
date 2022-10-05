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
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("DefNode");
        self.var.print(tabs + 1, false, line.clone());
        self.exp.print(tabs + 1, true, line.clone());
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        let v = self.exp.emit(ctx);
        let e = ctx.try_load(v).as_basic_value_enum();
        let tp = e.get_type();
        let p = alloc(ctx, tp, &self.var.name);
        ctx.builder.build_store(p, e);

        ctx.add_symbol(self.var.name.clone(), p);
        Value::None
    }
}
#[range]
pub struct AssignNode {
    pub var: Box<dyn Node>,
    pub exp: Box<dyn Node>,
}
impl Node for AssignNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("AssignNode");
        self.var.print(tabs + 1, false, line.clone());
        self.exp.print(tabs + 1, true, line.clone());
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        let pt = self.var.emit(ctx);
        let value = self.exp.emit(ctx);
        if let Value::VarValue(ptr) = pt {
            let load = ctx.try_load(value);
            ctx.builder.build_store(ptr, load.as_basic_value_enum());
            return Value::None;
        }
        todo!()
    }
}

#[range]
pub struct NLNode {}

impl Node for NLNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
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
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StatementsNode");
        let mut i = self.statements.len();
        for statement in &self.statements {
            i -= 1;
            statement.print(tabs + 1, i == 0, line.clone());
        }
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        let child = &mut ctx.new_child(self.range.start);
        for m in self.statements.iter_mut() {
            let pos = m.range().start;
            child.build_dbg_location(pos);
            m.emit(child);
        }
        Value::None
    }
}

impl StatementsNode {
    pub fn emit_child<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        let child = ctx;
        for m in self.statements.iter_mut() {
            let pos = m.range().start;
            child.build_dbg_location(pos);
            m.emit(child);
        }
        Value::None
    }
}
