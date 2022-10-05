use super::*;
use crate::ast::ctx::Ctx;

use internal_macro::range;

#[range]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct BoolConstNode {
    pub value: bool,
}

impl Node for BoolConstNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line, end);
        println!("BoolConstNode: {}", self.value);
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> (Value<'ctx>, Option<String>) {
        (
            Value::BoolValue(ctx.context.bool_type().const_int(self.value as u64, true)),
            Some("bool".to_string()),
        )
    }
}

#[range]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct NumNode {
    pub value: Num,
}
impl Node for NumNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line, end);
        println!("NumNode: {:?}", self.value);
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> (Value<'ctx>, Option<String>) {
        if let Num::INT(x) = self.value {
            let b = ctx.context.i64_type().const_int(x, true);
            return (Value::IntValue(b), Some("i64".to_string()));
        } else if let Num::FLOAT(x) = self.value {
            let b = ctx.context.f64_type().const_float(x);
            return (Value::FloatValue(b), Some("f64".to_string()));
        }
        panic!("not implemented")
    }
}

#[range]
#[derive(Debug, PartialEq, Clone)]
pub struct VarNode {
    pub name: String,
}
impl Node for VarNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("VarNode: {}", self.name);
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> (Value<'ctx>, Option<String>) {
        let v = ctx.get_symbol(&self.name);
        if let Some((v, pltype)) = v {
            return (Value::VarValue(*v), Some(pltype));
        }
        todo!(); // TODO: 未定义的变量
    }
}
