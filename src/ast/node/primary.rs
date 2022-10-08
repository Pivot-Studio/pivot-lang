use super::*;
use crate::{
    ast::{ctx::Ctx, error::ErrorCode},
    lsp::diagnostics::send_completions,
};

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
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        Ok((
            Value::BoolValue(ctx.context.bool_type().const_int(self.value as u64, true)),
            Some("bool".to_string()),
        ))
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
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        if let Num::INT(x) = self.value {
            let b = ctx.context.i64_type().const_int(x, true);
            return Ok((Value::IntValue(b), Some("i64".to_string())));
        } else if let Num::FLOAT(x) = self.value {
            let b = ctx.context.f64_type().const_float(x);
            return Ok((Value::FloatValue(b), Some("f64".to_string())));
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
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let v = ctx.get_symbol(&self.name);
        if let Some((v, pltype)) = v {
            return Ok((Value::VarValue(*v), Some(pltype)));
        }
        ctx.if_completion(|ctx, a| {
            let completions = ctx.get_completions();
            send_completions(ctx.sender.unwrap(), a.1.clone(), completions);
        });
        Err(ctx.add_err(self.range, ErrorCode::VAR_NOT_FOUND))
    }
}
