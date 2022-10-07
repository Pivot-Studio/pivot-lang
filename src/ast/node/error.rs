use super::*;
use crate::ast::{ctx::Ctx, error::ErrorCode};
use colored::Colorize;
use internal_macro::range;

#[range]
pub struct ErrorNode {
    pub msg: String,
    pub src: String,
    pub code: ErrorCode,
}

impl Node for ErrorNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ErrorNode: {}", self.msg.red());
        tab(tabs + 1, line, true);
        println!("Src: {}", format!("{:?}", self.src).red());
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> (Value<'ctx>, Option<String>) {
        let err = ctx.add_err(self.range, self.code);
        (Value::Err(err), None)
    }
}
