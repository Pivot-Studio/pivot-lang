use super::*;
use crate::{
    ast::{ctx::Ctx, error::ErrorCode},
    lsp::helpers::send_completions,
};
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
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let err = ctx.add_err(self.range, self.code);
        ctx.if_completion(|ctx, a| {
            if a.0.line >= self.range.start.line && a.0.line <= self.range.end.line {
                let completions = ctx.get_completions();
                send_completions(ctx.sender.unwrap(), a.1.clone(), completions);
            }
        });

        Err(err)
    }
}
