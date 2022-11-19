use super::*;
use crate::{
    ast::{ctx::Ctx, diag::ErrorCode},
    utils::read_config::enter,
};

use colored::Colorize;
use internal_macro::range;

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ErrorNode {
    pub msg: String,
    pub src: String,
    pub code: ErrorCode,
}

impl Node for ErrorNode {
    fn format(&self, _tabs: usize, _prefix: &str) -> String {
        let mut format_res = String::new();
        format_res.push_str(enter());
        format_res.push_str(&self.src);
        format_res.push_str(enter());
        format_res
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ErrorNode: {}", self.msg.red());
        tab(tabs + 1, line, true);
        println!("Src: {}", format!("{:?}", self.src).red());
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let err = ctx.add_err(self.range, self.code);
        ctx.if_completion(|ctx, a| {
            if a.0.is_in(self.range) {
                let completions = ctx.get_completions();
                ctx.completion_items.set(completions);
            }
        });

        Err(err)
    }
}

/// # STErrorNode
/// 表现一个因为缺少分号而错误的statement
#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct STErrorNode {
    pub err: ErrorNode,
    pub st: Box<NodeEnum>,
}

impl Node for STErrorNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        return self.st.format(tabs, prefix);
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("STErrorNode");
        self.st.print(tabs + 1, false, line.clone());
        self.err.print(tabs + 1, true, line);
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        _ = self.st.emit(ctx);
        self.err.emit(ctx)
    }
}
