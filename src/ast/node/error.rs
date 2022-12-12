use super::*;
use crate::ast::{ctx::Ctx, diag::ErrorCode};

use colored::Colorize;
use internal_macro::{fmt, range};

#[range]
#[fmt]
#[derive(Clone, PartialEq, Eq, Debug)]
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
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b LLVMBuilder<'a, 'ctx>,
    ) -> NodeResult {
        let err = ctx.add_err(self.range, self.code);
        ctx.if_completion(self.range, || ctx.get_completions());

        Err(err)
    }
}

/// # StErrorNode
/// 表现一个因为缺少分号而错误的statement
#[range]
#[fmt]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct StErrorNode {
    pub err: ErrorNode,
    pub st: Box<NodeEnum>,
}

impl Node for StErrorNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StErrorNode");
        self.st.print(tabs + 1, false, line.clone());
        self.err.print(tabs + 1, true, line);
    }
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b LLVMBuilder<'a, 'ctx>,
    ) -> NodeResult {
        _ = self.st.emit(ctx, builder);
        self.err.emit(ctx, builder)
    }
}
