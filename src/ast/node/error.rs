use super::*;
use crate::ast::ctx::Ctx;
use colored::Colorize;
use internal_macro::range;

#[range]
pub struct ErrorNode {
    pub msg: String,
    pub src: String,
}

impl Node for ErrorNode {
    fn format(&self, tabs: usize, prefix: &str) {
        println!("hello");
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ErrorNode: {}", self.msg.red());
        tab(tabs + 1, line, true);
        println!("Src: {}", format!("{:?}", self.src).red());
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> (Value<'ctx>, Option<String>) {
        let err = format!(
            "syntax error at {}\n{}\n{}",
            format!(
                "{}:{}:{}",
                ctx.src_file_path, self.range.start.line, self.range.start.column
            )
            .yellow(),
            format!("{}", self.src.red().underline()),
            format!("{}", self.msg.blue().bold()),
        );
        let err = PLErr::SyntaxError(err);
        ctx.add_err(err.clone());
        (Value::Err(err), None)
    }
}
