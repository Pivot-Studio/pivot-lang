use super::*;
use crate::ast::ctx::Ctx;
use internal_macro::range;
use lsp_types::SemanticTokenType;
#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct CommentNode {
    pub comment: String,
}

impl Node for CommentNode {
    fn format(&self, _tabs: usize, _prefix: &str) -> String {
        let mut format_res = String::from("//");
        format_res.push_str(&self.comment);
        format_res
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("CommentNode: {}", self.comment);
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.push_semantic_token(self.range, SemanticTokenType::COMMENT, 0);
        Ok((Value::None, None, TerminatorEnum::NONE, false))
    }
}
