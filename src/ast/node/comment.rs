use super::*;
use crate::ast::ctx::Ctx;
use internal_macro::{fmt, range};
use lsp_types::SemanticTokenType;
#[range]
#[fmt]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct CommentNode {
    pub comment: String,
    pub is_doc: bool, // use "///" (is_doc:true)
}

impl Node for CommentNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("CommentNode: {}", self.comment);
    }
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        _builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        ctx.push_semantic_token(self.range, SemanticTokenType::COMMENT, 0);
        Ok((None, None, TerminatorEnum::NONE))
    }
}
