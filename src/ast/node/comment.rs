use super::*;
use crate::{ast::ctx::Ctx, utils::read_config::enter};
use internal_macro::{format, range};
use lsp_types::SemanticTokenType;
#[range]
#[format]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct CommentNode {
    pub comment: String,
    pub is_doc: bool,
}

impl Node for CommentNode {
    fn format(&self, builder: &mut FmtBuilder) {
        self.formatBuild(builder);
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("CommentNode: {}", self.comment);
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.push_semantic_token(self.range, SemanticTokenType::COMMENT, 0);
        Ok((None, None, TerminatorEnum::NONE))
    }
}
