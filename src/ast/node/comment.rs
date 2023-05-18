use super::*;
use crate::ast::ctx::Ctx;
use internal_macro::node;
use lsp_types::SemanticTokenType;

#[node]
pub struct CommentNode {
    pub comment: String,
    pub is_doc: bool, // use "///" (is_doc:true)
}

impl PrintTrait for CommentNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("CommentNode: {}", self.comment);
    }
}

impl Node for CommentNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        _builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        ctx.push_semantic_token(self.range, SemanticTokenType::COMMENT, 0);
        Ok(Default::default())
    }
}
