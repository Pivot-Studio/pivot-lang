use super::*;
use crate::ast::ctx::Ctx;
use internal_macro::node;
use lsp_types::SemanticTokenType;
lazy_static::lazy_static! {
    pub static ref RODEO: lasso::ThreadedRodeo = lasso::ThreadedRodeo::default();
}

#[node]
pub struct CommentNode {
    pub comment_key: lasso::Spur,
    pub is_doc: bool, // use "///" (is_doc:true)
}

impl CommentNode {
    pub fn get_comment(&self) -> String {
        RODEO.resolve(&self.comment_key).to_string()
    }
}

impl PrintTrait for CommentNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("CommentNode: {}", self.get_comment());
    }
}

impl Node for CommentNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        _builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        ctx.push_semantic_token(self.range, SemanticTokenType::COMMENT, 0);
        Ok(Default::default())
    }
}
