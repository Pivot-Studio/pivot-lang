use internal_macro::range;
use lsp_types::SemanticTokenType;

use crate::ast::{
    ctx::Ctx,
    node::{deal_line, tab},
};

use super::{primary::VarNode, Node, NodeResult, TerminatorEnum, Value};

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct UseNode {
    pub ids: Vec<Box<VarNode>>,
    /// 是否完整
    /// use a::b 完整
    /// use a::b:: 不完整
    pub complete: bool,
}

impl Node for UseNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("UseNode");
        let mut i = self.ids.len();
        for id in &self.ids {
            i -= 1;
            id.print(tabs + 1, i == 0, line.clone());
        }
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        for id in &self.ids {
            ctx.push_semantic_token(id.range, SemanticTokenType::NAMESPACE, 0);
        }
        Ok((Value::None, None, TerminatorEnum::NONE))
    }
}
