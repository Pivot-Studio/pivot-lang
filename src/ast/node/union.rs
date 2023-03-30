use internal_macro::node;

use crate::ast::{range::Range, tokens::TokenType};

use super::{primary::VarNode, types::GenericDefNode, Node, PrintTrait, TypeNodeEnum};

#[node]
pub struct UnionDefNode {
    pub modifier: Option<(TokenType, Range)>,
    pub name: VarNode,
    pub generics: Option<Box<GenericDefNode>>,
    pub sum_types: Vec<Box<TypeNodeEnum>>,
}

impl Node for UnionDefNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut crate::ast::ctx::Ctx<'a>,
        builder: &'b crate::ast::builder::BuilderEnum<'a, 'ctx>,
    ) -> super::NodeResult {
        todo!()
    }
}

impl PrintTrait for UnionDefNode {
    fn print(&self, tabs: usize, end: bool, line: Vec<bool>) {
        todo!()
    }
}
