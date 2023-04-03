use internal_macro::node;

use crate::ast::{range::Range, tokens::TokenType, ctx::Ctx, builder::BuilderEnum};

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
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> super::NodeResult {
        todo!()
    }
}

impl PrintTrait for UnionDefNode {
    fn print(&self, tabs: usize, end: bool, line: Vec<bool>) {
        todo!()
    }
}

impl UnionDefNode {
    pub fn add_to_symbols<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) {

    }
}
