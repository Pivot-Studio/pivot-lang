use internal_macro::node;

use crate::ast::{builder::BuilderEnum, ctx::Ctx};

use super::{Node, NodeEnum, NodeResult, PrintTrait, TypeNodeEnum};

#[node]
pub struct AsNode {
    pub expr: Box<NodeEnum>,
    pub ty: Box<TypeNodeEnum>,
}

impl Node for AsNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        todo!()
    }
}

impl PrintTrait for AsNode {
    fn print(&self, tabs: usize, end: bool, line: Vec<bool>) {
        todo!()
    }
}
