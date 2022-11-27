use super::primary::VarNode;
use super::*;
use crate::ast::ctx::Ctx;
use internal_macro::range;

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TraitDefNode {
    pub id: Box<VarNode>,
    pub generics: Option<Box<GenericDefNode>>,
    pub methods: Vec<FuncDefNode>,
}

impl Node for TraitDefNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        todo!()
    }

    fn print(&self, tabs: usize, end: bool, line: Vec<bool>) {
        todo!()
    }

    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        todo!()
    }
}
