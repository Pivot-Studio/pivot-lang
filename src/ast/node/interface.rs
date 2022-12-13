use super::primary::VarNode;
use super::*;
use crate::ast::ctx::Ctx;
use internal_macro::{range, fmt};

#[range]
#[fmt]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TraitDefNode {
    pub id: Box<VarNode>,
    pub generics: Option<Box<GenericDefNode>>,
    pub methods: Vec<FuncDefNode>,
}

impl Node for TraitDefNode {
    fn print(&self,tabs:usize,end:bool,line:Vec<bool>) {
        todo!()
    }

    fn emit<'a,'ctx,'b>(&mut self,ctx: &'b mut Ctx<'a>,builder: &'b BuilderEnum<'a,'ctx>,) -> NodeResult {
        todo!()
    }
}
