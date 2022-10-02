use super::{
    types::{TypeNode, TypedIdentifierNode},
    Node,
};
use internal_macro::range;
use crate::utils::tabs;

use string_builder::Builder;

#[range]
pub struct FuncDefNode {
    pub id: String,
    pub paralist: Option<Vec<Box<TypedIdentifierNode>>>,
    pub ret: Box<dyn TypeNode>,
    pub body: Box<dyn Node>,
}

impl Node for FuncDefNode {
    fn string(&self, tabs: usize) -> String {
        let mut builder = Builder::default();
        tabs::print_tabs(&mut builder, tabs);
        builder.append("(FuncDefNode");
        builder.append(format!("id: {}", self.id));
        if let Some(paralist) = &self.paralist {
            for para in paralist {
                builder.append(para.string(tabs + 1));
            }
        }
        builder.append(self.ret.string(tabs + 1));
        builder.append(self.body.string(tabs + 1));
        tabs::print_tabs(&mut builder, tabs);
        builder.append(")");
        builder.string().unwrap()
    }
    fn emit<'a, 'ctx>(
        &'a mut self,
        ctx: &mut crate::ast::ctx::Ctx<'a, 'ctx>,
    ) -> super::Value<'ctx> {
        todo!()
    }
}
