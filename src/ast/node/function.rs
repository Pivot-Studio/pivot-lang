use super::{
    types::{TypeNode, TypedIdentifierNode},
    Node,
};
use internal_macro::range;

#[range]
pub struct FuncDefNode {
    pub id: String,
    pub paralist: Option<Vec<Box<TypedIdentifierNode>>>,
    pub ret: Box<dyn TypeNode>,
    pub body: Box<dyn Node>,
}

impl Node for FuncDefNode {
    fn print(&self) {
        println!("FuncDefNode:");
        println!("id: {}", self.id);
        if let Some(paralist) = &self.paralist {
            for para in paralist {
                para.print();
            }
        }
        self.ret.print();
        self.body.print();
    }
    fn emit<'a, 'ctx>(
        &'a mut self,
        ctx: &mut crate::ast::ctx::Ctx<'a, 'ctx>,
    ) -> super::Value<'ctx> {
        todo!()
    }
}
