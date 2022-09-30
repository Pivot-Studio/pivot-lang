use super::*;
use crate::ast::ctx::Ctx;
use inkwell::types::AnyTypeEnum;
use internal_macro::range;

#[range]
pub struct TypeNameNode {
    pub id: String,
}

pub trait TypeNode: Node {
    fn get_type<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> AnyTypeEnum<'ctx>;
}

impl Node for TypeNameNode {
    fn print(&self) {
        println!("TypeNameNode:");
        println!("{}", self.id)
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        todo!()
    }
}

impl TypeNode for TypeNameNode {
    fn get_type<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> AnyTypeEnum<'ctx> {
        todo!()
    }
}

#[range]
pub struct TypedIdentifierNode {
    pub id: String,
    pub tp: Box<dyn TypeNode>,
}

impl Node for TypedIdentifierNode {
    fn print(&self) {
        println!("TypedIdentifierNode:");
        println!("id: {}", self.id);
        self.tp.print();
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        todo!()
    }
}

#[range]
pub struct StructDefNode {
    pub id: String,
    pub fields: Vec<Box<TypedIdentifierNode>>,
}

impl Node for StructDefNode {
    fn print(&self) {
        println!("StructDefNode:");
        println!("id: {}", self.id);
        for field in &self.fields {
            field.print();
        }
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        todo!()
    }
}

impl TypeNode for StructDefNode {
    fn get_type<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> AnyTypeEnum<'ctx> {
        todo!()
    }
}
