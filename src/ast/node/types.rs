use std::collections::HashMap;

use super::*;
use crate::ast::ctx::{Ctx, PLType, STType};
use internal_macro::range;

#[range]
pub struct TypeNameNode {
    pub id: String,
}

pub trait TypeNode: Node {
    fn get_type<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Option<PLType<'ctx>>;
}

impl Node for TypeNameNode {
    fn print(&self) {
        println!("TypeNameNode:");
        println!("{}", self.id)
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        return Value::TypeValue(self.get_type(ctx).unwrap());
    }
}

impl TypeNode for TypeNameNode {
    fn get_type<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Option<PLType<'ctx>> {
        let tp = ctx.get_type(self.id.as_str());
        if let Some(tp) = tp {
            return Some(tp.clone());
        } else {
            return None;
        }
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
        return Value::TypeValue(self.get_type(ctx).unwrap().1);
    }
}

impl TypedIdentifierNode {
    fn get_type<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Option<(&str, PLType<'ctx>)> {
        let tp = self.tp.get_type(ctx)?;
        Some((self.id.as_str(), tp))
    }
}

impl TypedIdentifierNode {
    pub fn get_id(&self) -> &str {
        self.id.as_str()
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
        _ = self.gen_type(ctx);
        Value::None
    }
}

impl StructDefNode {
    pub fn gen_type<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Result<PLType, &str> {
        let mut fields = HashMap::<&str, PLType<'ctx>>::new();
        for field in self.fields.iter_mut() {
            if let Some((id, tp)) = field.get_type(ctx) {
                fields.insert(id, tp);
            } else {
                return Err("type not found");
            }
        }
        let name = self.id.as_str();
        let st = ctx.context.opaque_struct_type(name);
        let newf = fields.clone();
        st.set_body(
            &fields
                .into_iter()
                .map(|(_, v)| v.get_basic_type())
                .collect::<Vec<_>>(),
            false,
        );
        Ok(PLType::STRUCT(STType {
            name,
            struct_type: st,
            fields: newf,
        }))
    }
}
