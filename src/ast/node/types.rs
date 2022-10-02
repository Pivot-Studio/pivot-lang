use std::collections::HashMap;

use super::*;
use crate::ast::ctx::{Ctx, Field, PLType, STType};
use crate::utils::tabs;
use inkwell::types::BasicType;
use internal_macro::range;

use string_builder::Builder;

#[range]
pub struct TypeNameNode {
    pub id: String,
}

pub trait TypeNode: Node {
    fn get_type<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Option<PLType<'ctx>>;
}

impl Node for TypeNameNode {
    fn string(&self, tabs: usize) -> String {
        let mut builder = Builder::default();
        tabs::print_tabs(&mut builder, tabs);
        builder.append("(TypeNameNode");
        builder.append(format!("id: {}", self.id));
        tabs::print_tabs(&mut builder, tabs);
        builder.append(")");
        builder.string().unwrap()
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        return Value::TypeValue(self.get_type(ctx).unwrap().get_basic_type());
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
    fn string(&self, tabs: usize) -> String {
        let mut builder = Builder::default();
        tabs::print_tabs(&mut builder, tabs);
        builder.append("(TypedIdentifierNode");
        builder.append(format!("id: {}", self.id));
        builder.append(self.tp.string(tabs + 1));
        tabs::print_tabs(&mut builder, tabs);
        builder.append(")");
        builder.string().unwrap()
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        return Value::TypeValue(self.get_type(ctx).unwrap().1.get_basic_type());
    }
}

impl TypedIdentifierNode {
    fn get_type<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Option<(&str, PLType<'ctx>)> {
        let tp = self.tp.get_type(ctx)?;
        Some((self.id.as_str(), tp))
    }
}

#[range]
pub struct StructDefNode {
    pub id: String,
    pub fields: Vec<Box<TypedIdentifierNode>>,
}

impl Node for StructDefNode {
    fn string(&self, tabs: usize) -> String {
        let mut builder = Builder::default();
        tabs::print_tabs(&mut builder, tabs);
        builder.append("(StructDefNode");
        builder.append(format!("id: {}", self.id));
        for field in &self.fields {
            builder.append(field.string(tabs + 1));
        }
        tabs::print_tabs(&mut builder, tabs);
        builder.append(")");
        builder.string().unwrap()
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        let mut fields = HashMap::<String, Field<'ctx>>::new();
        let mut i = 0;
        for field in self.fields.iter_mut() {
            if let Some((id, tp)) = field.get_type(ctx) {
                fields.insert(id.to_string(), Field { index: i, tp });
            } else {
                return Value::None;
            }
            i = i + 1;
        }
        let name = self.id.as_str();
        let st = ctx.context.opaque_struct_type(name);
        let newf = fields.clone();
        st.set_body(
            &fields
                .into_iter()
                .map(|(_, v)| v.tp.get_basic_type())
                .collect::<Vec<_>>(),
            false,
        );
        let stu = PLType::STRUCT(STType {
            name: name.to_string(),
            struct_type: st,
            fields: newf,
        });
        ctx.add_type(name.to_string(), stu.clone());
        Value::TypeValue(st.as_basic_type_enum())
    }
}

#[range]
pub struct StructInitFieldNode {
    pub id: String,
    pub exp: Box<dyn Node>,
}

impl Node for StructInitFieldNode {
    fn string(&self, tabs: usize) -> String {
        todo!()
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        todo!()
    }
}
