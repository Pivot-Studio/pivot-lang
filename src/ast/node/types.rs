use std::collections::HashMap;

use super::*;
use crate::ast::ctx::{Ctx, Field, PLType, STType};
use crate::utils::tabs;
use inkwell::debug_info::*;
use inkwell::types::BasicType;
use internal_macro::range;

use string_builder::Builder;

#[range]
#[derive(Debug, Clone)]
pub struct TypeNameNode {
    pub id: String,
}

pub trait TypeNode: Node {
    fn get_type<'a, 'ctx>(&'a self, ctx: &mut Ctx<'a, 'ctx>) -> Option<PLType<'a, 'ctx>>;
}

impl Node for TypeNameNode {
    fn string(&self, tabs: usize) -> String {
        let mut builder = Builder::default();
        tabs::print_tabs(&mut builder, tabs);
        builder.append("(TypeNameNode ");
        builder.append(format!("id: {}", self.id));
        builder.append(")");
        builder.string().unwrap()
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        return Value::TypeValue(self.get_type(ctx).unwrap().get_basic_type());
    }
}

impl TypeNameNode {
    pub fn get_debug_type<'a, 'ctx>(&'a self, ctx: &mut Ctx<'a, 'ctx>) -> Option<DIType<'ctx>> {
        let tp = self.get_type(ctx).unwrap();
        let td = ctx.targetmachine.get_target_data();

        match tp {
            PLType::FN(_) => todo!(),
            PLType::STRUCT(x) => {
                let m = x
                    .fields
                    .iter()
                    .map(|(_, v)| v.typename.get_debug_type(ctx).unwrap())
                    .collect::<Vec<_>>();

                return Some(
                    ctx.dibuilder
                        .create_struct_type(
                            ctx.diunit.as_debug_info_scope(),
                            self.id.as_str(),
                            ctx.diunit.get_file(),
                            self.range.start.line as u32,
                            td.get_bit_size(&x.struct_type),
                            td.get_abi_alignment(&x.struct_type),
                            DIFlags::PUBLIC,
                            None,
                            &m,
                            0,
                            None,
                            self.id.as_str(),
                        )
                        .as_type(),
                );
            }
            PLType::PRIMITIVE(_) => {
                return Some(
                    ctx.dibuilder
                        .create_basic_type(
                            self.id.as_str(),
                            td.get_bit_size(&tp.get_basic_type()),
                            td.get_abi_alignment(&tp.get_basic_type()),
                            DIFlags::PUBLIC,
                        )
                        .unwrap()
                        .as_type(),
                );
            }
            PLType::VOID(_) => None,
        }
    }
}

impl TypeNode for TypeNameNode {
    fn get_type<'a, 'ctx>(&'a self, ctx: &mut Ctx<'a, 'ctx>) -> Option<PLType<'a, 'ctx>> {
        let tp = ctx.get_type(self.id.as_str());
        if let Some(tp) = tp {
            return Some(tp.clone());
        } else {
            return None;
        }
    }
}

#[range]
#[derive(Clone)]
pub struct TypedIdentifierNode {
    pub id: String,
    pub tp: Box<TypeNameNode>,
}

impl Node for TypedIdentifierNode {
    fn string(&self, tabs: usize) -> String {
        let mut builder = Builder::default();
        tabs::print_tabs(&mut builder, tabs);
        builder.append("(TypedIdentifierNode");
        tabs::print_tabs(&mut builder, tabs + 1);
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
    fn get_type<'a, 'ctx>(&'a self, ctx: &mut Ctx<'a, 'ctx>) -> Option<(&str, PLType<'a, 'ctx>)> {
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
        tabs::print_tabs(&mut builder, tabs + 1);
        builder.append(format!("id: {}", self.id));
        for field in &self.fields {
            builder.append(field.string(tabs + 1));
        }
        tabs::print_tabs(&mut builder, tabs);
        builder.append(")");
        builder.string().unwrap()
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        self.get_type(ctx)
    }
}

impl StructDefNode {
    pub fn get_type<'a, 'ctx>(&'a self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        if let Some(x) = ctx.get_type(self.id.as_str()) {
            return Value::TypeValue(x.get_basic_type());
        }
        let mut fields = HashMap::<String, Field<'a, 'ctx>>::new();
        let mut i = 0;
        for field in self.fields.iter() {
            if let Some((id, tp)) = field.get_type(ctx) {
                fields.insert(
                    id.to_string(),
                    Field {
                        index: i,
                        tp,
                        typename: &field.tp,
                    },
                );
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
        let mut builder = Builder::default();
        tabs::print_tabs(&mut builder, tabs);
        builder.append("(StructInitFieldNode");
        tabs::print_tabs(&mut builder, tabs + 1);
        builder.append(format!("id: {}", self.id));
        builder.append(self.exp.string(tabs + 1));
        tabs::print_tabs(&mut builder, tabs);
        builder.append(")");
        builder.string().unwrap()
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        return Value::StructFieldValue((
            self.id.clone(),
            self.exp.emit(ctx).as_basic_value_enum(),
        ));
    }
}

#[range]
pub struct StructInitNode {
    pub id: String,
    pub fields: Vec<Box<dyn Node>>,
}

impl Node for StructInitNode {
    fn string(&self, tabs: usize) -> String {
        let mut builder = Builder::default();
        tabs::print_tabs(&mut builder, tabs);
        builder.append("(StructInitNode");
        tabs::print_tabs(&mut builder, tabs + 1);
        builder.append(format!("id: {}", self.id));
        for field in &self.fields {
            builder.append(field.string(tabs + 1));
        }
        tabs::print_tabs(&mut builder, tabs);
        builder.append(")");
        builder.string().unwrap()
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        let mut fields = HashMap::<String, BasicValueEnum<'ctx>>::new();
        for field in self.fields.iter_mut() {
            if let Value::StructFieldValue((id, val)) = field.emit(ctx) {
                fields.insert(id, val);
            } else {
                panic!("StructInitNode::emit: invalid field");
            }
        }
        let st = ctx.get_type(self.id.as_str()).unwrap();
        if let PLType::STRUCT(st) = st {
            let et = st.struct_type.as_basic_type_enum();
            let stv = alloc(ctx, et, "initstruct");
            for (id, val) in fields {
                let field = st.fields.get(&id).unwrap();
                let ptr = ctx
                    .builder
                    .build_struct_gep(stv, field.index, "fieldptr")
                    .unwrap();
                ctx.builder.build_store(ptr, val);
            }
            return Value::VarValue(stv);
        } else {
            panic!("StructInitNode::emit: invalid type");
        }
    }
}
