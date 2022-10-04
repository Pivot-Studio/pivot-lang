use std::collections::{BTreeMap, HashMap};

use super::*;
use crate::ast::ctx::{Ctx, Field, PLType, STType};
use inkwell::debug_info::*;
use inkwell::types::BasicType;
use internal_macro::range;

// TODO: match all case
// const DW_ATE_UTF: u32 = 0x10;
const DW_ATE_BOOLEAN: u32 = 0x02;
const DW_ATE_FLOAT: u32 = 0x04;
const DW_ATE_SIGNED: u32 = 0x05;
// const DW_ATE_UNSIGNED: u32 = 0x07;
fn get_dw_ate_encoding(basetype: &BasicTypeEnum) -> u32 {
    match basetype {
        BasicTypeEnum::FloatType(_) => DW_ATE_FLOAT,
        BasicTypeEnum::IntType(i) => match i.get_bit_width() {
            1 => DW_ATE_BOOLEAN,
            64 => DW_ATE_SIGNED,
            _ => todo!(),
        },
        _ => todo!(),
    }
}
#[range]
#[derive(Debug, Clone)]
pub struct TypeNameNode {
    pub id: String,
}

pub trait TypeNode: Node {
    fn get_type<'a, 'ctx>(&'a self, ctx: &mut Ctx<'a, 'ctx>) -> Option<PLType<'a, 'ctx>>;
}

impl Node for TypeNameNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("TypeNameNode");
        tab(tabs + 1, line.clone(), true);
        println!("id: {}", self.id);
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
                let mut offset = 0;
                let m = x
                    .ordered_fields
                    .iter()
                    .map(|v| {
                        let (tp, off) = v.get_di_type(ctx, offset);
                        offset = off;
                        tp
                    })
                    .collect::<Vec<_>>();
                return Some(
                    ctx.dibuilder
                        .create_struct_type(
                            ctx.discope,
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
                            get_dw_ate_encoding(&tp.get_basic_type()),
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
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("TypedIdentifierNode");
        tab(tabs + 1, line.clone(), false);
        println!("id: {}", self.id);
        self.tp.print(tabs + 1, true, line.clone());
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
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StructDefNode");
        tab(tabs + 1, line.clone(), false);
        println!("id: {}", self.id);
        let mut i = self.fields.len();
        for field in &self.fields {
            i -= 1;
            field.print(tabs + 1, i == 0, line.clone());
        }
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
        let mut fields = BTreeMap::<String, Field<'a, 'ctx>>::new();
        let mut order_fields = Vec::<Field<'a, 'ctx>>::new();
        let mut i = 0;
        for field in self.fields.iter() {
            if let Some((id, tp)) = field.get_type(ctx) {
                fields.insert(
                    id.to_string(),
                    Field {
                        index: i,
                        tp: tp.clone(),
                        typename: &field.tp,
                        name: field.id.clone(),
                    },
                );
                order_fields.push(Field {
                    index: i,
                    tp,
                    typename: &field.tp,
                    name: field.id.clone(),
                });
            } else {
                return Value::None;
            }
            i = i + 1;
        }
        let name = self.id.as_str();
        let st = ctx.context.opaque_struct_type(name);
        let newf = order_fields.clone();
        st.set_body(
            &order_fields
                .into_iter()
                .map(|v| v.tp.get_basic_type())
                .collect::<Vec<_>>(),
            false,
        );
        let stu = PLType::STRUCT(STType {
            name: name.to_string(),
            struct_type: st,
            fields,
            ordered_fields: newf,
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
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StructInitFieldNode");
        tab(tabs + 1, line.clone(), false);
        println!("id: {}", self.id);
        self.exp.print(tabs + 1, true, line.clone());
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
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StructInitNode");
        tab(tabs + 1, line.clone(), false);
        println!("id: {}", self.id);
        let mut i = self.fields.len();
        for field in &self.fields {
            i -= 1;
            field.print(tabs + 1, i == 0, line.clone());
        }
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
