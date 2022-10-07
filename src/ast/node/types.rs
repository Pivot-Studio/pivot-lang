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

impl TypeNameNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("TypeNameNode");
        tab(tabs + 1, line.clone(), true);
        println!("id: {}", self.id);
    }
}

impl TypeNameNode {
    pub fn get_debug_type<'a, 'ctx>(&'a self, ctx: &mut Ctx<'a, 'ctx>) -> Option<DIType<'ctx>> {
        let (tp, _) = ctx.get_type(&self.id).unwrap().clone();
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

#[range]
#[derive(Clone)]
pub struct TypedIdentifierNode {
    pub id: String,
    pub tp: Box<TypeNameNode>,
}

impl TypedIdentifierNode {
    pub fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("TypedIdentifierNode");
        tab(tabs + 1, line.clone(), false);
        println!("id: {}", self.id);
        self.tp.print(tabs + 1, true, line.clone());
    }
}

#[range]
pub struct StructDefNode {
    pub id: String,
    pub fields: Vec<Box<TypedIdentifierNode>>,
}

impl StructDefNode {
    pub fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
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
}

impl StructDefNode {
    pub fn emit_struct_def<'a, 'ctx>(&'a self, ctx: &mut Ctx<'a, 'ctx>) -> bool {
        if ctx.get_type(self.id.as_str()).is_some() {
            return true;
        }
        let mut fields = BTreeMap::<String, Field<'a, 'ctx>>::new();
        let mut order_fields = Vec::<Field<'a, 'ctx>>::new();
        let mut i = 0;
        for field in self.fields.iter() {
            if let (id, Some((tp, _))) = (field.id.clone(), ctx.get_type(&field.tp.id)) {
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
                    tp: tp.clone(),
                    typename: &field.tp,
                    name: field.id.clone(),
                });
            } else {
                return false;
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
            line_no: self.range().start.line as u32,
        });
        ctx.add_type(name.to_string(), stu.clone());
        true
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
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let (v, tp) = self.exp.emit(ctx)?;
        return Ok((
            Value::StructFieldValue((self.id.clone(), v.as_basic_value_enum())),
            tp,
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
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let mut fields = HashMap::<String, BasicValueEnum<'ctx>>::new();
        for field in self.fields.iter_mut() {
            if let (Value::StructFieldValue((id, val)), _) = field.emit(ctx)? {
                fields.insert(id, val);
            } else {
                panic!("StructInitNode::emit: invalid field");
            }
        }
        let (st, _) = ctx.get_type(self.id.as_str()).unwrap();
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
            return Ok((Value::VarValue(stv), Some(self.id.clone())));
        } else {
            panic!("StructInitNode::emit: invalid type");
        }
    }
}
