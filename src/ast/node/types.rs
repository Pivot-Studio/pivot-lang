use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

use super::primary::VarNode;
use super::*;
use crate::ast::ctx::{Ctx, Field, PLType, STType};
use crate::ast::error::ErrorCode;
use crate::ast::range::Range;
use crate::lsp::helpers::send_completions;
use inkwell::debug_info::*;
use inkwell::types::{AnyType, BasicType};
use internal_macro::range;
use lsp_types::SemanticTokenType;
#[range]
#[derive(Debug, Clone)]
pub struct TypeNameNode {
    pub id: String,
    pub is_ref: bool,
}

impl TypeNameNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("TypeNameNode");
        tab(tabs + 1, line.clone(), true);
        println!("id: {}", self.id);
    }
    pub fn get_type<'a, 'ctx>(
        &'a self,
        ctx: &mut Ctx<'a, 'ctx>,
    ) -> Result<(PLType<'a, 'ctx>, Option<DIType<'ctx>>), PLDiag> {
        ctx.if_completion(|ctx, a| {
            if a.0.is_in(self.range) {
                let completions = ctx.get_type_completions();
                send_completions(ctx.sender.unwrap(), a.1.clone(), completions);
            }
        });
        ctx.push_semantic_token(self.range, SemanticTokenType::TYPE, 0);
        let re = ctx.get_type(&self.id, self.range)?.clone();
        if let Some(dst) = re.0.get_range() {
            ctx.send_if_go_to_def(self.range, dst);
        }
        ctx.set_if_refs_tp(&re.0, self.range);
        Ok(re)
    }
}
#[range]
#[derive(Clone)]
pub struct TypedIdentifierNode {
    pub id: VarNode,
    pub tp: Box<TypeNameNode>,
}

impl TypedIdentifierNode {
    pub fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("TypedIdentifierNode");
        tab(tabs + 1, line.clone(), false);
        println!("id: {}", self.id.name);
        self.tp.print(tabs + 1, true, line.clone());
    }
}

#[range]
#[derive(Clone)]
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

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.push_semantic_token(self.range, SemanticTokenType::STRUCT, 0);
        for f in self.fields.clone() {
            ctx.push_semantic_token(f.id.range, SemanticTokenType::PROPERTY, 0);
            ctx.push_semantic_token(f.tp.range, SemanticTokenType::TYPE, 0);
        }
        Ok((Value::None, None))
    }
}

impl StructDefNode {
    pub fn emit_struct_def<'a, 'ctx>(&'a self, ctx: &mut Ctx<'a, 'ctx>) -> Result<(), PLDiag> {
        if ctx.get_type(self.id.as_str(), self.range).is_ok() {
            return Ok(());
        }
        let mut fields = BTreeMap::<String, Field<'a, 'ctx>>::new();
        let mut order_fields = Vec::<Field<'a, 'ctx>>::new();
        let mut i = 0;
        for field in self.fields.iter() {
            let (id, (tp, _)) = (field.id.clone(), field.tp.get_type(ctx)?);
            let f = Field {
                index: i,
                tp: tp.clone(),
                typename: &field.tp,
                name: field.id.name.clone(),
                range: field.id.range,
                is_ref: field.tp.is_ref,
                refs: Rc::new(RefCell::new(vec![])),
            };
            ctx.send_if_go_to_def(f.range, f.range);
            ctx.set_if_refs(f.refs.clone(), field.id.range);
            fields.insert(id.name.to_string(), f.clone());
            order_fields.push(f);

            i = i + 1;
        }
        let name = self.id.as_str();
        let st = ctx.context.opaque_struct_type(name);
        let newf = order_fields.clone();
        st.set_body(
            &order_fields
                .into_iter()
                .map(|order_field| {
                    if order_field.is_ref {
                        order_field
                            .tp
                            .get_basic_type()
                            .ptr_type(inkwell::AddressSpace::Generic)
                            .as_basic_type_enum()
                    } else {
                        order_field.tp.get_basic_type()
                    }
                })
                .collect::<Vec<_>>(),
            false,
        );
        let stu = PLType::STRUCT(STType {
            name: name.to_string(),
            struct_type: st,
            fields,
            ordered_fields: newf,
            range: self.range(),
            refs: Rc::new(RefCell::new(vec![])),
        });
        ctx.set_if_refs_tp(&stu, self.range);
        _ = ctx.add_type(name.to_string(), stu.clone(), self.range);
        Ok(())
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
        let value = if let Value::RefValue(_) = v {
            v.as_basic_value_enum()
        } else {
            ctx.try_load2(v).as_basic_value_enum()
        };
        return Ok((Value::StructFieldValue((self.id.clone(), value)), tp));
    }
}

#[range]
pub struct StructInitNode {
    pub tp: Box<TypeNameNode>,
    pub fields: Vec<Box<dyn Node>>,
}

impl Node for StructInitNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StructInitNode");
        tab(tabs + 1, line.clone(), false);
        self.tp
            .print(tabs + 1, self.fields.len() == 0, line.clone());
        let mut i = self.fields.len();
        for field in &self.fields {
            i -= 1;
            field.print(tabs + 1, i == 0, line.clone());
        }
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.push_semantic_token(self.tp.range, SemanticTokenType::STRUCT, 0);
        let mut fields = HashMap::<String, (BasicValueEnum<'ctx>, Range)>::new();
        for field in self.fields.iter_mut() {
            let range = field.range();
            if let (Value::StructFieldValue((id, val)), _) = field.emit(ctx)? {
                fields.insert(id, (val, range));
            } else {
                panic!("StructInitNode::emit: invalid field");
            }
        }
        let (st, _) = self.tp.get_type(ctx)?;
        if let PLType::STRUCT(st) = st {
            let et = st.struct_type.as_basic_type_enum();
            let stv = alloc(ctx, et, "initstruct");
            for (id, (val, range)) in fields {
                let field = st.fields.get(&id);
                if field.is_none() {
                    ctx.if_completion(|ctx, a| {
                        if a.0.is_in(self.range) {
                            let completions = st.get_completions();
                            send_completions(ctx.sender.unwrap(), a.1.clone(), completions);
                        }
                    });
                    return Err(ctx.add_err(range, ErrorCode::STRUCT_FIELD_NOT_FOUND));
                }
                let field = field.unwrap();
                let ptr = ctx
                    .builder
                    .build_struct_gep(stv, field.index, "fieldptr")
                    .unwrap();
                if ptr.get_type().get_element_type() != val.get_type().as_any_type_enum() {
                    return Err(ctx.add_err(range, ErrorCode::STRUCT_FIELD_TYPE_NOT_MATCH));
                }
                ctx.builder.build_store(ptr, val);
                ctx.send_if_go_to_def(range, field.range)
            }
            return Ok((Value::VarValue(stv), Some(st.name)));
        } else {
            panic!("StructInitNode::emit: invalid type");
        }
    }
}
