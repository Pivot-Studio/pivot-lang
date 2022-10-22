use std::cell::RefCell;
use std::rc::Rc;

use super::primary::VarNode;
use super::*;
use crate::ast::ctx::{Ctx, Field, PLType, STType};
use crate::ast::diag::ErrorCode;
use crate::ast::range::Range;
use inkwell::types::{AnyType, BasicType};
use internal_macro::range;
use lsp_types::SemanticTokenType;
use rustc_hash::FxHashMap;
#[range]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeNameNode {
    pub id: String,
    pub is_ref: bool,
}

impl TypeNameNode {
    fn format(&self, _tabs: usize, _prefix: &str) -> String {
        let id = &self.id;
        return id.to_string();
    }
    pub fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("TypeNameNode");
        tab(tabs + 1, line.clone(), true);
        if self.is_ref {
            println!("id: &{}", self.id);
        } else {
            println!("id: {}", self.id);
        }
    }
    pub fn get_type<'a, 'ctx>(&'a self, ctx: &mut Ctx<'a, 'ctx>) -> Result<PLType, PLDiag> {
        ctx.if_completion(|ctx, a| {
            if a.0.is_in(self.range) {
                let completions = ctx.get_type_completions();
                ctx.completion_items.set(completions);
            }
        });
        ctx.push_semantic_token(self.range, SemanticTokenType::TYPE, 0);
        let re = ctx.get_type(&self.id, self.range)?.clone();
        if let Some(dst) = re.get_range() {
            ctx.send_if_go_to_def(self.range, dst, ctx.plmod.path.clone());
        }
        ctx.set_if_refs_tp(&re, self.range);
        Ok(re)
    }
}
#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TypedIdentifierNode {
    pub id: VarNode,
    pub tp: Box<TypeNameNode>,
    pub doc: Option<CommentNode>,
}

impl TypedIdentifierNode {
    pub fn format(&self, tabs: usize, prefix: &str) -> String {
        let mut format_res = String::from("\n\r");
        let mut id = String::new();
        if self.tp.is_ref {
            let ref_id = format!("&{}", &self.tp.id);
            id.push_str(&ref_id);
        } else {
            id.push_str(&self.tp.id);
        }
        format_res.push_str(&prefix.repeat(tabs));
        format_res.push_str(&self.id.name);
        format_res.push_str(": ");
        format_res.push_str(&id);
        format_res.push_str(";");
        if let Some(doc) = &self.doc {
            format_res.push_str(&doc.format(tabs, prefix));
        }
        return format_res;
    }
    pub fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("TypedIdentifierNode");
        tab(tabs + 1, line.clone(), false);
        println!("id: {}", self.id.name);
        if let Some(doc) = &self.doc {
            doc.print(tabs + 1, false, line.clone());
        }
        self.tp.print(tabs + 1, true, line.clone());
    }
}

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct StructDefNode {
    pub doc: Vec<Box<NodeEnum>>,
    pub id: String,
    pub fields: Vec<Box<TypedIdentifierNode>>,
}

impl Node for StructDefNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        let mut format_res = String::from("\n\r");
        let mut doc_str = String::new();
        for c in self.doc.iter() {
            doc_str.push_str(&c.format(tabs, prefix));
            doc_str.push_str("\n\r");
        }
        format_res.push_str(&doc_str);
        format_res.push_str(&prefix.repeat(tabs));
        format_res.push_str("struct ");
        format_res.push_str(&self.id);
        format_res.push_str(" {");
        for field in &self.fields {
            format_res.push_str(&field.format(tabs + 1, prefix));
        }
        format_res.push_str("\n\r");
        format_res.push_str(&prefix.repeat(tabs));
        format_res.push_str("}");
        format_res
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StructDefNode");
        tab(tabs + 1, line.clone(), false);
        println!("id: {}", self.id);
        for c in self.doc.iter() {
            c.print(tabs + 1, false, line.clone());
        }
        let mut i = self.fields.len();
        for field in &self.fields {
            i -= 1;
            field.print(tabs + 1, i == 0, line.clone());
        }
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        for c in self.doc.iter() {
            ctx.push_semantic_token(c.range(), SemanticTokenType::COMMENT, 0);
        }
        ctx.push_semantic_token(self.range, SemanticTokenType::STRUCT, 0);
        for f in self.fields.clone() {
            ctx.push_semantic_token(f.id.range, SemanticTokenType::PROPERTY, 0);
            ctx.push_semantic_token(f.tp.range, SemanticTokenType::TYPE, 0);
            if let Some(doc) = f.doc {
                ctx.push_semantic_token(doc.range, SemanticTokenType::COMMENT, 0);
            }
        }
        Ok((Value::None, None, TerminatorEnum::NONE, false))
    }
}

impl StructDefNode {
    pub fn emit_struct_def<'a, 'ctx>(&'a self, ctx: &mut Ctx<'a, 'ctx>) -> Result<(), PLDiag> {
        if ctx.get_type(self.id.as_str(), self.range).is_ok() {
            ctx.add_err(self.range, ErrorCode::REDEFINE_SYMBOL);
            return Ok(());
        }
        let mut fields = FxHashMap::<String, Field>::default();
        let mut order_fields = Vec::<Field>::new();
        let mut i = 0;
        for field in self.fields.iter() {
            let (id, tp) = (field.id.clone(), field.tp.get_type(ctx)?);
            let f = Field {
                index: i,
                tp: tp.clone(),
                typename: field.tp.clone(),
                name: field.id.name.clone(),
                range: field.id.range,
                is_ref: field.tp.is_ref,
                refs: Rc::new(RefCell::new(vec![])),
            };
            ctx.send_if_go_to_def(f.range, f.range, ctx.plmod.path.clone());
            ctx.set_if_refs(f.refs.clone(), field.id.range);
            fields.insert(id.name.to_string(), f.clone());
            order_fields.push(f);

            i = i + 1;
        }
        let name = self.id.as_str();
        let st = ctx
            .context
            .opaque_struct_type(&ctx.plmod.get_full_name(name));
        let newf = order_fields.clone();
        st.set_body(
            &order_fields
                .into_iter()
                .map(|order_field| {
                    if order_field.is_ref {
                        order_field
                            .tp
                            .get_basic_type(&ctx)
                            .ptr_type(inkwell::AddressSpace::Generic)
                            .as_basic_type_enum()
                    } else {
                        order_field.tp.get_basic_type(&ctx)
                    }
                })
                .collect::<Vec<_>>(),
            false,
        );
        let stu = PLType::STRUCT(STType {
            name: name.to_string(),
            fields,
            ordered_fields: newf,
            range: self.range(),
            refs: Rc::new(RefCell::new(vec![])),
            doc: self.doc.clone(),
        });
        ctx.set_if_refs_tp(&stu, self.range);
        _ = ctx.add_type(name.to_string(), stu.clone(), self.range);
        ctx.save_if_comment_doc_hover(self.range, Some(self.doc.clone()));
        Ok(())
    }
}

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct StructInitFieldNode {
    pub id: String,
    pub exp: Box<NodeEnum>,
}

impl Node for StructInitFieldNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        let mut format_res = String::from(prefix.repeat(tabs));
        format_res.push_str(&self.id);
        format_res.push_str(": ");
        format_res.push_str(&self.exp.format(tabs, prefix));
        format_res
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StructInitFieldNode");
        tab(tabs + 1, line.clone(), false);
        println!("id: {}", self.id);
        self.exp.print(tabs + 1, true, line.clone());
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let (v, tp, _, _) = self.exp.emit(ctx)?;
        let value = if let Value::RefValue(_) = v {
            v.as_basic_value_enum()
        } else {
            ctx.try_load2var(v).as_basic_value_enum()
        };
        return Ok((
            Value::StructFieldValue((self.id.clone(), value)),
            tp,
            TerminatorEnum::NONE,
            false,
        ));
    }
}

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct StructInitNode {
    pub tp: Box<TypeNameNode>,
    pub fields: Vec<Box<NodeEnum>>, // TODO: comment db and salsa comment struct
}

impl Node for StructInitNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        let mut format_res = String::new();
        let mut field_str = String::new();
        match Some(&self.fields) {
            Some(fields) => {
                let mut len = 0;
                field_str.push_str("{\n\r");
                for field in fields {
                    len += 1;
                    field_str.push_str(&field.format(tabs + 1, prefix));
                    if len < fields.len() {
                        field_str.push_str(",\n\r")
                    } else {
                        field_str.push_str("\n\r")
                    }
                }
                field_str.push_str(&prefix.repeat(tabs));
                field_str.push_str("}");
            }
            _ => (),
        }
        format_res.push_str(&self.tp.format(tabs, prefix));
        format_res.push_str(&field_str);
        format_res
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StructInitNode");
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
        let mut fields = FxHashMap::<String, (BasicValueEnum<'ctx>, Range)>::default();
        for field in self.fields.iter_mut() {
            let range = field.range();
            if let (Value::StructFieldValue((id, val)), _, _, _) = field.emit(ctx)? {
                fields.insert(id, (val, range));
            } else {
                panic!("StructInitNode::emit: invalid field");
            }
        }
        let st = self.tp.get_type(ctx)?;
        if let PLType::STRUCT(st) = st {
            ctx.save_if_comment_doc_hover(self.tp.range, Some(st.doc.clone()));
            let et = st.struct_type(ctx).as_basic_type_enum();
            let stv = alloc(ctx, et, "initstruct");
            for (id, (val, range)) in fields {
                let field = st.fields.get(&id);
                if field.is_none() {
                    ctx.if_completion(|ctx, a| {
                        if a.0.is_in(self.range) {
                            let completions = st.get_completions();
                            ctx.completion_items.set(completions);
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
                ctx.send_if_go_to_def(range, field.range, ctx.plmod.path.clone())
            }
            return Ok((
                Value::VarValue(stv),
                Some(st.name),
                TerminatorEnum::NONE,
                false,
            ));
        } else {
            panic!("StructInitNode::emit: invalid type");
        }
    }
}
