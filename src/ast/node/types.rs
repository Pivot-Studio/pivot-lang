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
    pub id: Option<ExternIDNode>,
    pub is_ref: bool,
}

impl TypeNameNode {
    pub fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("TypeNameNode");
        tab(tabs + 1, line.clone(), true);
        // if self.is_ref {
        //     println!("id: &{}", self.id);
        // } else {
        //     println!("id: {}", self.id);
        // }
    }
    pub fn get_type<'a, 'ctx>(&'a self, ctx: &mut Ctx<'a, 'ctx>) -> Result<PLType, PLDiag> {
        ctx.if_completion(|ctx, a| {
            if a.0.is_in(self.range) {
                let completions = ctx.get_type_completions();
                ctx.completion_items.set(completions);
            }
        });
        if self.id.is_none() {
            return Err(ctx.add_err(self.range, ErrorCode::EXPECT_TYPE));
        }
        let (_, pltype, _, _) = self.id.as_ref().unwrap().get_type(ctx)?;
        let pltype = pltype.unwrap();
        if let Some(dst) = pltype.get_range() {
            ctx.send_if_go_to_def(self.range, dst, ctx.plmod.path.clone());
        }
        ctx.set_if_refs_tp(&pltype, self.range);
        Ok(pltype)
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
    pub fields: Vec<(Box<TypedIdentifierNode>, bool)>,
}

impl Node for StructDefNode {
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
        for (field, _) in &self.fields {
            i -= 1;
            field.print(tabs + 1, i == 0, line.clone());
        }
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        for c in self.doc.iter() {
            ctx.push_semantic_token(c.range(), SemanticTokenType::COMMENT, 0);
        }
        ctx.push_semantic_token(self.range, SemanticTokenType::STRUCT, 0);
        for (field, has_semi) in self.fields.iter() {
            ctx.push_semantic_token(field.id.range, SemanticTokenType::PROPERTY, 0);
            field.tp.get_type(ctx)?;
            if !has_semi {
                return Err(ctx.add_err(field.range, ErrorCode::COMPLETION));
            }
            if let Some(doc) = &field.doc {
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
        for (field, has_semi) in self.fields.iter() {
            if !has_semi {
                return Err(ctx.add_err(field.range, ErrorCode::COMPLETION));
            }
            let (id, tp) = (field.id.clone(), field.tp.get_type(ctx)?);
            let f = Field {
                index: i,
                pltype: tp.clone(),
                name: field.id.name.clone(),
                range: field.range,
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
                            .pltype
                            .get_basic_type(&ctx)
                            .ptr_type(inkwell::AddressSpace::Generic)
                            .as_basic_type_enum()
                    } else {
                        order_field.pltype.get_basic_type(&ctx)
                    }
                })
                .collect::<Vec<_>>(),
            false,
        );
        let stu = PLType::STRUCT(STType {
            name: name.to_string(),
            path: ctx.plmod.path.clone(),
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
    pub id: VarNode,
    pub exp: Box<NodeEnum>,
    pub has_comma: bool,
}

impl Node for StructInitFieldNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StructInitFieldNode");
        tab(tabs + 1, line.clone(), false);
        // println!("id: {}", self.id);
        self.exp.print(tabs + 1, true, line.clone());
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let range = self.exp.range();
        let (v, tp, _, _) = self.exp.emit(ctx)?;
        if !self.has_comma {
            return Err(ctx.add_err(self.range, ErrorCode::COMPLETION));
        }
        let value = if let Value::RefValue(_) = v {
            v.as_basic_value_enum()
        } else {
            let v = ctx.try_load2var(v);
            let vop = v.as_basic_value_enum_op();
            if vop.is_none() {
                return Err(ctx.add_err(range, ErrorCode::STRUCT_FIELD_TYPE_NOT_MATCH));
            }
            vop.unwrap()
        };
        return Ok((
            Value::StructFieldValue((self.id.name.clone(), value)),
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
        let mut fields = FxHashMap::<String, (BasicValueEnum<'ctx>, Range)>::default();
        let tp = self.tp.get_type(ctx)?;
        for field in self.fields.iter_mut() {
            let range = field.range();
            if let (Value::StructFieldValue((id, val)), _, _, _) = field.emit(ctx)? {
                fields.insert(id, (val, range));
            } else {
                panic!("StructInitNode::emit: invalid field");
            }
        }
        if let PLType::STRUCT(st) = &tp {
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
                ctx.set_if_refs(field.refs.clone(), range);
                ctx.send_if_go_to_def(range, field.range, st.path.clone())
            }
            return Ok((Value::VarValue(stv), Some(tp), TerminatorEnum::NONE, false));
        } else {
            panic!("StructInitNode::emit: invalid type");
        }
    }
}
