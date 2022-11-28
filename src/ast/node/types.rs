use std::cell::RefCell;
use std::rc::Rc;

use super::primary::VarNode;
use super::*;
use crate::ast::ctx::Ctx;
use crate::ast::diag::ErrorCode;
use crate::ast::pltype::{eq, ARRType, Field, GenericType, PLType, STType};
use crate::utils::read_config::enter;
use indexmap::IndexMap;
use inkwell::types::BasicType;
use internal_macro::range;
use lsp_types::SemanticTokenType;
use rustc_hash::FxHashMap;
#[range]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeNameNode {
    pub id: Option<ExternIDNode>,
    pub generic_params: Option<Box<GenericParamNode>>,
}

impl TypeNode for TypeNameNode {
    fn format(&self, _: usize, _: &str) -> String {
        let mut res = String::new();
        if let Some(id_node) = &self.id {
            res.push_str(&id_node.format(0, ""));
        }
        if let Some(generic_params) = &self.generic_params {
            res.push_str(&generic_params.format(0, ""));
        }
        res
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("TypeNameNode");
        if let Some(id) = &self.id {
            id.print(tabs + 1, true, line.clone());
        } else {
            tab(tabs + 1, line.clone(), true);
            println!("id: <empty>");
        }
    }

    fn emit_highlight<'a, 'ctx>(&self, ctx: &mut Ctx<'a, 'ctx>) {
        if let Some(id) = &self.id {
            for ns in id.ns.iter() {
                ctx.push_semantic_token(ns.range, SemanticTokenType::NAMESPACE, 0);
            }
            ctx.push_semantic_token(id.id.range, SemanticTokenType::TYPE, 0);
        }
        if let Some(generic_params) = &self.generic_params {
            generic_params.emit_highlight(ctx);
        }
    }

    fn get_type<'a, 'ctx>(&self, ctx: &Ctx<'a, 'ctx>) -> TypeNodeResult<'ctx> {
        let mut child = ctx.tmp_child_ctx();
        child.if_completion_no_mut(|ctx, a| {
            if a.0.is_in(self.range) {
                let completions = ctx.get_type_completions();
                ctx.completion_items.set(completions);
            }
        });
        if self.id.is_none() {
            return Err(child.add_err(self.range, ErrorCode::EXPECT_TYPE));
        }
        let (_, pltype, _) = self.id.as_ref().unwrap().get_type(&child)?;
        let mut pltype = pltype.unwrap();
        if let Some(generic_params) = &self.generic_params {
            let mut sttype = match &mut *pltype.clone().borrow_mut() {
                PLType::STRUCT(s) => s.clone(),
                _ => return Err(child.add_err(self.range, ErrorCode::NOT_GENERIC_TYPE)),
            };
            sttype.clear_generic();
            sttype.add_generic_type(&mut child)?;
            if generic_params.generics.len() != sttype.generic_map.len() {
                return Err(child.add_err(
                    generic_params.range.clone(),
                    ErrorCode::GENERIC_PARAM_LEN_MISMATCH,
                ));
            }
            let generic_types = generic_params.get_generic_types(&child)?;
            let mut i = 0;
            for (_, pltype) in sttype.generic_map.iter() {
                if generic_types[i].is_none()
                    || !eq(pltype.clone(), generic_types[i].as_ref().unwrap().clone())
                {
                    return Err(child.add_err(self.range, ErrorCode::GENERIC_CANNOT_BE_INFER));
                }
                i = i + 1;
            }
            if sttype.need_gen_code() {
                sttype = sttype.generic_infer_pltype(&child);
            } else {
                return Err(child.add_err(self.range, ErrorCode::GENERIC_CANNOT_BE_INFER));
            }
            pltype = Rc::new(RefCell::new(PLType::STRUCT(sttype.clone())));
        }
        Ok(pltype)
    }

    fn eq_or_infer<'a, 'ctx>(
        &self,
        ctx: &mut Ctx<'a, 'ctx>,
        right: Rc<RefCell<PLType>>,
    ) -> Result<bool, PLDiag> {
        if let Some(generic_params) = &self.generic_params {
            if self.id.is_none() {
                return Err(ctx.add_err(self.range, ErrorCode::EXPECT_TYPE));
            }
            let (_, left, _) = self.id.as_ref().unwrap().get_type(&ctx)?;
            let left = left.unwrap();
            // name not match
            if left.borrow().get_name()
                != right.borrow().get_name().split("<").collect::<Vec<_>>()[0]
            {
                return Ok(false);
            }
            if let (PLType::STRUCT(sttype), PLType::STRUCT(right)) =
                (&mut *left.clone().borrow_mut(), &*right.clone().borrow())
            {
                sttype.clear_generic();
                sttype.add_generic_type(ctx)?;
                if generic_params.generics.len() != sttype.generic_map.len() {
                    return Err(ctx.add_err(
                        generic_params.range.clone(),
                        ErrorCode::GENERIC_PARAM_LEN_MISMATCH,
                    ));
                }
                let generic_types = generic_params.get_generic_types(&ctx)?;
                let mut i = 0;
                for (_, pltype) in sttype.generic_map.iter() {
                    if generic_types[i].is_none()
                        || !eq(pltype.clone(), generic_types[i].as_ref().unwrap().clone())
                    {
                        return Err(ctx.add_err(self.range, ErrorCode::GENERIC_CANNOT_BE_INFER));
                    }
                    i = i + 1;
                }
                for (k, leftfield) in sttype.fields.iter() {
                    let rightpltype = right.fields.get(k).unwrap().typenode.get_type(ctx).unwrap();
                    if !leftfield.typenode.eq_or_infer(ctx, rightpltype)? {
                        return Ok(false);
                    }
                }
                return Ok(true);
            }
            return Err(ctx.add_err(self.range, ErrorCode::NOT_GENERIC_TYPE));
        }
        return Ok(eq(self.get_type(ctx)?, right));
    }
}

#[range]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayTypeNameNode {
    pub id: Box<TypeNodeEnum>,
    pub size: Box<NodeEnum>,
}

impl TypeNode for ArrayTypeNameNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        format!(
            "[{} * {}]",
            &self.id.format(tabs, prefix),
            &self.size.format(tabs, prefix)
        )
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ArrayTypeNameNode");
        self.id.print(tabs + 1, false, line.clone());
        self.size.print(tabs + 1, true, line.clone());
    }
    fn get_type<'a, 'ctx>(&self, ctx: &Ctx<'a, 'ctx>) -> TypeNodeResult<'ctx> {
        if let NodeEnum::Num(num) = *self.size {
            if let Num::INT(sz) = num.value {
                let pltype = self.id.get_type(ctx)?;
                let arrtype = ARRType {
                    element_type: pltype,
                    size: sz as u32,
                };
                let arrtype = Rc::new(RefCell::new(PLType::ARR(arrtype)));
                return Ok(arrtype);
            }
        }
        return Err(ctx.add_err(self.range, ErrorCode::SIZE_MUST_BE_INT));
    }

    fn emit_highlight<'a, 'ctx>(&self, ctx: &mut Ctx<'a, 'ctx>) {
        self.id.emit_highlight(ctx);
    }

    fn eq_or_infer<'a, 'ctx>(
        &self,
        ctx: &mut Ctx<'a, 'ctx>,
        pltype: Rc<RefCell<PLType>>,
    ) -> Result<bool, PLDiag> {
        match &*pltype.borrow() {
            PLType::ARR(a) => {
                if let NodeEnum::Num(num) = *self.size {
                    if let Num::INT(size) = num.value {
                        if a.size as u64 != size {
                            return Ok(false);
                        }
                        return self.id.eq_or_infer(ctx, a.element_type.clone());
                    }
                }
                return Err(ctx.add_err(self.range, ErrorCode::SIZE_MUST_BE_INT));
            }
            _ => Ok(false),
        }
    }
}

#[range]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PointerTypeNode {
    pub elm: Box<TypeNodeEnum>,
}

impl TypeNode for PointerTypeNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        format!("*{}", self.elm.format(tabs, prefix))
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("PointerTypeNode");
        self.elm.print(tabs + 1, true, line.clone());
    }
    fn get_type<'a, 'ctx>(&'a self, ctx: &Ctx<'a, 'ctx>) -> TypeNodeResult<'ctx> {
        let pltype = self.elm.get_type(ctx)?;
        let pltype = Rc::new(RefCell::new(PLType::POINTER(pltype)));
        Ok(pltype)
    }

    fn emit_highlight<'a, 'ctx>(&self, ctx: &mut Ctx<'a, 'ctx>) {
        self.elm.emit_highlight(ctx);
    }

    fn eq_or_infer<'a, 'ctx>(
        &self,
        ctx: &mut Ctx<'a, 'ctx>,
        pltype: Rc<RefCell<PLType>>,
    ) -> Result<bool, PLDiag> {
        match &*pltype.borrow() {
            PLType::POINTER(p) => {
                return self.elm.eq_or_infer(ctx, p.clone());
            }
            _ => Ok(false),
        }
    }
}

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TypedIdentifierNode {
    pub id: VarNode,
    pub typenode: Box<TypeNodeEnum>,
    pub doc: Option<CommentNode>,
}

impl TypedIdentifierNode {
    pub fn format(&self, tabs: usize, prefix: &str) -> String {
        let mut format_res = String::new();
        format_res.push_str(enter());
        format_res.push_str(&prefix.repeat(tabs));
        format_res.push_str(&self.id.name);
        format_res.push_str(": ");
        format_res.push_str(&self.typenode.format(tabs, prefix));
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
        self.typenode.print(tabs + 1, true, line.clone());
    }
}

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct StructDefNode {
    pub doc: Vec<Box<NodeEnum>>,
    pub id: Box<VarNode>,
    pub fields: Vec<(Box<TypedIdentifierNode>, bool)>,
    pub generics: Option<Box<GenericDefNode>>,
}

impl Node for StructDefNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        let mut format_res = String::new();
        format_res.push_str(enter());
        let mut doc_str = String::new();
        for c in self.doc.iter() {
            doc_str.push_str(&c.format(tabs, prefix));
        }
        format_res.push_str(&doc_str);
        format_res.push_str(&prefix.repeat(tabs));
        format_res.push_str("struct ");
        format_res.push_str(&self.id.name);
        if let Some(generics) = &self.generics {
            format_res.push_str(&generics.format(0, ""));
        }
        format_res.push_str(" {");
        for (field, _i) in &self.fields {
            format_res.push_str(&field.format(tabs + 1, prefix));
        }
        format_res.push_str(enter());
        format_res.push_str(&prefix.repeat(tabs));
        format_res.push_str("}");
        format_res.push_str(enter());
        format_res
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StructDefNode");
        tab(tabs + 1, line.clone(), false);
        println!("id: {}", self.id.name);
        for c in self.doc.iter() {
            c.print(tabs + 1, false, line.clone());
        }
        let mut i = self.fields.len();
        for (field, _) in &self.fields {
            i -= 1;
            field.print(tabs + 1, i == 0, line.clone());
        }
    }

    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        for c in self.doc.iter() {
            ctx.push_semantic_token(c.range(), SemanticTokenType::COMMENT, 0);
        }
        ctx.push_semantic_token(self.id.range, SemanticTokenType::STRUCT, 0);
        if let Some(generics) = &mut self.generics {
            generics.emit(ctx)?;
        }
        for (field, has_semi) in self.fields.iter() {
            ctx.push_semantic_token(field.id.range, SemanticTokenType::PROPERTY, 0);
            field.typenode.emit_highlight(ctx);
            if !has_semi {
                return Err(ctx.add_err(field.range, ErrorCode::COMPLETION));
            }
            if let Some(doc) = &field.doc {
                ctx.push_semantic_token(doc.range, SemanticTokenType::COMMENT, 0);
            }
        }
        Ok((None, None, TerminatorEnum::NONE))
    }
}

impl StructDefNode {
    pub fn add_to_symbols<'a, 'ctx>(&self, ctx: &mut Ctx<'a, 'ctx>) {
        let mut generic_map = IndexMap::default();
        if let Some(generics) = &self.generics {
            generic_map = generics.gen_generic_type(ctx);
        }
        let stu = Rc::new(RefCell::new(PLType::STRUCT(STType {
            name: self.id.name.clone(),
            path: ctx.plmod.path.clone(),
            fields: FxHashMap::default(),
            ordered_fields: vec![],
            range: self.range(),
            refs: Rc::new(RefCell::new(vec![])),
            doc: vec![],
            generic_map,
        })));
        ctx.context
            .opaque_struct_type(&ctx.plmod.get_full_name(&self.id.name));
        _ = ctx.add_type(self.id.name.clone(), stu, self.id.range);
    }

    pub fn emit_struct_def<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> Result<(), PLDiag> {
        let mut fields = FxHashMap::<String, Field>::default();
        let mut order_fields = Vec::<Field>::new();
        let mut i = 0;
        // add generic type before field add type
        let child = &mut ctx.tmp_child_ctx();
        let mut generic_map = IndexMap::default();
        if let Some(generics) = &mut self.generics {
            generic_map = generics.gen_generic_type(child);
        }
        for (name, pltype) in generic_map.iter() {
            child.add_type(
                name.clone(),
                pltype.clone(),
                pltype.clone().borrow().get_range().unwrap(),
            )?;
        }
        let pltype = child.get_type(&self.id.name.as_str(), self.range)?;
        for (field, has_semi) in self.fields.iter() {
            if !has_semi {
                return Err(child.add_err(field.range, ErrorCode::COMPLETION));
            }
            let id = field.id.clone();
            let f = Field {
                index: i,
                typenode: field.typenode.clone(),
                name: field.id.name.clone(),
                range: field.range,
                refs: Rc::new(RefCell::new(vec![])),
            };
            let tp = field.typenode.get_type(child)?;
            child.set_if_refs(f.refs.clone(), field.id.range);
            fields.insert(id.name.to_string(), f.clone());
            order_fields.push(f);
            ctx.set_if_refs_tp(tp.clone(), field.typenode.range());
            i = i + 1;
        }
        let newf = order_fields.clone();
        if self.generics.is_none() {
            let st = child
                .module
                .get_struct_type(&child.plmod.get_full_name(&self.id.name))
                .unwrap();
            st.set_body(
                &order_fields
                    .into_iter()
                    .map(|order_field| {
                        order_field
                            .typenode
                            .get_type(child)
                            .unwrap()
                            .borrow()
                            .get_basic_type(&child)
                    })
                    .collect::<Vec<_>>(),
                false,
            );
        }
        if let PLType::STRUCT(st) = &mut *pltype.borrow_mut() {
            st.fields = fields;
            st.ordered_fields = newf;
            st.doc = self.doc.clone();
        }
        child.set_if_refs_tp(pltype.clone(), self.id.range);
        child.add_doc_symbols(pltype.clone());
        child.save_if_comment_doc_hover(self.range, Some(self.doc.clone()));
        Ok(())
    }
}

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct StructInitFieldNode {
    pub id: VarNode,
    pub exp: Box<NodeEnum>,
}

impl Node for StructInitFieldNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        let mut format_res = String::from(prefix.repeat(tabs));
        format_res.push_str(&self.id.name);
        format_res.push_str(": ");
        format_res.push_str(&self.exp.format(tabs, prefix));
        format_res
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StructInitFieldNode");
        tab(tabs + 1, line.clone(), false);
        println!("id: {}", self.id.name);
        self.exp.print(tabs + 1, true, line.clone());
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let (v, tp, _) = self.exp.emit(ctx)?;
        return Ok((v, tp, TerminatorEnum::NONE));
    }
}

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct StructInitNode {
    pub generic_params: Option<Box<GenericParamNode>>,
    pub typename: Box<TypeNodeEnum>,
    pub fields: Vec<Box<StructInitFieldNode>>, // TODO: comment db and salsa comment struct
}

impl Node for StructInitNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        let mut format_res = String::new();
        let mut field_str = String::new();
        match Some(&self.fields) {
            Some(fields) => {
                let mut len = 0;
                field_str.push_str("{");
                field_str.push_str(enter());
                for field in fields {
                    len += 1;
                    field_str.push_str(&field.format(tabs + 1, prefix));
                    if len < fields.len() {
                        field_str.push_str(",");
                        field_str.push_str(enter());
                    } else {
                        field_str.push_str(enter());
                    }
                }
                field_str.push_str(&prefix.repeat(tabs));
                field_str.push_str("}");
            }
            _ => (),
        }
        format_res.push_str(&self.typename.format(tabs, prefix));
        if let Some(generic_params) = &self.generic_params {
            format_res.push_str(&generic_params.format(0, ""));
        }
        format_res.push_str(&field_str);
        format_res
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StructInitNode");
        self.typename
            .print(tabs + 1, self.fields.len() == 0, line.clone());
        let mut i = self.fields.len();
        for field in &self.fields {
            i -= 1;
            field.print(tabs + 1, i == 0, line.clone());
        }
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let child = &mut ctx.tmp_child_ctx();
        self.typename.emit_highlight(child);
        let pltype = self.typename.get_type(child)?;
        ctx.set_if_refs_tp(pltype.clone(), self.typename.range());
        let mut sttype = match &mut *pltype.clone().borrow_mut() {
            PLType::STRUCT(s) => s.clone(),
            _ => unreachable!(),
        };
        sttype.clear_generic();
        sttype.add_generic_type(child)?;
        if let Some(generic_params) = &mut self.generic_params {
            generic_params.emit_highlight(child);
            if generic_params.generics.len() != sttype.generic_map.len() {
                return Err(child.add_err(
                    generic_params.range.clone(),
                    ErrorCode::GENERIC_PARAM_LEN_MISMATCH,
                ));
            }
            let generic_types = generic_params.get_generic_types(child)?;
            let mut i = 0;
            for (_, pltype) in sttype.generic_map.iter() {
                if generic_types[i].is_some() {
                    eq(pltype.clone(), generic_types[i].as_ref().unwrap().clone());
                }
                i = i + 1;
            }
        }
        child.save_if_comment_doc_hover(self.typename.range(), Some(sttype.doc.clone()));
        let mut field_init_values = vec![];
        for fieldinit in self.fields.iter_mut() {
            let field_id_range = fieldinit.id.range;
            let field_exp_range = fieldinit.exp.range();

            let field = sttype.fields.get(&fieldinit.id.name);
            if field.is_none() {
                child.if_completion(|ctx, a| {
                    if a.0.is_in(self.range) {
                        let completions = sttype.get_completions(&ctx);
                        ctx.completion_items.set(completions);
                    }
                });
                return Err(child.add_err(field_id_range, ErrorCode::STRUCT_FIELD_NOT_FOUND));
            }
            let field = field.unwrap();
            let (value, value_pltype, _) = fieldinit.emit(child)?;
            if value.is_none() || value_pltype.is_none() {
                return Err(child.add_err(field_exp_range, ErrorCode::EXPECT_VALUE));
            }
            let value = child.try_load2var(field_exp_range, value.unwrap())?;
            let value_pltype = value_pltype.unwrap();
            if !field.typenode.eq_or_infer(child, value_pltype)? {
                return Err(child.add_err(fieldinit.range, ErrorCode::STRUCT_FIELD_TYPE_NOT_MATCH));
            }
            field_init_values.push((field.index, value));
            child.set_if_refs(field.refs.clone(), field_id_range);
        }
        if !sttype.generic_map.is_empty() {
            if sttype.need_gen_code() {
                sttype = sttype.generic_infer_pltype(child);
            } else {
                return Err(
                    child.add_err(self.typename.range(), ErrorCode::GENERIC_CANNOT_BE_INFER)
                );
            }
        }
        let pltype = Rc::new(RefCell::new(PLType::STRUCT(sttype.clone())));
        let struct_pointer = alloc(
            child,
            sttype.struct_type(child).as_basic_type_enum(),
            "initstruct",
        );
        field_init_values.iter().for_each(|(index, value)| {
            let fieldptr = child
                .builder
                .build_struct_gep(struct_pointer, *index, "fieldptr")
                .unwrap();
            child.builder.build_store(fieldptr, *value);
        });
        let mut need_add_field_pltypes = vec![];
        for (_, f) in sttype.fields.iter() {
            let pltype = f.typenode.get_type(child).unwrap().clone();
            if ctx
                .get_type(&pltype.borrow().get_name(), Default::default())
                .is_err()
            {
                need_add_field_pltypes.push(pltype.clone());
            }
        }
        for pltype in need_add_field_pltypes.iter() {
            if let Some(range) = pltype.borrow().get_range() {
                ctx.add_type(pltype.borrow().get_name(), pltype.clone(), range)?;
            }
        }
        if ctx
            .get_type(&pltype.borrow().get_name(), Default::default())
            .is_err()
        {
            ctx.add_type(
                pltype.borrow().get_name(),
                pltype.clone(),
                pltype.borrow().get_range().unwrap(),
            )?;
        }
        return Ok((
            Some(struct_pointer.into()),
            Some(pltype.clone()),
            TerminatorEnum::NONE,
        ));
    }
}

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ArrayInitNode {
    // pub tp: Box<TypeNameNode>,
    pub exps: Vec<Box<NodeEnum>>,
}

impl Node for ArrayInitNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        let mut format_res = String::from("[");
        for (i, exp) in self.exps.iter().enumerate() {
            format_res.push_str(&exp.format(tabs, prefix));
            if i != self.exps.len() - 1 {
                format_res.push_str(", ");
            }
        }
        format_res.push(']');
        format_res
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ArrayInitNode");
        // self.tp.print(tabs + 1, self.exps.len() == 0, line.clone());
        let mut i = self.exps.len();
        for exp in &self.exps {
            i -= 1;
            exp.print(tabs + 1, i == 0, line.clone());
        }
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let mut exps = Vec::<BasicValueEnum<'ctx>>::new();
        let mut tp0 = None;

        for exp in self.exps.iter_mut() {
            let range = exp.range();
            let (v, tp, _) = exp.emit(ctx)?;
            // 检查类型是否一致
            if tp0.is_none() {
                tp0 = tp;
            } else if tp0 != tp {
                return Err(ctx.add_err(range, ErrorCode::ARRAY_TYPE_NOT_MATCH));
            }
            exps.push(ctx.try_load2var(range, v.unwrap())?.as_basic_value_enum());
        }
        if tp0.is_none() {
            return Err(ctx.add_err(self.range, ErrorCode::ARRAY_INIT_EMPTY));
        }
        let tp = exps[0].get_type();
        let sz = exps.len() as u32;
        let arr = alloc(ctx, tp.array_type(sz).as_basic_type_enum(), "array_alloca");

        for (i, v) in exps.into_iter().enumerate() {
            let index = &[
                ctx.context.i64_type().const_int(0 as u64, false),
                ctx.context.i64_type().const_int(i as u64, false),
            ];
            let ptr = unsafe { ctx.builder.build_in_bounds_gep(arr, index, "elem_ptr") };
            ctx.builder.build_store(ptr, v);
        }
        return Ok((
            Some(arr.into()),
            Some(Rc::new(RefCell::new(PLType::ARR(ARRType {
                element_type: tp0.unwrap(),
                size: sz,
            })))),
            TerminatorEnum::NONE,
        ));
    }
}

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct GenericDefNode {
    pub generics: Vec<Box<VarNode>>,
}
impl Node for GenericDefNode {
    fn format(&self, _tabs: usize, _prefix: &str) -> String {
        format!(
            "<{}>",
            self.generics
                .iter()
                .map(|g| { g.name.clone() })
                .collect::<Vec<_>>()
                .join("|")
        )
    }

    fn print(&self, _tabs: usize, _end: bool, _line: Vec<bool>) {
        todo!()
    }

    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        for g in self.generics.iter() {
            ctx.push_semantic_token(g.range, SemanticTokenType::TYPE, 0);
        }
        return Ok((None, None, TerminatorEnum::NONE));
    }
}
impl GenericDefNode {
    pub fn gen_generic_type<'a, 'ctx>(
        &self,
        _: &mut Ctx<'a, 'ctx>,
    ) -> IndexMap<String, Rc<RefCell<PLType>>> {
        let mut res = IndexMap::default();
        for g in self.generics.iter() {
            let range = g.range;
            let name = g.name.clone();
            let gentype = GenericType {
                name: name.clone(),
                range: range.clone(),
                curpltype: None,
            };
            res.insert(name, Rc::new(RefCell::new(PLType::GENERIC(gentype))));
        }
        res
    }
}

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct GenericParamNode {
    pub generics: Vec<Option<Box<TypeNodeEnum>>>,
}
impl Node for GenericParamNode {
    fn format(&self, _tabs: usize, _prefix: &str) -> String {
        format!(
            "<{}>",
            self.generics
                .iter()
                .map(|g| {
                    match g {
                        Some(n) => n.format(0, ""),
                        None => "_".to_string(),
                    }
                })
                .collect::<Vec<_>>()
                .join("|")
        )
    }

    fn print(&self, _tabs: usize, _end: bool, _line: Vec<bool>) {
        todo!()
    }

    fn emit<'a, 'ctx>(&mut self, _: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        return Ok((None, None, TerminatorEnum::NONE));
    }
}
impl GenericParamNode {
    pub fn get_generic_types<'a, 'ctx>(
        &self,
        ctx: &Ctx<'a, 'ctx>,
    ) -> Result<Vec<Option<Rc<RefCell<PLType>>>>, PLDiag> {
        let mut res = vec![];
        for g in self.generics.iter() {
            if g.is_none() {
                res.push(None);
                continue;
            }
            res.push(Some(g.as_ref().unwrap().get_type(ctx)?));
        }
        Ok(res)
    }
    pub fn emit_highlight<'a, 'ctx>(&self, ctx: &mut Ctx<'a, 'ctx>) {
        for g in self.generics.iter() {
            if g.is_some() {
                g.as_ref().unwrap().emit_highlight(ctx);
            }
        }
    }
}
