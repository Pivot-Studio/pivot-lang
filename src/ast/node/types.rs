use std::cell::RefCell;
use std::rc::Rc;

use super::primary::VarNode;
use super::*;
use crate::ast::ctx::Ctx;
use crate::ast::diag::ErrorCode;
use crate::ast::pltype::{eq, ARRType, Field, GenericType, PLType, STType};
use crate::plv;
use indexmap::IndexMap;
use inkwell::types::BasicType;
use internal_macro::{comments, fmt, range};
use lsp_types::SemanticTokenType;
use rustc_hash::FxHashMap;
#[range]
#[fmt]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeNameNode {
    pub id: Option<ExternIdNode>,
    pub generic_params: Option<Box<GenericParamNode>>,
}

impl TypeNode for TypeNameNode {
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

    fn get_type<'a, 'ctx>(&self, ctx: &mut Ctx<'a, 'ctx>) -> TypeNodeResult {
        if self.id.is_none() {
            ctx.if_completion(self.range, || ctx.get_type_completions());
            return Err(ctx.add_err(self.range, ErrorCode::EXPECT_TYPE));
        }
        let (_, pltype, _) = self.id.as_ref().unwrap().get_type(&ctx)?;
        ctx.if_completion(self.range, || ctx.get_type_completions());
        let mut pltype = pltype.unwrap();
        if let Some(generic_params) = &self.generic_params {
            let mut sttype = match &mut *pltype.clone().borrow_mut() {
                PLType::STRUCT(s) => s.clone(),
                _ => return Err(ctx.add_err(self.range, ErrorCode::NOT_GENERIC_TYPE)),
            };
            let generic_types = generic_params.get_generic_types(ctx)?;
            if generic_params.generics.len() != sttype.generic_map.len() {
                return Err(ctx.add_err(
                    generic_params.range.clone(),
                    ErrorCode::GENERIC_PARAM_LEN_MISMATCH,
                ));
            }
            let mut i = 0;
            for (_, pltype) in sttype.generic_map.iter() {
                if generic_types[i].is_none() {
                    return Err(ctx.add_err(self.range, ErrorCode::GENERIC_CANNOT_BE_INFER));
                }
                if pltype == generic_types[i].as_ref().unwrap() {
                    if let PLType::GENERIC(g) = &mut *pltype.borrow_mut() {
                        // self ref to avoid emit_struct_def check
                        if g.curpltype.is_none() {
                            g.curpltype = Some(generic_types[i].as_ref().unwrap().clone());
                        }
                    }
                    i = i + 1;
                    continue;
                }
                if let PLType::GENERIC(g) = &mut *pltype.borrow_mut() {
                    g.curpltype = None;
                }
                if !eq(pltype.clone(), generic_types[i].as_ref().unwrap().clone()) {
                    return Err(ctx.add_err(self.range, ErrorCode::GENERIC_CANNOT_BE_INFER));
                }
                i = i + 1;
            }
            if sttype.need_gen_code() {
                let mp = ctx.move_generic_types();
                sttype.add_generic_type(ctx)?;
                sttype = sttype.generic_infer_pltype(ctx);
                ctx.reset_generic_types(mp);
            } else {
                return Err(ctx.add_err(self.range, ErrorCode::GENERIC_CANNOT_BE_INFER));
            }
            pltype = Rc::new(RefCell::new(PLType::STRUCT(sttype.clone())));
        }
        ctx.add_type_without_check(pltype.clone());
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
                let mp = ctx.move_generic_types();
                let generic_types = generic_params.get_generic_types(ctx)?;
                sttype.clear_generic();
                sttype.add_generic_type(ctx)?;
                if generic_params.generics.len() != sttype.generic_map.len() {
                    return Err(ctx.add_err(
                        generic_params.range.clone(),
                        ErrorCode::GENERIC_PARAM_LEN_MISMATCH,
                    ));
                }
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
                ctx.reset_generic_types(mp);
                return Ok(true);
            }
            return Err(ctx.add_err(self.range, ErrorCode::NOT_GENERIC_TYPE));
        }
        return Ok(eq(self.get_type(ctx)?, right));
    }
}

#[range]
#[fmt]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayTypeNameNode {
    pub id: Box<TypeNodeEnum>,
    pub size: Box<NodeEnum>,
}

impl TypeNode for ArrayTypeNameNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ArrayTypeNameNode");
        self.id.print(tabs + 1, false, line.clone());
        self.size.print(tabs + 1, true, line.clone());
    }
    fn get_type<'a, 'ctx>(&self, ctx: &mut Ctx<'a, 'ctx>) -> TypeNodeResult {
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
#[fmt]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PointerTypeNode {
    pub elm: Box<TypeNodeEnum>,
}

impl TypeNode for PointerTypeNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("PointerTypeNode");
        self.elm.print(tabs + 1, true, line.clone());
    }
    fn get_type<'a, 'ctx>(&self, ctx: &mut Ctx<'a, 'ctx>) -> TypeNodeResult {
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
#[fmt]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TypedIdentifierNode {
    pub id: VarNode,
    pub typenode: Box<TypeNodeEnum>,
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
        self.typenode.print(tabs + 1, true, line.clone());
    }
}

#[range]
#[fmt]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct StructDefNode {
    pub precom: Vec<Box<NodeEnum>>,
    pub doc: Vec<Box<NodeEnum>>,
    pub id: Box<VarNode>,
    pub fields: Vec<(Box<TypedIdentifierNode>, bool)>,
    pub generics: Option<Box<GenericDefNode>>,
}

impl Node for StructDefNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StructDefNode");
        tab(tabs + 1, line.clone(), false);
        println!("id: {}", self.id.name);
        for c in self.precom.iter() {
            c.print(tabs + 1, false, line.clone());
        }
        let mut i = self.fields.len();
        for (field, _) in &self.fields {
            i -= 1;
            field.print(tabs + 1, i == 0, line.clone());
        }
    }

    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult {
        ctx.emit_comment_highlight(&self.precom);
        ctx.push_semantic_token(self.id.range, SemanticTokenType::STRUCT, 0);
        if let Some(generics) = &mut self.generics {
            generics.emit(ctx)?;
        }
        for (field, has_semi) in self.fields.iter() {
            ctx.push_semantic_token(field.id.range, SemanticTokenType::PROPERTY, 0);
            field.typenode.emit_highlight(ctx);
            if !has_semi {
                ctx.add_err(field.range, ErrorCode::COMPLETION);
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
        ctx.llbuilder
            .borrow()
            .opaque_struct_type(&ctx.plmod.get_full_name(&self.id.name));
        _ = ctx.add_type(self.id.name.clone(), stu, self.id.range);
    }

    pub fn emit_struct_def<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> Result<(), PLDiag> {
        let mp = ctx.move_generic_types();
        let mut fields = FxHashMap::<String, Field>::default();
        let mut order_fields = Vec::<Field>::new();
        let mut i = 0;
        // add generic type before field add type
        if let Some(generics) = &mut self.generics {
            let generic_map = generics.gen_generic_type(ctx);
            for (name, pltype) in generic_map.iter() {
                ctx.add_generic_type(
                    name.clone(),
                    pltype.clone(),
                    pltype.clone().borrow().get_range().unwrap(),
                );
            }
        }
        let pltype = ctx.get_type(&self.id.name.as_str(), self.range)?;
        let clone_map = ctx.plmod.types.clone();
        for (field, has_semi) in self.fields.iter() {
            if !has_semi {
                ctx.add_err(field.range, ErrorCode::COMPLETION);
            }
            let id = field.id.clone();
            let f = Field {
                index: i,
                typenode: field.typenode.clone(),
                name: field.id.name.clone(),
                range: field.range,
                refs: Rc::new(RefCell::new(vec![])),
            };
            let tpre = field.typenode.get_type(ctx);
            if tpre.is_err() {
                continue;
            }
            let tp = tpre.unwrap();
            match &*tp.borrow() {
                PLType::STRUCT(sttp) => {
                    ctx.send_if_go_to_def(field.typenode.range(), sttp.range, sttp.path.clone());
                }
                _ => {}
            };

            ctx.set_if_refs(f.refs.clone(), field.id.range);
            fields.insert(id.name.to_string(), f.clone());
            order_fields.push(f);
            ctx.set_if_refs_tp(tp.clone(), field.typenode.range());
            i = i + 1;
        }
        let newf = order_fields.clone();
        if self.generics.is_none() {
            ctx.llbuilder.borrow().add_body_to_struct_type(
                &ctx.plmod.get_full_name(&self.id.name),
                &order_fields,
                ctx,
            );
        }
        ctx.plmod.types = clone_map;
        if let PLType::STRUCT(st) = &mut *pltype.borrow_mut() {
            st.fields = fields;
            st.ordered_fields = newf;
            st.doc = self.doc.clone();
        }
        ctx.set_if_refs_tp(pltype.clone(), self.id.range);
        ctx.add_doc_symbols(pltype.clone());
        ctx.save_if_comment_doc_hover(self.range, Some(self.doc.clone()));
        ctx.reset_generic_types(mp);
        Ok(())
    }
}

#[range]
#[fmt]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct StructInitFieldNode {
    pub id: VarNode,
    pub exp: Box<NodeEnum>,
}

impl Node for StructInitFieldNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StructInitFieldNode");
        tab(tabs + 1, line.clone(), false);
        println!("id: {}", self.id.name);
        self.exp.print(tabs + 1, true, line.clone());
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult {
        let (v, tp, _) = self.exp.emit(ctx)?;
        return Ok((v, tp, TerminatorEnum::NONE));
    }
}

#[range]
#[fmt]
#[comments]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct StructInitNode {
    pub generic_params: Option<Box<GenericParamNode>>,
    pub typename: Box<TypeNodeEnum>,
    pub fields: Vec<Box<StructInitFieldNode>>, // TODO: comment db and salsa comment struct
}

impl Node for StructInitNode {
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
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult {
        self.typename.emit_highlight(ctx);
        let pltype = self.typename.get_type(ctx)?;
        ctx.set_if_refs_tp(pltype.clone(), self.typename.range());
        let mut sttype = match &mut *pltype.clone().borrow_mut() {
            PLType::STRUCT(s) => s.clone(),
            _ => unreachable!(),
        };
        ctx.send_if_go_to_def(self.typename.range(), sttype.range, sttype.path.clone());
        let mp = ctx.move_generic_types();
        sttype.clear_generic();
        sttype.add_generic_type(ctx)?;
        ctx.save_if_comment_doc_hover(self.typename.range(), Some(sttype.doc.clone()));
        let mut field_init_values = vec![];
        let mut idx = 0;
        for fieldinit in self.fields.iter_mut() {
            let field_id_range = fieldinit.id.range;
            let field_exp_range = fieldinit.exp.range();
            let field = sttype.fields.get(&fieldinit.id.name);
            if field.is_none() {
                ctx.if_completion(self.range, || sttype.get_completions(&ctx));
                return Err(ctx.add_err(field_id_range, ErrorCode::STRUCT_FIELD_NOT_FOUND));
            }
            let field = field.unwrap();
            let (value, value_pltype, _) = fieldinit.emit(ctx)?;
            idx += 1;
            ctx.emit_comment_highlight(&self.comments[idx - 1]);
            if value.is_none() || value_pltype.is_none() {
                return Err(ctx.add_err(field_exp_range, ErrorCode::EXPECT_VALUE));
            }
            let (value, _) =
                ctx.try_load2var(field_exp_range, value.unwrap(), value_pltype.unwrap())?;
            let value_pltype = value_pltype.unwrap();
            if !field.typenode.eq_or_infer(ctx, value_pltype)? {
                return Err(ctx.add_err(fieldinit.range, ErrorCode::STRUCT_FIELD_TYPE_NOT_MATCH));
            }
            field_init_values.push((field.index, value));
            ctx.set_if_refs(field.refs.clone(), field_id_range);
        }
        if self.fields.len() < self.comments.len() {
            ctx.emit_comment_highlight(&self.comments[idx]);
        }
        if !sttype.generic_map.is_empty() {
            if sttype.need_gen_code() {
                sttype = sttype.generic_infer_pltype(ctx);
            } else {
                return Err(ctx.add_err(self.typename.range(), ErrorCode::GENERIC_CANNOT_BE_INFER));
            }
        }
        let pltype = Rc::new(RefCell::new(PLType::STRUCT(sttype.clone())));
        // let tp = sttype.struct_type(ctx).as_basic_type_enum().clone();
        let struct_pointer =
            ctx.llbuilder
                .borrow()
                .alloc("initstruct", &PLType::STRUCT(sttype), ctx); //alloc(ctx, tp, "initstruct");
        field_init_values.iter().for_each(|(index, value)| {
            let fieldptr = ctx
                .llbuilder
                .borrow()
                .build_struct_gep(struct_pointer, *index, "fieldptr")
                .unwrap();
            ctx.llbuilder.borrow().build_store(fieldptr, *value);
        });
        ctx.reset_generic_types(mp);
        return Ok((
            Some(plv!(struct_pointer)),
            Some(pltype.clone()),
            TerminatorEnum::NONE,
        ));
    }
}

#[range]
#[fmt]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ArrayInitNode {
    // pub tp: Box<TypeNameNode>,
    pub exps: Vec<Box<NodeEnum>>,
}

impl Node for ArrayInitNode {
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
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult {
        let mut exps = Vec::new();
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
            let tp = tp.unwrap();
            exps.push(ctx.try_load2var(range, v.unwrap(), tp)?);
        }
        if tp0.is_none() {
            return Err(ctx.add_err(self.range, ErrorCode::ARRAY_INIT_EMPTY));
        }
        let tp = exps[0].1;
        let sz = exps.len() as u32;
        let arr = ctx.llbuilder.borrow().alloc(
            "array_alloca",
            &PLType::ARR(ARRType {
                element_type: tp,
                size: exps.len() as u32,
            }),
            ctx,
        );

        for (i, (v, _)) in exps.into_iter().enumerate() {
            let ptr = ctx
                .llbuilder
                .borrow()
                .build_const_in_bounds_gep(arr, &[0, 1], "elem_ptr");
            ctx.llbuilder.borrow().build_store(ptr, v);
        }
        return Ok((
            Some(plv!(arr)),
            Some(Rc::new(RefCell::new(PLType::ARR(ARRType {
                element_type: tp0.unwrap(),
                size: sz,
            })))),
            TerminatorEnum::NONE,
        ));
    }
}

#[range]
#[fmt]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct GenericDefNode {
    pub generics: Vec<Box<VarNode>>,
}
impl Node for GenericDefNode {
    fn print(&self, _tabs: usize, _end: bool, _line: Vec<bool>) {
        todo!()
    }

    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult {
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
#[fmt]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct GenericParamNode {
    pub generics: Vec<Option<Box<TypeNodeEnum>>>,
}
impl Node for GenericParamNode {
    fn print(&self, _tabs: usize, _end: bool, _line: Vec<bool>) {
        todo!()
    }

    fn emit<'a, 'ctx>(&mut self, _: &mut Ctx<'a, 'ctx>) -> NodeResult {
        return Ok((None, None, TerminatorEnum::NONE));
    }
}
impl GenericParamNode {
    pub fn get_generic_types<'a, 'ctx>(
        &self,
        ctx: &mut Ctx<'a, 'ctx>,
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
