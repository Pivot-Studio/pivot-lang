use std::cell::RefCell;

use std::sync::Arc;

use super::primary::VarNode;
use super::*;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::ast::ctx::Ctx;
use crate::ast::ctx::EqRes;
use crate::ast::diag::ErrorCode;

use crate::ast::pltype::get_type_deep;
use crate::ast::pltype::{ARRType, Field, GenericType, PLType, STType};
use crate::ast::tokens::TokenType;
use crate::plv;
use indexmap::IndexMap;

use internal_macro::node;
use lsp_types::SemanticTokenType;
use rustc_hash::FxHashMap;
#[node]
pub struct TypeNameNode {
    pub id: Option<ExternIdNode>,
    pub generic_params: Option<Box<GenericParamNode>>,
}

impl TypeNameNode {
    pub fn new_from_str(s: &str) -> Self {
        let id = ExternIdNode {
            id: Box::new(VarNode {
                name: s.to_string(),
                range: Default::default(),
            }),
            range: Default::default(),
            ns: vec![],
            complete: true,
            singlecolon: false,
        };
        Self {
            id: Some(id),
            generic_params: None,
            range: Default::default(),
        }
    }
    pub fn get_origin_type_with_infer<'a, 'b, 'ctx>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> TypeNodeResult {
        if self.id.is_none() {
            ctx.if_completion(self.range, || ctx.get_type_completions());
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::EXPECT_TYPE)));
        }
        let (_, pltype, _) = self.id.as_ref().unwrap().get_type(ctx)?;
        ctx.if_completion(self.range, || ctx.get_type_completions());
        let pltype = pltype.unwrap();
        if let PLType::STRUCT(sttype) = &*pltype.clone().borrow() {
            let sttype = sttype.new_pltype();
            if let Some(generic_params) = &self.generic_params {
                let generic_types = generic_params.get_generic_types(ctx, builder)?;
                if generic_params.generics.len() != sttype.generic_map.len() {
                    return Err(ctx.add_diag(
                        generic_params
                            .range
                            .new_err(ErrorCode::GENERIC_PARAM_LEN_MISMATCH),
                    ));
                }
                let mut i = 0;
                for (_, st_generic_type) in sttype.generic_map.iter() {
                    if generic_types[i].is_none() {
                        i += 1;
                        continue;
                    }
                    if st_generic_type == generic_types[i].as_ref().unwrap() {
                        if let PLType::GENERIC(g) = &mut *st_generic_type.borrow_mut() {
                            // self ref to avoid emit_struct_def check
                            if g.curpltype.is_none() {
                                g.curpltype = Some(generic_types[i].as_ref().unwrap().clone());
                            }
                        }
                        i += 1;
                        continue;
                    }
                    if let PLType::GENERIC(g) = &mut *st_generic_type.borrow_mut() {
                        g.curpltype = None;
                    }
                    if !ctx
                        .eq(
                            st_generic_type.clone(),
                            generic_types[i].as_ref().unwrap().clone(),
                        )
                        .eq
                    {
                        return Err(
                            ctx.add_diag(self.range.new_err(ErrorCode::GENERIC_CANNOT_BE_INFER))
                        );
                    }
                    i += 1;
                }
            }
            Ok(Arc::new(RefCell::new(PLType::STRUCT(sttype))))
        } else {
            Ok(pltype)
        }
    }
}

impl PrintTrait for TypeNameNode {
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
}

impl TypeNode for TypeNameNode {
    fn emit_highlight<'a, 'ctx>(&self, ctx: &mut Ctx<'a>) {
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

    fn get_type<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> TypeNodeResult {
        let mut pltype = self.get_origin_type_with_infer(ctx, builder)?;
        if self.generic_params.is_some() {
            let mut sttype = match &*pltype.borrow() {
                PLType::STRUCT(s) => s.clone(),
                _ => unreachable!(),
            };
            if sttype.need_gen_code() {
                sttype = ctx.protect_generic_context(&sttype.generic_map, |ctx| {
                    Ok(sttype.gen_code(ctx, builder))
                })?;
                pltype = Arc::new(RefCell::new(PLType::STRUCT(sttype)));
                return Ok(pltype);
            } else {
                return Err(ctx.add_diag(self.range.new_err(ErrorCode::GENERIC_CANNOT_BE_INFER)));
            }
        }
        Ok(pltype)
    }

    fn eq_or_infer<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        right: Arc<RefCell<PLType>>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<EqRes, PLDiag> {
        let left = self.get_origin_type_with_infer(ctx, builder)?;
        if self.generic_params.is_some() {
            // name not match
            if left.borrow().get_name()
                != right.borrow().get_name().split('<').collect::<Vec<_>>()[0]
            {
                return Ok(EqRes {
                    eq: false,
                    need_up_cast: false,
                });
            }
            if let (PLType::STRUCT(left), PLType::STRUCT(right)) =
                (&*left.borrow(), &*right.borrow())
            {
                return ctx.protect_generic_context(&left.generic_map, |ctx| {
                    for (k, leftfield) in left.fields.iter() {
                        let rightpltype = right
                            .fields
                            .get(k)
                            .unwrap()
                            .typenode
                            .get_type(ctx, builder)
                            .unwrap();
                        if !leftfield
                            .typenode
                            .eq_or_infer(ctx, rightpltype, builder)?
                            .eq
                        {
                            return Ok(EqRes {
                                eq: false,
                                need_up_cast: false,
                            });
                        }
                    }
                    return Ok(EqRes {
                        eq: true,
                        need_up_cast: false,
                    });
                });
            }
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::NOT_GENERIC_TYPE)));
        }
        Ok(ctx.eq(left, right))
    }
}

#[node]
pub struct ArrayTypeNameNode {
    pub id: Box<TypeNodeEnum>,
    pub size: Box<NodeEnum>,
}

impl PrintTrait for ArrayTypeNameNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ArrayTypeNameNode");
        self.id.print(tabs + 1, false, line.clone());
        self.size.print(tabs + 1, true, line.clone());
    }
}

impl TypeNode for ArrayTypeNameNode {
    fn get_type<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> TypeNodeResult {
        if let NodeEnum::Num(num) = *self.size {
            if let Num::INT(sz) = num.value {
                let pltype = self.id.get_type(ctx, builder)?;
                let arrtype = ARRType {
                    element_type: pltype,
                    size: sz as u32,
                };
                let arrtype = Arc::new(RefCell::new(PLType::ARR(arrtype)));
                return Ok(arrtype);
            }
        }
        Err(ctx.add_diag(self.range.new_err(ErrorCode::SIZE_MUST_BE_INT)))
    }

    fn emit_highlight<'a, 'ctx>(&self, ctx: &mut Ctx<'a>) {
        self.id.emit_highlight(ctx);
    }

    fn eq_or_infer<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        pltype: Arc<RefCell<PLType>>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<EqRes, PLDiag> {
        match &*pltype.borrow() {
            PLType::ARR(a) => {
                if let NodeEnum::Num(num) = *self.size {
                    if let Num::INT(size) = num.value {
                        if a.size as u64 != size {
                            return Ok(EqRes {
                                eq: false,
                                need_up_cast: false,
                            });
                        }
                        return self.id.eq_or_infer(ctx, a.element_type.clone(), builder);
                    }
                }
                Err(ctx.add_diag(self.range.new_err(ErrorCode::SIZE_MUST_BE_INT)))
            }
            _ => Ok(EqRes {
                eq: false,
                need_up_cast: false,
            }),
        }
    }
}

#[node]
pub struct PointerTypeNode {
    pub elm: Box<TypeNodeEnum>,
}

impl PrintTrait for PointerTypeNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("PointerTypeNode");
        self.elm.print(tabs + 1, true, line.clone());
    }
}

impl TypeNode for PointerTypeNode {
    fn get_type<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> TypeNodeResult {
        let pltype = self.elm.get_type(ctx, builder)?;
        let pltype = Arc::new(RefCell::new(PLType::POINTER(pltype)));
        Ok(pltype)
    }

    fn emit_highlight<'a, 'ctx>(&self, ctx: &mut Ctx<'a>) {
        self.elm.emit_highlight(ctx);
    }

    fn eq_or_infer<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        pltype: Arc<RefCell<PLType>>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<EqRes, PLDiag> {
        match &*pltype.borrow() {
            PLType::POINTER(p) => self.elm.eq_or_infer(ctx, p.clone(), builder),
            _ => Ok(EqRes {
                eq: false,
                need_up_cast: false,
            }),
        }
    }
}

#[node]
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

#[node]
pub struct StructDefNode {
    pub precom: Vec<Box<NodeEnum>>,
    pub doc: Vec<Box<NodeEnum>>,
    pub id: Box<VarNode>,
    pub fields: Vec<(Box<TypedIdentifierNode>, bool, Option<(TokenType, Range)>)>,
    pub generics: Option<Box<GenericDefNode>>,
    pub modifier: Option<(TokenType, Range)>,
}

impl PrintTrait for StructDefNode {
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
        for (field, _, _) in &self.fields {
            i -= 1;
            field.print(tabs + 1, i == 0, line.clone());
        }
    }
}

impl Node for StructDefNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        _builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        ctx.emit_comment_highlight(&self.precom);
        ctx.push_semantic_token(self.id.range, SemanticTokenType::STRUCT, 0);
        if let Some(generics) = &mut self.generics {
            generics.emit_highlight(ctx);
        }
        for (field, has_semi, _) in self.fields.iter() {
            ctx.push_semantic_token(field.id.range, SemanticTokenType::PROPERTY, 0);
            field.typenode.emit_highlight(ctx);
            if !has_semi {
                ctx.add_diag(field.range.new_err(ErrorCode::COMPLETION));
            }
            if let Some(doc) = &field.doc {
                ctx.push_semantic_token(doc.range, SemanticTokenType::COMMENT, 0);
            }
        }
        Ok((None, None, TerminatorEnum::NONE))
    }
}

impl StructDefNode {
    pub fn add_to_symbols<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) {
        let generic_map = self
            .generics
            .as_ref()
            .map_or(IndexMap::default(), |generics| generics.gen_generic_type());
        let stu = Arc::new(RefCell::new(PLType::STRUCT(STType {
            name: self.id.name.clone(),
            path: ctx.plmod.path.clone(),
            fields: FxHashMap::default(),
            ordered_fields: vec![],
            range: self.range(),
            doc: vec![],
            generic_map,
            derives: vec![],
            modifier: self.modifier,
        })));
        builder.opaque_struct_type(&ctx.plmod.get_full_name(&self.id.name));
        _ = ctx.add_type(self.id.name.clone(), stu, self.id.range);
    }

    pub fn emit_struct_def<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<(), PLDiag> {
        let generic_map = self
            .generics
            .as_ref()
            .map_or(IndexMap::default(), |generics| generics.gen_generic_type());
        ctx.protect_generic_context(&generic_map, |ctx| {
            let mut fields = FxHashMap::<String, Field>::default();
            let mut order_fields = Vec::<Field>::new();
            // gcrtti fields
            let vtable_field = Field {
                index: 0,
                typenode: Box::new(TypeNameNode::new_from_str("u64").into()),
                name: "_vtable".to_string(),
                range: Default::default(),
                modifier: None,
            };
            fields.insert("_vtable".to_string(), vtable_field.clone());
            order_fields.push(vtable_field);
            let mut i = 1;
            let mut field_pltps = vec![];
            let pltype = ctx.get_type(self.id.name.as_str(), self.range)?;
            let clone_map = ctx.plmod.types.clone();
            for (field, has_semi, modifier) in self.fields.iter() {
                if !has_semi {
                    ctx.add_diag(field.range.new_err(ErrorCode::COMPLETION));
                }
                let id = field.id.clone();
                let f = Field {
                    index: i,
                    typenode: field.typenode.clone(),
                    name: field.id.name.clone(),
                    range: field.range,
                    modifier: *modifier,
                };
                let tpre = field.typenode.get_type(ctx, builder);
                if tpre.is_err() {
                    continue;
                }
                let tp = tpre.unwrap();
                field_pltps.push(tp.clone());
                match &*tp.borrow() {
                    PLType::STRUCT(sttp) => {
                        ctx.send_if_go_to_def(
                            field.typenode.range(),
                            sttp.range,
                            sttp.path.clone(),
                        );
                    }
                    _ => {}
                };
                ctx.set_field_refs(pltype.clone(), &f, f.range);
                ctx.send_if_go_to_def(f.range, f.range, ctx.plmod.path.clone());
                fields.insert(id.name.to_string(), f.clone());
                order_fields.push(f);
                ctx.set_if_refs_tp(tp.clone(), field.typenode.range());
                i += 1;
            }
            let newf = order_fields.clone();
            if self.generics.is_none() {
                builder.add_body_to_struct_type(
                    &ctx.plmod.get_full_name(&self.id.name),
                    &order_fields,
                    ctx,
                );
            }
            ctx.plmod.types = clone_map;
            if let PLType::STRUCT(st) = &mut *pltype.borrow_mut() {
                builder.gen_st_visit_function(ctx, st, &field_pltps);
                st.fields = fields;
                st.ordered_fields = newf;
                st.doc = self.doc.clone();
            }
            ctx.set_if_refs_tp(pltype.clone(), self.id.range);
            ctx.add_doc_symbols(pltype.clone());
            ctx.save_if_comment_doc_hover(self.range, Some(self.doc.clone()));
            Ok(())
        })
    }
}

#[node]
pub struct StructInitFieldNode {
    pub id: VarNode,
    pub exp: Box<NodeEnum>,
}

impl PrintTrait for StructInitFieldNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StructInitFieldNode");
        tab(tabs + 1, line.clone(), false);
        println!("id: {}", self.id.name);
        self.exp.print(tabs + 1, true, line.clone());
    }
}

impl Node for StructInitFieldNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        let (v, tp, _) = self.exp.emit(ctx, builder)?;
        Ok((v, tp, TerminatorEnum::NONE))
    }
}

#[node(comment)]
pub struct StructInitNode {
    pub typename: Box<TypeNodeEnum>,
    pub fields: Vec<Box<StructInitFieldNode>>, // TODO: comment db and salsa comment struct
}

impl PrintTrait for StructInitNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StructInitNode");
        self.typename
            .print(tabs + 1, self.fields.is_empty(), line.clone());
        let mut i = self.fields.len();
        for field in &self.fields {
            i -= 1;
            field.print(tabs + 1, i == 0, line.clone());
        }
    }
}

impl Node for StructInitNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        self.typename.emit_highlight(ctx);
        let mut pltype = match &*self.typename {
            TypeNodeEnum::BasicTypeNode(b) => b.get_origin_type_with_infer(ctx, builder)?,
            _ => unreachable!(),
        };
        ctx.set_if_refs_tp(pltype.clone(), self.typename.range());
        let mut sttype = match &*pltype.clone().borrow() {
            PLType::STRUCT(s) => s.clone(),
            _ => {
                return Err(self
                    .range
                    .new_err(ErrorCode::EXPECT_STRUCT_TYPE)
                    .add_to_ctx(ctx))
            }
        };
        ctx.send_if_go_to_def(self.typename.range(), sttype.range, sttype.path.clone());
        ctx.protect_generic_context(&sttype.generic_map.clone(), |ctx| {
            let mut field_init_values = vec![];
            let mut idx = 0;
            ctx.save_if_comment_doc_hover(self.typename.range(), Some(sttype.doc.clone()));
            ctx.run_in_st_mod_mut(&mut sttype, |ctx, sttype| {
                for fieldinit in self.fields.iter_mut() {
                    let field_id_range = fieldinit.id.range;
                    let field_exp_range = fieldinit.exp.range();
                    let field = sttype.fields.get(&fieldinit.id.name);
                    if field.is_none() {
                        ctx.if_completion(self.range, || sttype.get_completions(ctx));
                        return Err(
                            ctx.add_diag(field_id_range.new_err(ErrorCode::STRUCT_FIELD_NOT_FOUND))
                        );
                    }
                    let field = field.unwrap();
                    let (value, value_pltype, _) = fieldinit.emit(ctx, builder)?;
                    idx += 1;
                    ctx.emit_comment_highlight(&self.comments[idx - 1]);
                    if value.is_none() || value_pltype.is_none() {
                        return Err(ctx.add_diag(field_exp_range.new_err(ErrorCode::EXPECT_VALUE)));
                    }
                    let value = ctx.try_load2var(field_exp_range, value.unwrap(), builder)?;
                    let value_pltype = value_pltype.unwrap();
                    if !field.typenode.eq_or_infer(ctx, value_pltype, builder)?.eq {
                        return Err(ctx.add_diag(
                            fieldinit
                                .range
                                .new_err(ErrorCode::STRUCT_FIELD_TYPE_NOT_MATCH),
                        ));
                    }
                    field_init_values.push((field.index, value));
                    ctx.set_field_refs(pltype.clone(), field, field_id_range);
                }
                if !sttype.generic_map.is_empty() {
                    if sttype.need_gen_code() {
                        pltype = Arc::new(RefCell::new(PLType::STRUCT(
                            ctx.run_in_st_mod_mut(sttype, |ctx, sttype| {
                                Ok(sttype.gen_code(ctx, builder))
                            })?,
                        )));
                    } else {
                        return Err(ctx.add_diag(
                            self.typename
                                .range()
                                .new_err(ErrorCode::GENERIC_CANNOT_BE_INFER),
                        ));
                    }
                }
                Ok(())
            })?;

            if self.fields.len() < self.comments.len() {
                ctx.emit_comment_highlight(&self.comments[idx]);
            }
            let struct_pointer = builder.alloc("initstruct", &pltype.borrow(), ctx, None); //alloc(ctx, tp, "initstruct");
            field_init_values.iter().for_each(|(index, value)| {
                let fieldptr = builder
                    .build_struct_gep(struct_pointer, *index, "fieldptr")
                    .unwrap();
                builder.build_store(fieldptr, *value);
            });
            Ok((
                Some(plv!(struct_pointer)),
                Some(pltype.clone()),
                TerminatorEnum::NONE,
            ))
        })
    }
}

#[node]
pub struct ArrayInitNode {
    // pub tp: Box<TypeNameNode>,
    pub exps: Vec<Box<NodeEnum>>,
}

impl PrintTrait for ArrayInitNode {
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
}

impl Node for ArrayInitNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        let mut exps = Vec::new();
        let mut tp0 = None;

        for exp in self.exps.iter_mut() {
            let range = exp.range();
            let (v, tp, _) = exp.emit(ctx, builder)?;
            // 检查类型是否一致
            if tp0.is_none() {
                tp0 = tp.clone();
            } else if tp0 != tp {
                return Err(ctx.add_diag(range.new_err(ErrorCode::ARRAY_TYPE_NOT_MATCH)));
            }
            let tp = tp.unwrap();
            exps.push((ctx.try_load2var(range, v.unwrap(), builder)?, tp));
        }
        if tp0.is_none() {
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::ARRAY_INIT_EMPTY)));
        }
        let tp = exps[0].clone().1;
        let sz = exps.len() as u32;
        let arr = builder.alloc(
            "array_alloca",
            &PLType::ARR(ARRType {
                element_type: tp,
                size: exps.len() as u32,
            }),
            ctx,
            None,
        );
        let real_arr = builder.build_struct_gep(arr, 1, "real_arr").unwrap();

        for (i, (v, _)) in exps.into_iter().enumerate() {
            let ptr = builder.build_const_in_bounds_gep(real_arr, &[0, i as u64], "elem_ptr");
            builder.build_store(ptr, v);
        }
        Ok((
            Some(plv!(arr)),
            Some(Arc::new(RefCell::new(PLType::ARR(ARRType {
                element_type: tp0.unwrap(),
                size: sz,
            })))),
            TerminatorEnum::NONE,
        ))
    }
}

#[node]
pub struct GenericDefNode {
    pub generics: Vec<Box<VarNode>>,
}

impl PrintTrait for GenericDefNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("GenericDefNode");
        let mut i = self.generics.len();
        for g in &self.generics {
            i -= 1;
            g.print(tabs + 1, i == 0, line.clone());
        }
    }
}

impl Node for GenericDefNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        _ctx: &'b mut Ctx<'a>,
        _builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        Ok((None, None, TerminatorEnum::NONE))
    }
}
impl GenericDefNode {
    pub fn emit_highlight<'a, 'ctx, 'b>(&self, ctx: &'b mut Ctx<'a>) {
        for g in self.generics.iter() {
            ctx.push_semantic_token(g.range, SemanticTokenType::TYPE, 0);
        }
    }
    pub fn gen_generic_type<'a, 'ctx>(&self) -> IndexMap<String, Arc<RefCell<PLType>>> {
        let mut res = IndexMap::default();
        for g in self.generics.iter() {
            let range = g.range;
            let name = g.name.clone();
            let gentype = GenericType {
                name: name.clone(),
                range,
                curpltype: None,
            };
            res.insert(name, Arc::new(RefCell::new(PLType::GENERIC(gentype))));
        }
        res
    }
}

#[node]
pub struct GenericParamNode {
    pub generics: Vec<Option<Box<TypeNodeEnum>>>,
}

impl PrintTrait for GenericParamNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("GenericParamNode");
        let mut i = self.generics.len();
        for g in &self.generics {
            i -= 1;
            if g.is_none() {
                tab(tabs + 1, line.clone(), i == 0);
                println!("None");
            } else {
                g.as_ref().unwrap().print(tabs + 1, i == 0, line.clone());
            }
        }
    }
}

impl Node for GenericParamNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        _: &'b mut Ctx<'a>,
        _: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        Ok((None, None, TerminatorEnum::NONE))
    }
}
impl GenericParamNode {
    pub fn get_generic_types<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<Vec<Option<Arc<RefCell<PLType>>>>, PLDiag> {
        let mut res = vec![];
        for g in self.generics.iter() {
            if g.is_none() {
                res.push(None);
                continue;
            }
            res.push(Some(get_type_deep(
                g.as_ref().unwrap().get_type(ctx, builder)?,
            )));
        }
        Ok(res)
    }
    pub fn emit_highlight<'a, 'ctx>(&self, ctx: &mut Ctx<'a>) {
        for g in self.generics.iter() {
            if g.is_some() {
                g.as_ref().unwrap().emit_highlight(ctx);
            }
        }
    }
}
