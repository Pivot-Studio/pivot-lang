use std::cell::RefCell;

use std::sync::Arc;

use super::interface::TraitBoundNode;
use super::node_result::NodeResultBuilder;
use super::primary::VarNode;
use super::*;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::ast::ctx::Ctx;
use crate::ast::ctx::EqRes;
use crate::ast::diag::ErrorCode;

use crate::ast::plmod::MutVec;
use crate::ast::pltype::get_type_deep;
use crate::ast::pltype::ClosureType;
use crate::ast::pltype::{ARRType, Field, GenericType, PLType, STType};
use crate::ast::tokens::TokenType;
use crate::ast::traits::CustomType;
use indexmap::IndexMap;

use internal_macro::node;
use linked_hash_map::LinkedHashMap;
use lsp_types::SemanticTokenType;
use rustc_hash::FxHashSet;
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
    pub fn get_origin_type_with_infer<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> TypeNodeResult {
        if self.id.is_none() {
            ctx.if_completion(self.range, || ctx.get_type_completions());
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::EXPECT_TYPE)));
        }
        let pltype = self
            .id
            .as_ref()
            .unwrap()
            .get_type(ctx)?
            .get_value()
            .unwrap()
            .get_ty();
        ctx.if_completion(self.range, || ctx.get_type_completions());

        match &*pltype.clone().borrow() {
            PLType::Struct(sttype) | PLType::Trait(sttype) => {
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
                    for (i, st_generic_type) in sttype.generic_map.values().enumerate() {
                        if generic_types[i].is_none() {
                            continue;
                        }
                        let res = ctx.eq(
                            st_generic_type.clone(),
                            generic_types[i].as_ref().unwrap().clone(),
                        );
                        if !res.eq {
                            let mut diag = generic_params.generics[i]
                                .as_ref()
                                .unwrap()
                                .range()
                                .new_err(ErrorCode::TYPE_MISMATCH);
                            if let Some(reason) = res.reason {
                                diag.add_help(&reason);
                            }
                            return Err(diag.add_to_ctx(ctx));
                        }
                    }
                }
                let ret = if sttype.is_trait {
                    Arc::new(RefCell::new(PLType::Trait(sttype)))
                } else {
                    Arc::new(RefCell::new(PLType::Struct(sttype)))
                };
                ctx.linked_tp_tbl
                    .entry(pltype.as_ptr())
                    .or_insert(vec![])
                    .push(ret.clone());
                Ok(ret)
            }
            PLType::Union(untype) => {
                let untype = untype.new_pltype();
                if let Some(generic_params) = &self.generic_params {
                    let generic_types = generic_params.get_generic_types(ctx, builder)?;
                    if generic_params.generics.len() != untype.generic_map.len() {
                        return Err(ctx.add_diag(
                            generic_params
                                .range
                                .new_err(ErrorCode::GENERIC_PARAM_LEN_MISMATCH),
                        ));
                    }
                    for (i, un_generic_type) in untype.generic_map.values().enumerate() {
                        if generic_types[i].is_none() {
                            continue;
                        }
                        let res = ctx.eq(
                            un_generic_type.clone(),
                            generic_types[i].as_ref().unwrap().clone(),
                        );
                        if !res.eq {
                            let mut diag = generic_params.generics[i]
                                .as_ref()
                                .unwrap()
                                .range()
                                .new_err(ErrorCode::TYPE_MISMATCH);
                            if let Some(reason) = res.reason {
                                diag.add_help(&reason);
                            }

                            return Err(diag.add_to_ctx(ctx));
                        }
                    }
                }
                let ret = Arc::new(RefCell::new(PLType::Union(untype)));
                ctx.linked_tp_tbl
                    .entry(pltype.as_ptr())
                    .or_insert(vec![])
                    .push(ret.clone());
                Ok(ret)
            }
            _ => Ok(pltype),
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
    fn emit_highlight(&self, ctx: &mut Ctx) {
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
    fn get_type<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
        gen_code: bool,
    ) -> TypeNodeResult {
        let pltype = self.get_origin_type_with_infer(ctx, builder)?;
        if self.generic_params.is_some() && gen_code {
            match &*pltype.borrow() {
                PLType::Struct(sttype) | PLType::Trait(sttype) => {
                    let sttype = sttype.clone();
                    if sttype.need_gen_code() {
                        return ctx.protect_generic_context(&sttype.generic_map, |ctx| {
                            ctx.run_in_type_mod(&sttype, |ctx, sttype| {
                                sttype.gen_code(ctx, builder)
                            })
                        });
                    } else {
                        return Err(
                            ctx.add_diag(self.range.new_err(ErrorCode::GENERIC_CANNOT_BE_INFER))
                        );
                    }
                }
                PLType::Union(untype) => {
                    let mut untype = untype.clone();
                    if untype.need_gen_code() {
                        untype = ctx.protect_generic_context(&untype.generic_map, |ctx| {
                            untype.gen_code(ctx, builder)
                        })?;
                        let pltype = Arc::new(RefCell::new(PLType::Union(untype)));
                        return Ok(pltype);
                    } else {
                        return Err(
                            ctx.add_diag(self.range.new_err(ErrorCode::GENERIC_CANNOT_BE_INFER))
                        );
                    }
                }
                _ => unreachable!(),
            };
        }
        Ok(pltype)
    }

    fn eq_or_infer<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        right: Arc<RefCell<PLType>>,
        builder: &'b BuilderEnum<'a, '_>,
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
                    reason: None,
                });
            }
            if let (PLType::Struct(left), PLType::Struct(right)) =
                (&*left.borrow(), &*right.borrow())
            {
                return Ok(EqRes {
                    eq: !left.generic_map.iter().any(|(k, l_type)| {
                        !ctx.eq(
                            l_type.clone(),
                            right.generic_infer_types.get(k).unwrap().clone(),
                        )
                        .eq
                    }),
                    need_up_cast: false,
                    reason: None,
                });
            } else if let (PLType::Union(left), PLType::Union(right)) =
                (&*left.borrow(), &*right.borrow())
            {
                return ctx.protect_generic_context(&left.generic_map, |ctx| {
                    for (l, r) in left.sum_types.iter().zip(right.sum_types.iter()) {
                        let r_type = r.get_type(ctx, builder, true)?;
                        if !l.eq_or_infer(ctx, r_type, builder)?.eq {
                            return Ok(EqRes {
                                eq: false,
                                need_up_cast: false,
                                reason: None,
                            });
                        }
                    }
                    Ok(EqRes {
                        eq: true,
                        need_up_cast: false,
                        reason: None,
                    })
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
}

impl PrintTrait for ArrayTypeNameNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ArrayTypeNameNode");
        self.id.print(tabs + 1, true, line.clone());
    }
}

impl TypeNode for ArrayTypeNameNode {
    fn get_type<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
        gen_code: bool,
    ) -> TypeNodeResult {
        let pltype = self.id.get_type(ctx, builder, gen_code)?;
        let arrtype = ARRType {
            element_type: pltype,
            size_handle: 0,
        };
        let arrtype = Arc::new(RefCell::new(PLType::Arr(arrtype)));
        Ok(arrtype)
    }

    fn emit_highlight<'ctx>(&self, ctx: &mut Ctx<'_>) {
        self.id.emit_highlight(ctx);
    }

    fn eq_or_infer<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        pltype: Arc<RefCell<PLType>>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> Result<EqRes, PLDiag> {
        match &*pltype.borrow() {
            PLType::Arr(a) => self.id.eq_or_infer(ctx, a.element_type.clone(), builder),
            _ => Ok(EqRes {
                eq: false,
                need_up_cast: false,
                reason: None,
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
    fn get_type<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
        gen_code: bool,
    ) -> TypeNodeResult {
        let pltype = self.elm.get_type(ctx, builder, gen_code)?;
        let pltype = Arc::new(RefCell::new(PLType::Pointer(pltype)));
        Ok(pltype)
    }

    fn emit_highlight<'ctx>(&self, ctx: &mut Ctx<'_>) {
        self.elm.emit_highlight(ctx);
    }

    fn eq_or_infer<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        pltype: Arc<RefCell<PLType>>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> Result<EqRes, PLDiag> {
        match &*pltype.borrow() {
            PLType::Pointer(p) => self.elm.eq_or_infer(ctx, p.clone(), builder),
            _ => Ok(EqRes {
                eq: false,
                need_up_cast: false,
                reason: None,
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
    pub pre_comments: Vec<Box<NodeEnum>>,
    pub doc: Vec<Box<NodeEnum>>,
    pub id: Box<VarNode>,
    pub fields: Vec<StructField>,
    pub generics: Option<Box<GenericDefNode>>,
    pub modifier: Option<(TokenType, Range)>,
}
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct StructField {
    pub id: Box<TypedIdentifierNode>,
    pub has_semi: bool,
    pub modifier: Option<(TokenType, Range)>,
}

impl PrintTrait for StructDefNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StructDefNode");
        tab(tabs + 1, line.clone(), false);
        println!("id: {}", self.id.name);
        for c in self.pre_comments.iter() {
            c.print(tabs + 1, false, line.clone());
        }
        let mut i = self.fields.len();
        for field in &self.fields {
            i -= 1;
            field.id.print(tabs + 1, i == 0, line.clone());
        }
    }
}

impl Node for StructDefNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        _builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        ctx.emit_comment_highlight(&self.pre_comments);
        ctx.push_semantic_token(self.id.range, SemanticTokenType::STRUCT, 0);
        if let Some(generics) = &mut self.generics {
            generics.emit_highlight(ctx);
        }
        for field in self.fields.iter() {
            ctx.push_semantic_token(field.id.id.range, SemanticTokenType::PROPERTY, 0);
            field.id.typenode.emit_highlight(ctx);
            if !field.has_semi {
                ctx.add_diag(field.id.range.new_err(ErrorCode::COMPLETION));
            }
            if let Some(doc) = &field.id.doc {
                ctx.push_semantic_token(doc.range, SemanticTokenType::COMMENT, 0);
            }
        }
        Ok(Default::default())
    }
}

impl StructDefNode {
    pub fn add_to_symbols<'a, 'b>(&self, ctx: &'b mut Ctx<'a>, builder: &'b BuilderEnum<'a, '_>) {
        let generic_map = if let Some(generics) = &self.generics {
            let mp = generics.gen_generic_type(ctx);
            _ = generics.set_traits(ctx, builder, &mp);
            mp
        } else {
            IndexMap::default()
        };
        let stu = Arc::new(RefCell::new(PLType::Struct(STType {
            name: self.id.name.clone(),
            path: ctx.plmod.path.clone(),
            fields: LinkedHashMap::new(),
            range: self.id.range(),
            doc: vec![],
            generic_map,
            derives: vec![],
            modifier: self.modifier,
            body_range: self.range(),
            is_trait: false,
            is_tuple: false,
            generic_infer_types: Default::default(),
            methods: Default::default(),
            // generic_infer: Default::default(),
            trait_methods_impl: Default::default(),
        })));
        if self.generics.is_none() {
            builder.opaque_struct_type(&ctx.plmod.get_full_name(&self.id.name));
        }
        _ = ctx.add_type(self.id.name.clone(), stu, self.id.range);
    }

    pub fn emit_struct_def<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> Result<(), PLDiag> {
        let pltype = ctx.get_type(self.id.name.as_str(), self.id.range)?;
        for f in self.fields.iter() {
            let id = &f.id;
            // 自引用检查
            if let TypeNodeEnum::Basic(b) = &*id.typenode {
                if let Some(id) = &b.id {
                    if id.ns.is_empty() {
                        // 只有本包内类型可能自引用
                        let v = ctx
                            .self_ref_map
                            .entry(id.id.name.clone())
                            .or_insert(FxHashSet::default());
                        v.insert((self.id.name.clone(), self.id.range()));
                        ctx.check_self_ref(&id.id.name, id.range)?;
                    }
                }
            }
        }
        let generic_map = if let PLType::Struct(st) = &mut *pltype.borrow_mut() {
            st.generic_map.clone()
        } else {
            IndexMap::default()
        };
        ctx.protect_generic_context(&generic_map, |ctx| {
            let mut fields = LinkedHashMap::new();
            let mut field_pltps = vec![];
            let clone_map = ctx.plmod.types.clone();
            for (i, field) in self.fields.iter().enumerate() {
                if !field.has_semi {
                    ctx.add_diag(field.id.range.new_err(ErrorCode::COMPLETION));
                }
                let id = field.id.id.clone();
                let f = Field {
                    index: i as u32 + 1,
                    typenode: field.id.typenode.clone(),
                    name: id.name.clone(),
                    range: field.id.id.range,
                    modifier: field.modifier,
                };
                let tpre = field
                    .id
                    .typenode
                    .get_type(ctx, builder, self.generics.is_none());
                if tpre.is_err() {
                    continue;
                }
                let tp = tpre.unwrap();
                field_pltps.push(tp.clone());
                ctx.set_field_refs(pltype.tp.clone(), &f, f.range);
                ctx.send_if_go_to_def(f.range, f.range, ctx.plmod.path.clone());
                fields.insert(id.name.to_string(), f.clone());
            }
            ctx.plmod.types = clone_map;
            if let PLType::Struct(st) = &mut *pltype.borrow_mut() {
                st.fields = fields.clone();
                st.doc = self.doc.clone();
                if let Some(stpltype) = ctx.linked_tp_tbl.remove(&pltype.tp.as_ptr()) {
                    for st in stpltype {
                        if let PLType::Struct(st) = &mut *st.borrow_mut() {
                            st.fields = fields.clone();
                            st.doc = self.doc.clone();
                        }
                    }
                }
            }
            if let PLType::Struct(st) = &*pltype.borrow() {
                if self.generics.is_none() {
                    builder.add_body_to_struct_type(
                        &ctx.plmod.get_full_name(&self.id.name),
                        st,
                        ctx,
                    );
                    // gen st vist function must be called after add_body_to_struct_type
                    builder.gen_st_visit_function(ctx, st, &field_pltps);
                }
            }
            ctx.set_if_refs_tp(pltype.tp.clone(), self.id.range);
            ctx.add_doc_symbols(pltype.tp.clone());
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
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        ctx.push_semantic_token(self.id.range(), SemanticTokenType::PROPERTY, 0);
        self.exp.emit(ctx, builder)
    }
}

#[node]
pub struct StructInitNode {
    pub typename: Box<TypeNodeEnum>,
    pub fields: Vec<Box<NodeEnum>>, // TODO: comment db and salsa comment struct
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
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        self.typename.emit_highlight(ctx);
        let mut pltype = match &*self.typename {
            TypeNodeEnum::Basic(b) => b.get_origin_type_with_infer(ctx, builder)?,
            _ => unreachable!(),
        };
        ctx.set_if_refs_tp(pltype.clone(), self.typename.range());
        let mut sttype = match &*pltype.clone().borrow() {
            PLType::Struct(s) => s.clone(),
            _ => {
                return Err(self
                    .range
                    .new_err(ErrorCode::EXPECT_STRUCT_TYPE)
                    .add_to_ctx(ctx))
            }
        };
        ctx.send_if_go_to_def(self.typename.range(), sttype.range, sttype.path.clone());
        let mut field_init_values = vec![];
        let mut idx = 0;
        ctx.save_if_comment_doc_hover(self.typename.range(), Some(sttype.doc.clone()));
        ctx.run_in_type_mod_mut(&mut sttype, |ctx, sttype| {
            for fieldinit in self.fields.iter_mut() {
                if let NodeEnum::STInitField(fieldinit) = &mut **fieldinit {
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
                    let v = fieldinit.emit(ctx, builder)?.get_value();
                    idx += 1;
                    if v.is_none() {
                        return Err(ctx.add_diag(field_exp_range.new_err(ErrorCode::EXPECT_VALUE)));
                    }
                    let v = v.unwrap();
                    let value = ctx.try_load2var(field_exp_range, v.get_value(), builder)?;
                    let value_pltype = v.get_ty();
                    ctx.protect_generic_context(&sttype.generic_map, |ctx| {
                        if !field
                            .typenode
                            .eq_or_infer(ctx, value_pltype.clone(), builder)?
                            .eq
                        {
                            return Err(ctx.add_diag(
                                fieldinit
                                    .range()
                                    .new_err(ErrorCode::STRUCT_FIELD_TYPE_NOT_MATCH),
                            ));
                        }
                        Ok(())
                    })?;
                    field_init_values.push((field.index, value));
                    ctx.send_if_go_to_def(field_id_range, field.range, sttype.get_path());
                    ctx.set_field_refs(pltype.clone(), field, field_id_range);
                } else if let NodeEnum::Err(fieldinit) = &mut **fieldinit {
                    if !fieldinit.src.contains(':') {
                        ctx.if_completion(fieldinit.range(), || sttype.get_completions(ctx));
                    }
                    let _ = fieldinit.emit(ctx, builder);
                } else {
                    unreachable!()
                }
            }
            if !sttype.generic_map.is_empty() {
                if sttype.need_gen_code() {
                    pltype = ctx
                        .run_in_type_mod_mut(sttype, |ctx, sttype| sttype.gen_code(ctx, builder))?;
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

        let struct_pointer = builder.alloc("initstruct", &pltype.borrow(), ctx, None); //alloc(ctx, tp, "initstruct");
        field_init_values.iter().for_each(|(index, value)| {
            let fieldptr = builder
                .build_struct_gep(struct_pointer, *index, "fieldptr")
                .unwrap();
            builder.build_store(fieldptr, *value);
        });
        struct_pointer.new_output(pltype.clone()).to_result()
    }
}

#[node]
pub struct ArrayInitNode {
    pub tp: Option<(Box<TypeNodeEnum>, Box<NodeEnum>)>,
    pub exps: Vec<Box<NodeEnum>>,
}

impl PrintTrait for ArrayInitNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ArrayInitNode");
        if let Some((tp, len)) = &self.tp {
            tp.print(tabs + 1, false, line.clone());
            len.print(tabs + 1, false, line.clone());
        }
        let mut i = self.exps.len();
        for exp in &self.exps {
            i -= 1;
            exp.print(tabs + 1, i == 0, line.clone());
        }
    }
}

impl Node for ArrayInitNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        let mut exps = Vec::new();
        let mut tp0 = None;

        for exp in self.exps.iter_mut() {
            let range = exp.range();
            let v = exp.emit(ctx, builder)?.get_value();
            // 检查类型是否一致
            if tp0.is_none() {
                tp0 = v.as_ref().map(|v| v.get_ty());
            } else if tp0 != v.as_ref().map(|v| v.get_ty()) {
                return Err(ctx.add_diag(range.new_err(ErrorCode::ARRAY_TYPE_NOT_MATCH)));
            }
            let v = v.unwrap();
            let tp = v.get_ty();
            exps.push((ctx.try_load2var(range, v.get_value(), builder)?, tp));
        }
        let sz = exps.len() as u64;
        let (tp, size_handle) = if let Some((tp, len_v)) = &mut self.tp {
            tp.emit_highlight(ctx);
            let tp = tp.get_type(ctx, builder, true)?;
            let len = len_v.emit(ctx, builder)?.get_value().unwrap();
            if !matches!(&*len.get_ty().borrow(), PLType::Primitive(PriType::I64)) {
                return Err(ctx.add_diag(len_v.range().new_err(ErrorCode::ARRAY_LEN_MUST_BE_I64)));
            }
            let len = ctx.try_load2var(len_v.range(), len.get_value(), builder)?;
            if let Some(tp0) = &tp0 {
                if !ctx.eq(tp.clone(), tp0.clone()).total_eq() {
                    return Err(ctx.add_diag(self.range.new_err(ErrorCode::ARRAY_TYPE_NOT_MATCH)));
                }
            }
            (tp, len)
        } else {
            if tp0.is_none() {
                return Err(ctx.add_diag(self.range.new_err(ErrorCode::ARRAY_INIT_EMPTY)));
            }
            let tp = exps[0].clone().1;
            (tp, builder.int_value(&PriType::I64, sz, true))
        };
        let arr_tp = Arc::new(RefCell::new(PLType::Arr(ARRType {
            element_type: tp,
            size_handle,
        })));
        let arr = builder.alloc("array_alloca", &arr_tp.borrow(), ctx, None);
        let real_arr = builder.build_struct_gep(arr, 1, "real_arr").unwrap();

        let real_arr = builder.build_load(real_arr, "load_arr");
        for (i, (v, _)) in exps.into_iter().enumerate() {
            let ptr = builder.build_const_in_bounds_gep(real_arr, &[i as u64], "elem_ptr");
            builder.build_store(ptr, v);
        }
        arr.new_output(arr_tp).to_result()
    }
}

#[node]
pub struct GenericDefNode {
    pub generics: Vec<Box<TraitBoundNode>>,
    pub generics_size: usize,
}

impl PrintTrait for GenericDefNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("GenericDefNode");
        let mut i = self.generics.len();
        for g in &self.generics {
            i -= 1;
            g.generic.print(tabs + 1, i == 0, line.clone());
        }
    }
}

impl Node for GenericDefNode {
    fn emit<'a, 'b>(
        &mut self,
        _ctx: &'b mut Ctx<'a>,
        _builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        Ok(Default::default())
    }
}
impl GenericDefNode {
    pub fn emit_highlight(&self, ctx: &mut Ctx) {
        for g in self.generics.iter() {
            g.emit_highlight(ctx);
        }
    }
    pub fn set_traits<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
        generic_map: &IndexMap<String, Arc<RefCell<PLType>>>,
    ) -> Result<(), PLDiag> {
        for g in self.generics.iter() {
            g.set_traits(ctx, builder, generic_map)?;
        }
        Ok(())
    }
    pub fn gen_generic_type(&self, ctx: &Ctx) -> IndexMap<String, Arc<RefCell<PLType>>> {
        let mut res = IndexMap::default();
        for g in self.generics.iter() {
            let range = g.generic.range;
            let name = g.generic.name.clone();
            let gentype = GenericType {
                name: name.clone(),
                range,
                curpltype: None,
                trait_impl: None,
                trait_place_holder: None,
                refs: Arc::new(MutVec::new(vec![])),
            };
            let pltp = Arc::new(RefCell::new(PLType::Generic(gentype)));
            ctx.send_if_go_to_def(range, range, ctx.get_file());
            ctx.set_if_refs_tp(pltp.clone(), range);
            res.insert(name, pltp);
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
    fn emit<'a, 'b>(&mut self, _: &'b mut Ctx<'a>, _: &'b BuilderEnum<'a, '_>) -> NodeResult {
        Ok(Default::default())
    }
}
impl GenericParamNode {
    pub fn get_generic_types<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> Result<Vec<Option<Arc<RefCell<PLType>>>>, PLDiag> {
        let mut res = vec![];
        for g in self.generics.iter() {
            if g.is_none() {
                res.push(None);
                continue;
            }
            res.push(Some(get_type_deep(
                g.as_ref().unwrap().get_type(ctx, builder, true)?,
            )));
        }
        Ok(res)
    }
    pub fn emit_highlight(&self, ctx: &mut Ctx) {
        for g in self.generics.iter() {
            if g.is_some() {
                g.as_ref().unwrap().emit_highlight(ctx);
            }
        }
    }
}

#[node]
pub struct ClosureTypeNode {
    pub arg_types: Vec<Box<TypeNodeEnum>>,
    pub ret_type: Box<TypeNodeEnum>,
}

impl TypeNode for ClosureTypeNode {
    fn get_type<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
        gen_code: bool,
    ) -> TypeNodeResult {
        let mut arg_types = vec![];
        for g in self.arg_types.iter() {
            arg_types.push(g.get_type(ctx, builder, gen_code)?);
        }
        let ret_type = self.ret_type.get_type(ctx, builder, gen_code)?;
        Ok(Arc::new(RefCell::new(PLType::Closure(ClosureType {
            arg_types,
            ret_type,
            range: self.range,
        }))))
    }

    fn emit_highlight(&self, ctx: &mut Ctx) {
        for g in self.arg_types.iter() {
            g.emit_highlight(ctx);
        }
        self.ret_type.emit_highlight(ctx);
    }

    fn eq_or_infer<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        pltype: Arc<RefCell<PLType>>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> Result<EqRes, PLDiag> {
        if let PLType::Closure(closure) = &*pltype.borrow() {
            if self.arg_types.len() != closure.arg_types.len() {
                return Ok(EqRes {
                    eq: false,
                    need_up_cast: false,
                    reason: Some("closure arg len not match".to_string()),
                });
            }
            for (i, arg) in closure.arg_types.iter().enumerate() {
                if !self.arg_types[i].eq_or_infer(ctx, arg.clone(), builder)?.eq {
                    return Ok(EqRes {
                        eq: false,
                        need_up_cast: false,
                        reason: Some("closure arg type not match".to_string()),
                    });
                }
            }
            if !self
                .ret_type
                .eq_or_infer(ctx, closure.ret_type.clone(), builder)?
                .eq
            {
                return Ok(EqRes {
                    eq: false,
                    need_up_cast: false,
                    reason: Some("closure ret type not match".to_string()),
                });
            }
            return Ok(EqRes {
                eq: true,
                need_up_cast: false,
                reason: None,
            });
        }
        Ok(EqRes {
            eq: false,
            need_up_cast: false,
            reason: None,
        })
    }
}

impl PrintTrait for ClosureTypeNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ClosureTypeNode");
        tab(tabs, line.clone(), false);
        println!("arg_types:");
        deal_line(tabs + 1, &mut line, false);
        for g in &self.arg_types {
            g.print(tabs + 1, false, line.clone());
        }
        tab(tabs, line.clone(), false);
        println!("ret_type:");
        deal_line(tabs + 1, &mut line, false);
        self.ret_type.print(tabs + 1, true, line.clone());
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CustomTypeNode {
    pub name: String,
    pub range: Range,
    pub path: String,
}

impl TypeNode for CustomTypeNode {
    fn get_type<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        _builder: &'b BuilderEnum<'a, '_>,
        _gen_code: bool,
    ) -> TypeNodeResult {
        let m = ctx.get_mod(&self.path);
        let re = ctx.get_type(&self.name, self.range);
        if let Ok(tp) = re {
            return Ok(tp.tp);
        }
        let tp = ctx.get_type_in_mod(&m, &self.name, self.range)?;
        Ok(tp.tp)
    }

    fn emit_highlight(&self, _ctx: &mut Ctx) {
        todo!()
    }

    fn eq_or_infer<'a, 'b>(
        &self,
        _ctx: &'b mut Ctx<'a>,
        _pltype: Arc<RefCell<PLType>>,
        _builder: &'b BuilderEnum<'a, '_>,
    ) -> Result<EqRes, PLDiag> {
        let eq = get_type_deep(_pltype).borrow().get_full_elm_name()
            == format!("{}..{}", self.path, self.name);
        Ok(EqRes {
            eq,
            need_up_cast: false,
            reason: None,
        })
    }
}

impl RangeTrait for CustomTypeNode {
    fn range(&self) -> Range {
        self.range
    }
}

impl PrintTrait for CustomTypeNode {
    fn print(&self, _tabs: usize, _end: bool, _line: Vec<bool>) {
        todo!()
    }
}

impl FmtTrait for CustomTypeNode {
    fn format(&self, _builder: &mut FmtBuilder) {
        _builder.token(&self.name)
    }
}
