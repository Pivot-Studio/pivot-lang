//! # Inference
//!
//! This module is used to do type inference.
//!
//! ## How it works
//!
//! The basic idea is that most of statements
//! have type constraints, for example,
//!
//! ```pl
//! a = b
//! ```
//!
//! The type of `a` and `b` shall be the same.
//!
//! So we can use a unify table to record the type relationship.
//! It's not always necessary to generate all the type constraints,
//! as type inference will only take effect when the type is unknown.
//!
//! > What is unify table?
//! >
//! > Unify table is very much like a hashtable, but it can map
//! > multiple keys to the same value. In type inference, one
//! > variable has many constraints, and different variables'
//! > constraints may be the same. So we use unify table to
//! > record the constraints.
use std::{cell::RefCell, sync::Arc};

use ena::unify::{UnificationTable, UnifyKey, UnifyValue};
use linked_hash_map::LinkedHashMap;
use rustc_hash::FxHashMap;

use crate::ast::{
    builder::{BuilderEnum, IRBuilder},
    ctx::Ctx,
    node::{
        pointer::PointerOpEnum,
        statement::{DefVar, StatementsNode},
        tuple::{new_tuple_field, new_tuple_type},
        types::{GenericParamNode, TypeNameNode},
        NodeEnum, TypeNode, TypeNodeEnum,
    },
    pltype::{get_type_deep, ARRType, ClosureType, ImplAble, PLType, STType, TraitImplAble},
    tokens::TokenType,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TyVariable {
    id: u32,
}

pub trait GenericInferenceAble {
    fn get_inference_result(&self) -> &Option<Vec<TyVariable>>;
    fn get_generic_params(&self) -> &Option<Box<GenericParamNode>>;
}

impl UnifyKey for TyVariable {
    type Value = TyInfer;

    fn index(&self) -> u32 {
        self.id
    }

    fn from_index(u: u32) -> Self {
        Self { id: u }
    }

    fn tag() -> &'static str {
        "TyVariable"
    }
}

/// # The type inference result
///
/// ## Term
///
/// A `Term`` is a `PLType`. When it's `PLType::Unknown`,
/// it means that the type is not inferred yet. An `Unknown`
/// type unify with any other type will become the other type.
///
/// ## Err
///
/// If an inference error occurs, the type will be `Err`.
///
/// `Err`'s type is `PLType::Unknown`.
/// However, it's not the same as `Term(PLType::Unknown)`.
/// When `Err` unify with any other type, it will always become `Err`.
///
/// ## Closure
///
/// A `Closure` is a function type. It contains a list of argument types,
/// and a return type.
///
/// As the function type may not be inferred yet, the argument types and return type
/// are all `TyVariable`, which allows them to unify with other `TyVariable`s.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyInfer {
    Err,
    Term(Arc<RefCell<PLType>>),
    Generic((Vec<TyVariable>, GenericTy)),
    // Pointer(TyVariable),
}

/// # GenericTy
///
/// In inference, we treat
/// closure and tuple as generic type as well
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GenericTy {
    Closure,
    Tuple,
    St(STType),
    Pointer,
    Array,
}

impl UnifyValue for TyInfer {
    fn unify_values(value1: &Self, value2: &Self) -> Result<Self, (Self, Self)> {
        if matches!(value1, TyInfer::Err) || matches!(value2, TyInfer::Err) {
            return Ok(TyInfer::Err);
        }

        // if there's no error, then set unknown to the real type
        if value1 == value2
            || matches!((value1,value2), (TyInfer::Term(t1), TyInfer::Term(t2)) if get_type_deep(t1.clone()) == get_type_deep(t2.clone()))
        {
            Ok(value1.clone())
        } else if matches!(value1, TyInfer::Term(ty) if !get_type_deep(ty.clone()).borrow().is_complete())
        {
            Ok(value2.clone())
        } else if matches!(value2, TyInfer::Term(ty) if !get_type_deep(ty.clone()).borrow().is_complete())
            || matches!(value2, TyInfer::Generic(_))
            || matches!(value1, TyInfer::Generic(_))
        {
            Ok(value1.clone())
        } else if matches!(value1, TyInfer::Term(ty) if matches!(&*get_type_deep(ty.clone()).borrow(), PLType::Trait(_)|PLType::Union(_)))
        {
            // implicit cast
            Err((value1.clone(), value2.clone()))
        } else {
            Ok(TyInfer::Err)
        }
    }
}

impl TyInfer {
    pub fn get_type<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
        unify_table: &mut UnificationTable<TyVariable>,
    ) -> Arc<RefCell<PLType>> {
        let cur_bb = builder.get_cur_basic_block();
        let re = self.get_type_inner(ctx, builder, unify_table);
        ctx.position_at_end(cur_bb, builder);
        re
    }

    fn get_type_inner<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
        unify_table: &mut UnificationTable<TyVariable>,
    ) -> Arc<RefCell<PLType>> {
        match self {
            TyInfer::Term(ty) => ty.clone(),
            TyInfer::Generic((gen, gty)) => {
                match gty {
                    GenericTy::Closure => {
                        // last in gen is ret ty, others are arg ty
                        let args = &gen[..gen.len() - 1];
                        let ty = &gen[gen.len() - 1];
                        let mut argtys = vec![];
                        for arg in args {
                            argtys.push(unify_table.probe(*arg).get_type(
                                ctx,
                                builder,
                                unify_table,
                            ));
                        }
                        let ret_ty = unify_table.probe(*ty).get_type(ctx, builder, unify_table);
                        Arc::new(RefCell::new(PLType::Closure(ClosureType {
                            arg_types: argtys,
                            ret_type: ret_ty,
                            range: Default::default(),
                        })))
                    }
                    GenericTy::Tuple => {
                        let mut fields = LinkedHashMap::default();
                        let mut name = "".to_string();
                        for (i, arg) in gen.iter().enumerate() {
                            if i != 0 {
                                name += ", ";
                            }
                            let ty = unify_table.probe(*arg).get_type(ctx, builder, unify_table);
                            fields.insert(
                                i.to_string(),
                                new_tuple_field(
                                    i,
                                    &ty.borrow(),
                                    &ty.borrow().get_path().unwrap_or("".to_string()),
                                ),
                            );
                            name += &ty.borrow().get_llvm_name();
                        }
                        name = format!("({})", name);

                        Arc::new(RefCell::new(PLType::Struct(new_tuple_type(
                            name,
                            fields,
                            Default::default(),
                        ))))
                    }
                    GenericTy::St(st) => {
                        let mut partial = false;
                        let mut st = st.clone();
                        for (i, (_, v)) in st.generic_map.iter_mut().enumerate() {
                            let t = unify_table.probe(*gen.get(i).unwrap()).get_type(
                                ctx,
                                builder,
                                unify_table,
                            );
                            if *t.borrow() == PLType::Unknown {
                                // return unknown_arc();
                                partial = true;
                            }
                            match &mut *v.borrow_mut() {
                                PLType::Generic(g) => g.curpltype = Some(t),
                                _ => unreachable!(),
                            };
                        }
                        let st = ctx.run_in_type_mod(&st, |ctx, st| {
                            st.gen_code(ctx, builder).unwrap_or(unknown_arc())
                        });
                        if partial {
                            return new_arc_refcell(PLType::PartialInferred(st));
                        }
                        st
                    }
                    GenericTy::Pointer => {
                        let p = gen[0];
                        let infer = unify_table.probe(p);
                        let ty = infer.get_type(ctx, builder, unify_table);
                        if !ty.borrow().is_complete() {
                            return new_arc_refcell(PLType::PartialInferred(Arc::new(
                                RefCell::new(PLType::Arr(ARRType {
                                    element_type: ty,
                                    size_handle: 0,
                                })),
                            )));
                        }
                        Arc::new(RefCell::new(PLType::Pointer(ty)))
                    }
                    GenericTy::Array => {
                        let p = gen[0];
                        let infer = unify_table.probe(p);
                        let ty = infer.get_type(ctx, builder, unify_table);
                        if !ty.borrow().is_complete() {
                            return new_arc_refcell(PLType::PartialInferred(Arc::new(
                                RefCell::new(PLType::Arr(ARRType {
                                    element_type: ty,
                                    size_handle: 0,
                                })),
                            )));
                        }
                        Arc::new(RefCell::new(PLType::Arr(ARRType {
                            element_type: ty,
                            size_handle: 0,
                        })))
                    }
                }
            }
            _ => unknown_arc(),
        }
    }
}

pub struct InferenceCtx<'ctx> {
    unify_table: Arc<RefCell<UnificationTable<TyVariable>>>,
    symbol_table: FxHashMap<String, TyVariable>,
    father: Option<&'ctx InferenceCtx<'ctx>>,
}

fn unknown() -> SymbolType {
    SymbolType::PLType(Arc::new(RefCell::new(PLType::Unknown)))
}

pub fn unknown_arc() -> Arc<RefCell<PLType>> {
    Arc::new(RefCell::new(PLType::Unknown))
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolType {
    Var(TyVariable),
    PLType(Arc<RefCell<PLType>>),
}

impl<'ctx> InferenceCtx<'ctx> {
    pub fn new(table: Arc<RefCell<UnificationTable<TyVariable>>>) -> Self {
        Self {
            unify_table: table,
            symbol_table: FxHashMap::default(),
            father: None,
        }
    }

    pub fn new_child(&'ctx self) -> Self {
        Self {
            unify_table: self.unify_table.clone(),
            symbol_table: FxHashMap::default(),
            father: Some(self),
        }
    }

    pub fn add_symbol(&mut self, name: &str, ty: TyVariable) {
        if !self.symbol_table.contains_key(name) {
            self.symbol_table.insert(name.to_string(), ty);
        }
    }

    pub fn get_symbol(&self, name: &str) -> Option<TyVariable> {
        if let Some(ty) = self.symbol_table.get(name) {
            return Some(*ty);
        }
        if let Some(father) = self.father {
            return father.get_symbol(name);
        }
        None
    }

    pub fn unify<'a, 'b>(
        &self,
        var: TyVariable,
        value: SymbolType,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) {
        self.unify_two_symbol(SymbolType::Var(var), value, ctx, builder)
    }

    pub fn unify_var_value(&self, var: TyVariable, value: TyInfer) {
        _ = self.unify_table.borrow_mut().unify_var_value(var, value);
    }

    pub fn unify_two_symbol<'a, 'b>(
        &self,
        var1: SymbolType,
        var2: SymbolType,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) {
        match (var1, var2) {
            (SymbolType::Var(v), SymbolType::Var(v2)) => {
                self.handle_unify(v, v2, ctx, builder);
            }
            (SymbolType::Var(v), SymbolType::PLType(tp)) => {
                self.unify_var_tp(v, tp, ctx, builder);
            }
            (SymbolType::PLType(tp), SymbolType::Var(v)) => {
                self.unify_var_tp(v, tp, ctx, builder);
            }
            (SymbolType::PLType(_), SymbolType::PLType(_)) => (),
        }
    }

    fn handle_unify<'a, 'b>(
        &self,
        v: TyVariable,
        v2: TyVariable,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) {
        let v_1 = self.unify_table.borrow_mut().probe(v);
        let v_2 = self.unify_table.borrow_mut().probe(v2);
        match (v_1, v_2) {
            (TyInfer::Generic(c1), TyInfer::Generic(c2)) => {
                if c1.0.len() == c2.0.len() && c1.1 == c2.1 {
                    for (i, arg) in c1.0.iter().enumerate() {
                        self.unify_two_symbol(
                            SymbolType::Var(*arg),
                            SymbolType::Var(c2.0[i]),
                            ctx,
                            builder,
                        );
                    }
                }
            }
            (TyInfer::Generic(_), TyInfer::Term(t)) => {
                self.unify_var_tp(v, t, ctx, builder);
            }
            (TyInfer::Term(t), TyInfer::Generic(_)) => {
                self.unify_var_tp(v2, t, ctx, builder);
            }
            // (TyInfer::Pointer(t1), TyInfer::Pointer(t2)) => {
            //     self.handle_unify(t1, t2, ctx, builder);
            // }
            // (TyInfer::Term(t), TyInfer::Pointer(p)) | (TyInfer::Pointer(p), TyInfer::Term(t)) => {
            //     match &*t.borrow() {
            //         PLType::Pointer(ptr_ty) => {
            //             self.unify_var_tp(p, ptr_ty.clone(), ctx, builder);
            //         }
            //         _ => (),
            //     }
            // }
            _ => (),
        };
        _ = self.unify_table.borrow_mut().unify_var_var(v, v2);
    }

    pub fn unify_var_tp<'a, 'b>(
        &self,
        var: TyVariable,
        tp: Arc<RefCell<PLType>>,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) {
        let ty = self.unify_table.borrow_mut().probe(var);
        match (ty, &*tp.borrow()) {
            (TyInfer::Generic((c1, GenericTy::Closure)), PLType::Closure(c2)) => {
                if c1.len() == c2.arg_types.len() + 1 {
                    for (t1, t2) in c1
                        .iter()
                        .zip(c2.arg_types.iter().chain(&[c2.ret_type.clone()]))
                    {
                        self.unify_var_tp(*t1, t2.clone(), ctx, builder);
                    }
                }
            }
            (TyInfer::Generic((c1, GenericTy::Closure)), PLType::Fn(c2)) => {
                if c1.len() == c2.fntype.param_pltypes.len() + 1 {
                    for (t1, t2) in c1.iter().zip(
                        c2.fntype
                            .param_pltypes
                            .iter()
                            .chain(&[c2.fntype.ret_pltype.clone()]),
                    ) {
                        self.unify_var_tp(
                            *t1,
                            t2.get_type(ctx, builder, true).unwrap_or(unknown_arc()),
                            ctx,
                            builder,
                        );
                    }
                }
            }
            (TyInfer::Generic((c1, GenericTy::Pointer)), PLType::Pointer(ptr_ty)) => {
                self.unify_var_tp(c1[0], ptr_ty.clone(), ctx, builder);
            }
            (TyInfer::Generic((c1, GenericTy::Array)), PLType::Arr(arr)) => {
                self.unify_var_tp(c1[0], arr.element_type.clone(), ctx, builder);
            }
            _ => (),
        }
        _ = self
            .unify_table
            .borrow_mut()
            .unify_var_value(var, TyInfer::Term(tp));
    }

    pub fn import_symbols(&mut self, ctx: &Ctx) {
        for (name, ty) in &ctx.table {
            // self.add_symbol(name, );
            let key = self.new_key();
            _ = self
                .unify_table
                .borrow_mut()
                .unify_var_value(key, TyInfer::Term(ty.pltype.clone()));
            self.add_symbol(name, key);
        }
    }

    #[allow(dead_code)]
    pub fn add_unknown_variable(&mut self, name: &str) {
        let key = self.new_key();
        self.add_symbol(name, key);
    }

    pub fn import_global_symbols(&mut self, ctx: &Ctx) {
        let ctx = ctx.get_root_ctx();
        for (name, ty) in &ctx.plmod.global_table {
            // self.add_symbol(name, );
            let key = self.new_key();
            self.unify_table
                .borrow_mut()
                .unify_var_value(key, TyInfer::Term(ty.tp.clone()))
                .unwrap();
            self.add_symbol(name, key);
        }
    }

    pub fn inference_statements<'a, 'b>(
        &mut self,
        node: &mut StatementsNode,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) {
        let prev = ctx.disable_diag;
        ctx.disable_diag = true;
        for s in &mut node.statements {
            self.inference(&mut *s, ctx, builder);
        }
        ctx.disable_diag = prev;
    }

    pub fn new_key(&self) -> TyVariable {
        self.unify_table
            .borrow_mut()
            .new_key(TyInfer::Term(unknown_arc()))
    }

    pub fn inference<'a, 'b>(
        &mut self,
        node: &mut NodeEnum,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> SymbolType {
        match node {
            NodeEnum::Def(d) => {
                let mut ty = unknown();
                if let Some(exp) = &mut d.exp {
                    ty = self.inference(&mut *exp, ctx, builder);
                }
                if let Some(tp) = &d.tp {
                    let new_ty = SymbolType::PLType(
                        tp.get_type(ctx, builder, true).unwrap_or(unknown_arc()),
                    );
                    ty = new_ty;
                }
                let defvar = &mut *d.var;
                self.def_inference(defvar, ty, ctx, builder);
            }
            NodeEnum::Assign(a) => {
                let ty = self.inference(&mut a.exp, ctx, builder);
                match &mut a.var {
                    crate::ast::node::statement::AssignVar::Pointer(p) => {
                        let re = self.inference(&mut *p, ctx, builder);
                        self.unify_two_symbol(re, ty, ctx, builder);
                    }
                    crate::ast::node::statement::AssignVar::Raw(d) => {
                        let defvar = &mut *d;
                        self.def_inference(defvar, ty, ctx, builder);
                    }
                }
            }
            NodeEnum::Expr(e) => match e.op.0 {
                TokenType::EQ
                | TokenType::NE
                | TokenType::LEQ
                | TokenType::GEQ
                | TokenType::GREATER
                | TokenType::LESS => {
                    let i1 = self.inference(&mut e.left, ctx, builder);
                    let i2 = self.inference(&mut e.right, ctx, builder);
                    self.unify_two_symbol(i1, i2, ctx, builder);
                    return SymbolType::PLType(new_arc_refcell(PLType::Primitive(
                        crate::ast::pltype::PriType::BOOL,
                    )));
                }
                _ => {
                    let i1 = self.inference(&mut e.left, ctx, builder);
                    let i2 = self.inference(&mut e.right, ctx, builder);
                    self.unify_two_symbol(i1.clone(), i2, ctx, builder);
                    return i1;
                }
            },
            NodeEnum::ExternIdNode(ex) => {
                if ex.ns.is_empty() {
                    if let Some(t) = self.get_symbol(&ex.id.name) {
                        let id = self.new_key();
                        ex.id.id = Some(id);
                        self.unify_two_symbol(
                            SymbolType::Var(id),
                            SymbolType::Var(t),
                            ctx,
                            builder,
                        );
                        return SymbolType::Var(id);
                    }
                }
                let plmod = &ctx.get_root_ctx().plmod;
                if let Ok(plmod) = ex.solve_mod(plmod, ctx) {
                    if let Some(t) = plmod.global_table.get(&ex.id.name) {
                        return SymbolType::PLType(t.tp.clone());
                    }
                    if let Some(r) = plmod.types.get(&ex.id.name) {
                        return SymbolType::PLType(r.tp.clone());
                    }
                }
            }
            NodeEnum::Bool(_) => {
                return SymbolType::PLType(new_arc_refcell(PLType::Primitive(
                    crate::ast::pltype::PriType::BOOL,
                )))
            }
            NodeEnum::Num(n) => match n.value {
                crate::ast::node::Num::Int(_) => {
                    return SymbolType::PLType(new_arc_refcell(PLType::Primitive(
                        crate::ast::pltype::PriType::I64,
                    )))
                }
                crate::ast::node::Num::Float(_) => {
                    return SymbolType::PLType(new_arc_refcell(PLType::Primitive(
                        crate::ast::pltype::PriType::F64,
                    )))
                }
            },
            NodeEnum::Primary(p) => {
                return self.inference(&mut p.value, ctx, builder);
            }
            NodeEnum::AsNode(a) => {
                if a.tail.is_none() || a.tail.unwrap().0 == TokenType::NOT {
                    let tp = a.ty.get_type(ctx, builder, true).unwrap_or(unknown_arc());
                    return SymbolType::PLType(tp);
                }
            }
            NodeEnum::ClosureNode(c) => {
                let mut child = self.new_child();
                let mut argtys = vec![];
                for (var, ty) in &mut c.paralist {
                    let key_id = child.new_key();
                    match ty {
                        Some(ty) => {
                            let arg_ty = ty.get_type(ctx, builder, true).unwrap_or(unknown_arc());
                            child.unify(key_id, SymbolType::PLType(arg_ty), ctx, builder);
                        }
                        None => {}
                    }
                    argtys.push(key_id);
                    child.add_symbol(&var.name, key_id);
                    var.id = Some(key_id);
                }
                let ret_ty = child.new_key();
                child.unify(
                    ret_ty,
                    SymbolType::PLType(
                        c.ret
                            .as_ref()
                            .and_then(|r| r.get_type(ctx, builder, true).ok())
                            .unwrap_or(unknown_arc()),
                    ),
                    ctx,
                    builder,
                );
                let id = child.new_key();
                argtys.push(ret_ty);
                child
                    .unify_table
                    .borrow_mut()
                    .unify_var_value(id, TyInfer::Generic((argtys, GenericTy::Closure)))
                    .unwrap();
                child.add_symbol("@ret", ret_ty);
                c.ret_id = Some(ret_ty);
                child.inference_statements(&mut c.body, ctx, builder);
                return SymbolType::Var(id);
            }
            NodeEnum::StructInit(si) => match &mut *si.typename {
                crate::ast::node::TypeNodeEnum::Basic(TypeNameNode {
                    id: Some(i),
                    generic_params: gp,
                    generic_infer: infer,
                    ..
                }) => {
                    let ty = i
                        .get_type(ctx)
                        .unwrap_or_default()
                        .get_value()
                        .unwrap_or_default()
                        .get_ty();
                    match &*ty.clone().borrow() {
                        PLType::Struct(s) => {
                            if s.generic_map.is_empty() {
                                let sym = SymbolType::PLType(ty);
                                for f in &mut si.fields {
                                    if let NodeEnum::STInitField(in_f) = &mut **f {
                                        if let Some(f) = self.symbol_field_symbol(
                                            sym.clone(),
                                            ctx,
                                            &in_f.id.name,
                                            builder,
                                        ) {
                                            let ty = self.inference(&mut in_f.exp, ctx, builder);
                                            self.unify_two_symbol(f, ty, ctx, builder);
                                        }
                                    }
                                }
                                return sym;
                            } else {
                                let mut tys = vec![];
                                for _ in &s.generic_map {
                                    tys.push(self.new_key());
                                }
                                if let Some(gp) = gp {
                                    for (i, g) in gp.generics.iter().enumerate() {
                                        if let Some(g) = g {
                                            let ty = g
                                                .get_type(ctx, builder, true)
                                                .unwrap_or(unknown_arc());
                                            self.unify(
                                                tys[i],
                                                SymbolType::PLType(ty),
                                                ctx,
                                                builder,
                                            );
                                        }
                                    }
                                }
                                let id = self.new_key();
                                self.unify_table
                                    .borrow_mut()
                                    .unify_var_value(
                                        id,
                                        TyInfer::Generic((tys.clone(), GenericTy::St(s.clone()))),
                                    )
                                    .unwrap();
                                *infer = Some(tys);
                                let sym = SymbolType::Var(id);
                                for f in &mut si.fields {
                                    if let NodeEnum::STInitField(in_f) = &mut **f {
                                        if let Some(f) = self.symbol_field_symbol(
                                            sym.clone(),
                                            ctx,
                                            &in_f.id.name,
                                            builder,
                                        ) {
                                            let ty = self.inference(&mut in_f.exp, ctx, builder);
                                            self.unify_two_symbol(f, ty, ctx, builder);
                                        }
                                    }
                                }
                                return sym;
                            }
                        }
                        _ => (),
                    };
                }
                _ => (),
            },
            NodeEnum::FuncCall(fc) => {
                let mut argtys = vec![];
                for arg in &mut fc.paralist {
                    let arg_ty = self.inference(&mut *arg, ctx, builder);
                    argtys.push(arg_ty);
                }
                let func_ty = self.inference(&mut fc.callee, ctx, builder);
                match func_ty {
                    SymbolType::Var(id) => {
                        let k = self.unify_table.borrow_mut().probe(id);
                        match k {
                            TyInfer::Err => (),
                            TyInfer::Term(t) => match &*t.borrow() {
                                PLType::Closure(c) => {
                                    if c.arg_types.len() != argtys.len() {
                                        return unknown();
                                    }

                                    for (i, arg) in c.arg_types.iter().enumerate() {
                                        self.unify_two_symbol(
                                            argtys[i].clone(),
                                            SymbolType::PLType(arg.clone()),
                                            ctx,
                                            builder,
                                        );
                                    }
                                    return SymbolType::PLType(c.ret_type.clone());
                                }
                                PLType::Fn(f) => {
                                    if let Some(value) =
                                        self.handle_fn_ty(f, fc, ctx, builder, &argtys)
                                    {
                                        return value;
                                    }
                                }
                                PLType::Unknown => {
                                    let mut arg_keys = vec![];
                                    for arg in argtys {
                                        let arg_key = self.new_key();
                                        self.unify(arg_key, arg, ctx, builder);
                                        arg_keys.push(arg_key);
                                    }
                                    let ret_ty = self.new_key();
                                    arg_keys.push(ret_ty);
                                    self.unify_table
                                        .borrow_mut()
                                        .unify_var_value(
                                            id,
                                            TyInfer::Generic((arg_keys, GenericTy::Closure)),
                                        )
                                        .unwrap();
                                    return SymbolType::Var(ret_ty);
                                }
                                _ => (),
                            },
                            TyInfer::Generic((gen, GenericTy::Closure)) => {
                                if gen.len() != argtys.len() + 1 {
                                    return unknown();
                                }
                                for (i, arg) in gen[0..gen.len() - 1].iter().enumerate() {
                                    self.unify(*arg, argtys[i].clone(), ctx, builder);
                                }
                                return SymbolType::Var(gen.last().unwrap().to_owned());
                            }
                            _ => (),
                        }
                    }
                    SymbolType::PLType(tp) => match &*tp.borrow() {
                        PLType::Fn(f) => {
                            if let Some(value) = self.handle_fn_ty(f, fc, ctx, builder, &argtys) {
                                return value;
                            }
                        }
                        PLType::Closure(cl) => {
                            if cl.arg_types.len() != argtys.len() {
                                return unknown();
                            }
                            for (i, arg) in cl.arg_types.iter().enumerate() {
                                self.unify_two_symbol(
                                    argtys[i].clone(),
                                    SymbolType::PLType(arg.clone()),
                                    ctx,
                                    builder,
                                );
                            }
                            return SymbolType::PLType(cl.ret_type.clone());
                        }
                        _ => (),
                    },
                }
            }
            NodeEnum::If(i) => {
                let cond_ty = self.inference(&mut i.cond, ctx, builder);
                self.unify_two_symbol(
                    cond_ty,
                    SymbolType::PLType(new_arc_refcell(PLType::Primitive(
                        crate::ast::pltype::PriType::BOOL,
                    ))),
                    ctx,
                    builder,
                );
                let mut child_then = self.new_child();
                child_then.inference_statements(&mut i.then, ctx, builder);
                if let Some(else_) = &mut i.els {
                    self.inference(else_, ctx, builder);
                }
            }
            NodeEnum::Sts(sts) => {
                let mut child = self.new_child();
                child.inference_statements(sts, ctx, builder);
            }
            NodeEnum::Ret(r) => {
                let ret = self.get_symbol("@ret");
                if r.yiel.is_some() {
                    return unknown();
                }
                if let Some(ret) = ret {
                    if let Some(r) = &mut r.value {
                        let ty = self.inference(&mut *r, ctx, builder);
                        self.unify(ret, ty, ctx, builder);
                    } else {
                        self.unify(
                            ret,
                            SymbolType::PLType(new_arc_refcell(PLType::Void)),
                            ctx,
                            builder,
                        );
                    }
                }
            }
            NodeEnum::While(w) => {
                let cond_ty = self.inference(&mut w.cond, ctx, builder);
                self.unify_two_symbol(
                    cond_ty,
                    SymbolType::PLType(new_arc_refcell(PLType::Primitive(
                        crate::ast::pltype::PriType::BOOL,
                    ))),
                    ctx,
                    builder,
                );
                let mut child = self.new_child();
                child.inference_statements(&mut w.body, ctx, builder);
            }
            NodeEnum::For(f) => {
                let mut child = self.new_child();
                if let Some(pre) = &mut f.pre {
                    child.inference(&mut *pre, ctx, builder);
                }
                let cond_ty = child.inference(&mut f.cond, ctx, builder);
                child.unify_two_symbol(
                    cond_ty,
                    SymbolType::PLType(new_arc_refcell(PLType::Primitive(
                        crate::ast::pltype::PriType::BOOL,
                    ))),
                    ctx,
                    builder,
                );
                if let Some(post) = &mut f.opt {
                    child.inference(&mut *post, ctx, builder);
                }

                child.inference_statements(&mut f.body, ctx, builder);
            }
            NodeEnum::Take(tk) => {
                let head = self.inference(&mut tk.head, ctx, builder);
                if tk.field.is_none() {
                    return head;
                }
                let f = &tk.field.as_ref().unwrap().name;
                if let Some(value) = self.symbol_field_symbol(head, ctx, f, builder) {
                    return value;
                }
            }
            NodeEnum::Un(u) => {
                return self.inference(&mut u.exp, ctx, builder);
            }
            NodeEnum::PointerOpNode(p) => {
                let ty = self.inference(&mut p.value, ctx, builder);
                match ty {
                    SymbolType::Var(v) => {
                        let k = self.unify_table.borrow_mut().probe(v);
                        match k {
                            TyInfer::Term(t) => {
                                if p.op == PointerOpEnum::Addr {
                                    if *t.borrow() != PLType::Unknown {
                                        return SymbolType::PLType(new_arc_refcell(
                                            PLType::Pointer(t),
                                        ));
                                    } else {
                                        let ptr = self.new_key();
                                        self.unify_var_value(
                                            ptr,
                                            TyInfer::Generic((vec![v], GenericTy::Pointer)),
                                        );
                                        return SymbolType::Var(ptr);
                                    }
                                } else if p.op == PointerOpEnum::Deref {
                                    match &*t.borrow() {
                                        PLType::Pointer(p) => {
                                            return SymbolType::PLType(p.clone());
                                        }
                                        _ => (),
                                    }
                                }
                            }
                            TyInfer::Generic((v, GenericTy::Pointer))
                                if p.op == PointerOpEnum::Deref =>
                            {
                                return SymbolType::Var(v[0]);
                            }
                            TyInfer::Generic((_, GenericTy::Pointer))
                                if p.op == PointerOpEnum::Addr =>
                            {
                                let ptr = self.new_key();
                                self.unify_var_value(
                                    ptr,
                                    TyInfer::Generic((vec![v], GenericTy::Pointer)),
                                );
                                return SymbolType::Var(ptr);
                            }
                            TyInfer::Generic(_) => {
                                if p.op == PointerOpEnum::Addr {
                                    let ptr = self.new_key();
                                    self.unify_var_value(
                                        ptr,
                                        TyInfer::Generic((vec![v], GenericTy::Pointer)),
                                    );
                                    return SymbolType::Var(ptr);
                                }
                            }
                            _ => (),
                        }
                    }
                    SymbolType::PLType(t) => {
                        if p.op == PointerOpEnum::Addr {
                            if *t.borrow() != PLType::Unknown {
                                return SymbolType::PLType(new_arc_refcell(PLType::Pointer(t)));
                            }
                        } else if p.op == PointerOpEnum::Deref {
                            match &*t.borrow() {
                                PLType::Pointer(p) => {
                                    return SymbolType::PLType(p.clone());
                                }
                                _ => (),
                            }
                        }
                    }
                }
            }
            NodeEnum::ParanthesesNode(p) => {
                return self.inference(&mut p.node, ctx, builder);
            }
            NodeEnum::TupleInitNode(ti) => {
                let mut tys = vec![];
                for t in &mut ti.exprs {
                    let ty = self.inference(&mut *t, ctx, builder);
                    let key = self.new_key();
                    self.unify(key, ty, ctx, builder);
                    tys.push(key);
                }
                let id = self.new_key();
                self.unify_table
                    .borrow_mut()
                    .unify_var_value(id, TyInfer::Generic((tys, GenericTy::Tuple)))
                    .unwrap();
                return SymbolType::Var(id);
            }
            NodeEnum::ArrayElementNode(ae) => {
                let ty = self.inference(&mut ae.arr, ctx, builder);
                match ty {
                    SymbolType::Var(v) => {
                        let k = self.unify_table.borrow_mut().probe(v);
                        match k {
                            TyInfer::Term(t) => {
                                if let PLType::Arr(arr) = &*t.borrow() {
                                    return SymbolType::PLType(arr.element_type.clone());
                                }
                            }
                            TyInfer::Generic((v, GenericTy::Array)) => {
                                return SymbolType::Var(v[0]);
                            }
                            _ => (),
                        }
                    }
                    SymbolType::PLType(t) => {
                        if let PLType::Arr(arr) = &*t.borrow() {
                            return SymbolType::PLType(arr.element_type.clone());
                        }
                    }
                }
            }
            NodeEnum::ArrayInitNode(ai) => {
                let elm_ty = ai
                    .tp
                    .as_mut()
                    .map(|(tp, _)| {
                        SymbolType::PLType(tp.get_type(ctx, builder, true).unwrap_or(unknown_arc()))
                    })
                    .unwrap_or(
                        ai.exps
                            .first_mut()
                            .map(|e| self.inference(e, ctx, builder))
                            .unwrap_or(unknown()),
                    );
                let elm_id = self.new_key();
                self.unify(elm_id, elm_ty, ctx, builder);
                let id = self.new_key();
                self.unify_var_value(id, TyInfer::Generic((vec![elm_id], GenericTy::Array)));
                return SymbolType::Var(id);
            }
            NodeEnum::StringNode(_) => {
                let tp = ctx
                    .plmod
                    .submods
                    .get("gc")
                    .map(|m| m.types.get("string").unwrap().clone())
                    .unwrap_or_else(|| ctx.plmod.types.get("string").unwrap().clone());
                return SymbolType::PLType(tp.tp.clone());
            }
            _ => (),
        }
        unknown()
    }

    fn handle_fn_ty<'a, 'b>(
        &mut self,
        f: &crate::ast::pltype::FNValue,
        fc: &mut crate::ast::node::function::FuncCallNode,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
        argtys: &Vec<SymbolType>,
    ) -> Option<SymbolType> {
        let mut tys = vec![];
        for _ in &f.fntype.generic_map {
            tys.push(self.new_key());
        }
        if let Some(gp) = &fc.generic_params {
            for (i, g) in gp.generics.iter().enumerate() {
                if let Some(g) = g {
                    let ty = g.get_type(ctx, builder, true).unwrap_or(unknown_arc());
                    self.unify(tys[i], SymbolType::PLType(ty), ctx, builder);
                }
            }
        }
        fc.generic_infer = Some(tys.clone());
        let offset = if f.fntype.method { 1 } else { 0 };
        if f.fntype.param_pltypes.len() != argtys.len() + offset {
            return Some(unknown());
        }
        let mut generic_map = FxHashMap::default();
        for (i, (arg, _)) in f.fntype.generic_map.iter().enumerate() {
            generic_map.insert(arg.clone(), tys[i]);
        }
        for (i, arg) in f.fntype.param_pltypes.iter().enumerate() {
            if i < offset {
                if let NodeEnum::Take(t) = &mut *fc.callee {
                    let head_ty = self.inference(&mut t.head, ctx, builder);
                    // on method, unify type generic
                    match head_ty {
                        SymbolType::Var(v) => {
                            let k = self.unify_table.borrow_mut().probe(v);
                            if let TyInfer::Generic((v, GenericTy::St(_))) = k {
                                let offset = tys.len() - v.len();
                                for (i, g) in v.iter().enumerate() {
                                    self.unify(*g, SymbolType::Var(tys[offset + i]), ctx, builder);
                                }
                            }
                        }
                        _ => (),
                    };
                    let sym = ctx.run_in_type_mod(f, |ctx, _| {
                        arg.solve_in_infer_generic_ctx(ctx, builder, self, &generic_map)
                            .unwrap_or(SymbolType::PLType(unknown_arc()))
                    });
                    self.unify_two_symbol(head_ty, sym, ctx, builder);
                }
                continue;
            }

            let sym = ctx.run_in_type_mod(f, |ctx, _| {
                arg.solve_in_infer_generic_ctx(ctx, builder, self, &generic_map)
                    .unwrap_or(SymbolType::PLType(unknown_arc()))
            });
            self.unify_two_symbol(argtys[i - offset].clone(), sym, ctx, builder);
        }
        Some(ctx.run_in_type_mod(f, |ctx, f| {
            f.fntype
                .ret_pltype
                .solve_in_infer_generic_ctx(ctx, builder, self, &generic_map)
                .unwrap_or(SymbolType::PLType(unknown_arc()))
        }))
    }

    fn def_inference<'a, 'b>(
        &mut self,
        defvar: &mut DefVar,
        ty: SymbolType,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) {
        match defvar {
            DefVar::Identifier(v) => {
                self.id_inference(&ty, ctx, builder, v);
            }
            DefVar::TupleDeconstruct(t) => {
                for (i, var) in t.var.iter_mut().enumerate() {
                    let ty = self.symbol_field_symbol(ty.clone(), ctx, &i.to_string(), builder);
                    if let Some(ty) = ty {
                        self.def_inference(&mut *var, ty, ctx, builder)
                    }
                }
            }
            DefVar::StructDeconstruct(t) => {
                for dec in t.var.iter_mut() {
                    match dec {
                        crate::ast::node::statement::StructFieldDeconstructEnum::Var(v) => {
                            let ty = self.symbol_field_symbol(ty.clone(), ctx, &v.name, builder);
                            if let Some(ty) = ty {
                                self.id_inference(&ty, ctx, builder, v);
                            }
                        }
                        crate::ast::node::statement::StructFieldDeconstructEnum::Taged(k, var) => {
                            let ty = self.symbol_field_symbol(ty.clone(), ctx, &k.name, builder);
                            if let Some(ty) = ty {
                                self.def_inference(&mut *var, ty, ctx, builder)
                            }
                        }
                    };
                }
            }
        }
    }

    fn id_inference<'a, 'b>(
        &mut self,
        ty: &SymbolType,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
        v: &mut crate::ast::node::primary::VarNode,
    ) {
        let id = self.new_key();
        self.unify(id, ty.clone(), ctx, builder);
        v.id = Some(id);
        self.add_symbol(&v.name, id);
    }

    fn symbol_field_symbol<'a, 'b>(
        &mut self,
        head: SymbolType,
        ctx: &'b mut Ctx<'a>,
        f: &str,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> Option<SymbolType> {
        match head {
            SymbolType::Var(v) => {
                let k = self.unify_table.borrow_mut().probe(v);
                match k {
                    TyInfer::Term(t) => {
                        let tp = ctx.auto_deref_tp(t);
                        if let Some(value) = tp_field_symbol(tp, f, ctx, builder) {
                            return Some(value);
                        }
                    }
                    TyInfer::Generic((tys, GenericTy::Tuple)) => {
                        let f = f.parse::<usize>().ok()?;
                        if f < tys.len() {
                            return Some(SymbolType::Var(tys[f]));
                        }
                    }
                    TyInfer::Generic((tys, GenericTy::St(st))) => {
                        let mut generic_map = FxHashMap::default();
                        st.generic_map.iter().enumerate().for_each(|(i, (k, _))| {
                            generic_map.insert(k.clone(), tys[i]);
                        });
                        return ctx.run_in_type_mod(&st, |ctx, st| {
                            let field = f;
                            let f = st.fields.get(field);
                            if let Some(f) = f {
                                return f.typenode.solve_in_infer_generic_ctx(
                                    ctx,
                                    builder,
                                    self,
                                    &generic_map,
                                );
                            }
                            let a = st;
                            if let Some(mthd) = a
                                .get_method(field)
                                .or(ctx.find_global_method(&a.get_full_name(), field).or(ctx
                                    .find_global_method(&a.get_full_name_except_generic(), field)))
                            {
                                return Some(SymbolType::PLType(new_arc_refcell(PLType::Fn(
                                    mthd.borrow().clone(),
                                ))));
                            }

                            None
                        });
                    }
                    _ => (),
                }
            }
            SymbolType::PLType(tp) => {
                if let Some(value) = tp_field_symbol(tp, f, ctx, builder) {
                    return Some(value);
                }
            }
        }
        None
    }
}

trait Inferable {
    fn solve_in_infer_generic_ctx<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
        infer_ctx: &mut InferenceCtx<'_>,
        generic_map: &FxHashMap<String, TyVariable>,
    ) -> Option<SymbolType>;
}

impl Inferable for TypeNodeEnum {
    fn solve_in_infer_generic_ctx<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
        infer_ctx: &mut InferenceCtx<'_>,
        generic_map: &FxHashMap<String, TyVariable>,
    ) -> Option<SymbolType> {
        match self {
            TypeNodeEnum::Basic(ty) => match ty {
                TypeNameNode {
                    id: Some(i),
                    generic_params: Some(gp),
                    ..
                } => {
                    let mut tys = vec![];
                    for g in gp.generics.iter().flatten() {
                        let ty = g
                            .solve_in_infer_generic_ctx(ctx, builder, infer_ctx, generic_map)
                            .unwrap_or(SymbolType::PLType(unknown_arc()));
                        let ty_key = infer_ctx.new_key();
                        infer_ctx.unify(ty_key, ty, ctx, builder);
                        tys.push(ty_key);
                    }
                    let id = infer_ctx.new_key();
                    let ty = i
                        .get_type(ctx)
                        .unwrap_or_default()
                        .get_value()
                        .unwrap_or_default()
                        .get_ty();
                    match &*ty.borrow() {
                        PLType::Struct(s) => {
                            infer_ctx
                                .unify_table
                                .borrow_mut()
                                .unify_var_value(
                                    id,
                                    TyInfer::Generic((tys, GenericTy::St(s.clone()))),
                                )
                                .unwrap();
                            return Some(SymbolType::Var(id));
                        }
                        _ => (),
                    };
                }
                TypeNameNode {
                    id: Some(i),
                    generic_params: None,
                    ..
                } => {
                    if i.ns.is_empty() && generic_map.contains_key(&i.id.name) {
                        return Some(SymbolType::Var(*generic_map.get(&i.id.name).unwrap()));
                    }
                }
                _ => (),
            },
            TypeNodeEnum::Pointer(p) => {
                let sym = p
                    .elm
                    .solve_in_infer_generic_ctx(ctx, builder, infer_ctx, generic_map);
                if let Some(sym) = sym {
                    let id = infer_ctx.new_key();
                    infer_ctx.unify(id, sym, ctx, builder);
                    let ptr = infer_ctx.new_key();
                    infer_ctx
                        .unify_var_value(ptr, TyInfer::Generic((vec![id], GenericTy::Pointer)));
                    return Some(SymbolType::Var(ptr));
                }
            }
            TypeNodeEnum::Closure(c) => {
                let mut tys = vec![];
                for arg_ty in &c.arg_types {
                    let arg_ty = arg_ty
                        .solve_in_infer_generic_ctx(ctx, builder, infer_ctx, generic_map)
                        .unwrap_or(SymbolType::PLType(unknown_arc()));
                    let arg_ty_key = infer_ctx.new_key();
                    infer_ctx.unify(arg_ty_key, arg_ty, ctx, builder);
                    tys.push(arg_ty_key);
                }
                let ret_ty = c
                    .ret_type
                    .solve_in_infer_generic_ctx(ctx, builder, infer_ctx, generic_map)
                    .unwrap_or(SymbolType::PLType(unknown_arc()));
                let ret_ty_key = infer_ctx.new_key();
                infer_ctx.unify(ret_ty_key, ret_ty, ctx, builder);
                tys.push(ret_ty_key);
                let id = infer_ctx.new_key();
                infer_ctx.unify_var_value(id, TyInfer::Generic((tys, GenericTy::Closure)));
                return Some(SymbolType::Var(id));
            }
            TypeNodeEnum::Array(a) => {
                let elm =
                    a.id.solve_in_infer_generic_ctx(ctx, builder, infer_ctx, generic_map)
                        .unwrap_or(SymbolType::PLType(unknown_arc()));
                let elm_key = infer_ctx.new_key();
                infer_ctx.unify(elm_key, elm, ctx, builder);
                let id = infer_ctx.new_key();
                infer_ctx.unify_var_value(id, TyInfer::Generic((vec![elm_key], GenericTy::Array)));
                return Some(SymbolType::Var(id));
            }
            TypeNodeEnum::Tuple(t) => {
                let mut tys = vec![];
                for ty in &t.tps {
                    let ty = ty
                        .solve_in_infer_generic_ctx(ctx, builder, infer_ctx, generic_map)
                        .unwrap_or(SymbolType::PLType(unknown_arc()));
                    let ty_key = infer_ctx.new_key();
                    infer_ctx.unify(ty_key, ty, ctx, builder);
                    tys.push(ty_key);
                }
                let id = infer_ctx.new_key();
                infer_ctx.unify_var_value(id, TyInfer::Generic((tys, GenericTy::Tuple)));
                return Some(SymbolType::Var(id));
            }
            _ => (),
        };
        self.get_type(ctx, builder, true)
            .ok()
            .map(SymbolType::PLType)
    }
}

fn tp_field_symbol<'a, 'b>(
    tp: Arc<RefCell<PLType>>,
    field: &str,
    ctx: &'b mut Ctx<'a>,
    builder: &'b BuilderEnum<'a, '_>,
) -> Option<SymbolType> {
    match &*tp.borrow() {
        PLType::Struct(a) => {
            return ctx.run_in_type_mod(a, |ctx, a| {
                ctx.protect_generic_context(&a.generic_map, |ctx| {
                    let f = a.fields.get(field);
                    if let Some(f) = f {
                        return Some(SymbolType::PLType(
                            f.typenode
                                .get_type(ctx, builder, true)
                                .unwrap_or(unknown_arc()),
                        ));
                    }

                    if let Some(mthd) = a.get_method(field).or(ctx
                        .find_global_method(&a.get_full_name(), field)
                        .or(ctx.find_global_method(&a.get_full_name_except_generic(), field)))
                    {
                        return Some(SymbolType::PLType(new_arc_refcell(PLType::Fn(
                            mthd.borrow().clone(),
                        ))));
                    }
                    None
                })
            });
        }
        _ => (),
    };
    None
}

fn new_arc_refcell<T>(t: T) -> Arc<RefCell<T>> {
    Arc::new(RefCell::new(t))
}
