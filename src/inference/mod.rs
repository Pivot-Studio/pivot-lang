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
use rustc_hash::FxHashMap;

use crate::ast::{
    builder::BuilderEnum,
    ctx::Ctx,
    node::{
        pointer::PointerOpEnum,
        statement::{DefVar, StatementsNode},
        NodeEnum, TypeNode,
    },
    pltype::{get_type_deep, ClosureType, PLType},
    tokens::TokenType,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TyVariable {
    id: u32,
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
    Closure((Vec<TyVariable>, TyVariable)),
}

impl UnifyValue for TyInfer {
    fn unify_values(value1: &Self, value2: &Self) -> Result<Self, (Self, Self)> {
        if matches!(value1, TyInfer::Err) || matches!(value2, TyInfer::Err) {
            return Ok(TyInfer::Err);
        }

        // if there's no error, then set unknown to the real type
        if value1 == value2 {
            Ok(value1.clone())
        } else if matches!(value1, TyInfer::Term(ty) if *get_type_deep(ty.clone()).borrow()== PLType::Unknown)
        {
            Ok(value2.clone())
        } else if matches!(value2, TyInfer::Term(ty) if *get_type_deep(ty.clone()).borrow()== PLType::Unknown)
            || matches!(value2, TyInfer::Closure(_))
            || matches!(value1, TyInfer::Closure(_))
        {
            Ok(value1.clone())
        } else {
            Ok(TyInfer::Err)
        }
    }
}

impl TyInfer {
    pub fn get_type(&self, unify_table: &mut UnificationTable<TyVariable>) -> Arc<RefCell<PLType>> {
        match self {
            TyInfer::Term(ty) => ty.clone(),
            TyInfer::Closure((args, ty)) => {
                let mut argtys = vec![];
                for arg in args {
                    argtys.push(unify_table.probe(*arg).get_type(unify_table));
                }
                let ret_ty = unify_table.probe(*ty).get_type(unify_table);
                Arc::new(RefCell::new(PLType::Closure(ClosureType {
                    arg_types: argtys,
                    ret_type: ret_ty,
                    range: Default::default(),
                })))
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

fn unknown_arc() -> Arc<RefCell<PLType>> {
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

    pub fn unify_two_symbol<'a, 'b>(
        &self,
        var1: SymbolType,
        var2: SymbolType,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) {
        match (var1, var2) {
            (SymbolType::Var(v), SymbolType::Var(v2)) => {
                let v_1 = self.unify_table.borrow_mut().probe(v);
                let v_2 = self.unify_table.borrow_mut().probe(v2);
                match (v_1, v_2) {
                    (TyInfer::Closure(c1), TyInfer::Closure(c2)) => {
                        if c1.0.len() == c2.0.len() {
                            for (i, arg) in c1.0.iter().enumerate() {
                                self.unify_two_symbol(
                                    SymbolType::Var(*arg),
                                    SymbolType::Var(c2.0[i]),
                                    ctx,
                                    builder,
                                );
                            }
                        }
                        self.unify_two_symbol(
                            SymbolType::Var(c1.1),
                            SymbolType::Var(c2.1),
                            ctx,
                            builder,
                        );
                    }
                    (TyInfer::Closure(_), TyInfer::Term(t)) => {
                        self.unify_var_tp(v, t, ctx, builder);
                    }
                    (TyInfer::Term(t), TyInfer::Closure(_)) => {
                        self.unify_var_tp(v2, t, ctx, builder);
                    }
                    _ => (),
                };
                self.unify_table.borrow_mut().unify_var_var(v, v2).unwrap();
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

    pub fn unify_var_tp<'a, 'b>(
        &self,
        var: TyVariable,
        tp: Arc<RefCell<PLType>>,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) {
        let ty = self.unify_table.borrow_mut().probe(var);
        // check if closure
        match (ty, &*tp.borrow()) {
            (TyInfer::Closure(c1), PLType::Closure(c2)) => {
                if c1.0.len() == c2.arg_types.len() {
                    for (i, arg) in c1.0.iter().enumerate() {
                        self.unify_var_tp(*arg, c2.arg_types[i].clone(), ctx, builder);
                    }
                }
                self.unify_var_tp(c1.1, c2.ret_type.clone(), ctx, builder);
            }
            (TyInfer::Closure(c1), PLType::Fn(c2)) => {
                if c1.0.len() == c2.fntype.param_pltypes.len() {
                    for (i, arg) in c1.0.iter().enumerate() {
                        self.unify_var_tp(
                            *arg,
                            c2.fntype.param_pltypes[i]
                                .get_type(ctx, builder, true)
                                .unwrap_or(unknown_arc()),
                            ctx,
                            builder,
                        );
                    }
                }
                self.unify_var_tp(
                    c1.1,
                    c2.fntype
                        .ret_pltype
                        .get_type(ctx, builder, true)
                        .unwrap_or(unknown_arc()),
                    ctx,
                    builder,
                );
            }
            _ => (),
        }
        self.unify_table
            .borrow_mut()
            .unify_var_value(var, TyInfer::Term(tp))
            .unwrap();
    }

    pub fn import_symbols(&mut self, ctx: &Ctx) {
        for (name, ty) in &ctx.table {
            // self.add_symbol(name, );
            let key = self.new_key();
            self.unify_table
                .borrow_mut()
                .unify_var_value(key, TyInfer::Term(ty.pltype.clone()))
                .unwrap();
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
                match &mut *d.var {
                    DefVar::Identifier(v) => {
                        let id = self.new_key();
                        self.unify(id, ty, ctx, builder);
                        v.id = Some(id);
                        self.add_symbol(&v.name, id);
                    }
                    DefVar::TupleDeconstruct(_) => (),
                    DefVar::StructDeconstruct(_) => (),
                }
            }
            NodeEnum::Assign(a) => {
                let ty = self.inference(&mut a.exp, ctx, builder);
                match &mut a.var {
                    crate::ast::node::statement::AssignVar::Pointer(p) => {
                        let re = self.inference(&mut *p, ctx, builder);
                        self.unify_two_symbol(re, ty, ctx, builder);
                    }
                    crate::ast::node::statement::AssignVar::Raw(d) => match &mut **d {
                        DefVar::Identifier(v) => {
                            let id = self.new_key();
                            v.id = Some(id);
                            if let Some(ty) = self.get_symbol(&v.name) {
                                self.unify_two_symbol(
                                    SymbolType::Var(id),
                                    SymbolType::Var(ty),
                                    ctx,
                                    builder,
                                );
                            }
                            self.unify(id, ty, ctx, builder);
                        }
                        DefVar::TupleDeconstruct(_) => (),
                        DefVar::StructDeconstruct(_) => (),
                    },
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
                    let id = self.new_key();
                    ex.id.id = Some(id);
                    if let Some(t) = self.get_symbol(&ex.id.name) {
                        self.unify_two_symbol(
                            SymbolType::Var(id),
                            SymbolType::Var(t),
                            ctx,
                            builder,
                        );
                        return SymbolType::Var(id);
                    }
                    if let Some(r) = ctx.get_root_ctx().plmod.types.get(&ex.id.name) {
                        if let PLType::Fn(f) = &*r.tp.clone().borrow() {
                            if f.fntype.generic {
                                return unknown();
                            }
                            let mut argtys = vec![];
                            for arg in &f.fntype.param_pltypes {
                                let arg = arg.get_type(ctx, builder, true).unwrap_or(unknown_arc());
                                let arg_key = self.new_key();
                                self.unify(arg_key, SymbolType::PLType(arg), ctx, builder);
                                argtys.push(arg_key);
                            }
                            let ret_ty = self.new_key();
                            self.unify(
                                ret_ty,
                                SymbolType::PLType(
                                    f.fntype
                                        .ret_pltype
                                        .get_type(ctx, builder, true)
                                        .unwrap_or(unknown_arc()),
                                ),
                                ctx,
                                builder,
                            );
                            self.unify_table
                                .borrow_mut()
                                .unify_var_value(id, TyInfer::Closure((argtys, ret_ty)))
                                .unwrap();
                            return SymbolType::Var(id);
                        }
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
                child
                    .unify_table
                    .borrow_mut()
                    .unify_var_value(id, TyInfer::Closure((argtys, ret_ty)))
                    .unwrap();
                child.add_symbol("@ret", ret_ty);
                c.ret_id = Some(ret_ty);
                child.inference_statements(&mut c.body, ctx, builder);
                return SymbolType::Var(id);
            }
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
                                    if f.fntype.param_pltypes.len() != argtys.len() {
                                        return unknown();
                                    }

                                    for (i, arg) in f.fntype.param_pltypes.iter().enumerate() {
                                        self.unify_two_symbol(
                                            argtys[i].clone(),
                                            SymbolType::PLType(
                                                arg.get_type(ctx, builder, true)
                                                    .unwrap_or(unknown_arc()),
                                            ),
                                            ctx,
                                            builder,
                                        );
                                    }
                                    return SymbolType::PLType(
                                        f.fntype
                                            .ret_pltype
                                            .get_type(ctx, builder, true)
                                            .unwrap_or(unknown_arc()),
                                    );
                                }
                                PLType::Unknown => {
                                    let mut arg_keys = vec![];
                                    for arg in argtys {
                                        let arg_key = self.new_key();
                                        self.unify(arg_key, arg, ctx, builder);
                                        arg_keys.push(arg_key);
                                    }
                                    let ret_ty = self.new_key();
                                    self.unify_table
                                        .borrow_mut()
                                        .unify_var_value(id, TyInfer::Closure((arg_keys, ret_ty)))
                                        .unwrap();
                                    return SymbolType::Var(ret_ty);
                                }
                                _ => (),
                            },
                            TyInfer::Closure((args, ret)) => {
                                if args.len() != argtys.len() {
                                    return unknown();
                                }
                                for (i, arg) in args.iter().enumerate() {
                                    self.unify(*arg, argtys[i].clone(), ctx, builder);
                                }
                                return SymbolType::Var(ret);
                            }
                        }
                    }
                    SymbolType::PLType(tp) => match &*tp.borrow() {
                        PLType::Fn(f) => {
                            if f.fntype.param_pltypes.len() != argtys.len() {
                                return unknown();
                            }
                            for (i, arg) in f.fntype.param_pltypes.iter().enumerate() {
                                self.unify_two_symbol(
                                    argtys[i].clone(),
                                    SymbolType::PLType(
                                        arg.get_type(ctx, builder, true)
                                            .unwrap_or(unknown_arc())
                                            .clone(),
                                    ),
                                    ctx,
                                    builder,
                                );
                            }
                            return SymbolType::PLType(
                                f.fntype
                                    .ret_pltype
                                    .get_type(ctx, builder, true)
                                    .unwrap_or(unknown_arc()),
                            );
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
                match head {
                    SymbolType::Var(v) => {
                        let k = self.unify_table.borrow_mut().probe(v);
                        match k {
                            TyInfer::Term(t) => {
                                let tp = ctx.auto_deref_tp(t);
                                match &*tp.borrow() {
                                    PLType::Struct(a) => {
                                        let f = a.fields.get(&tk.field.as_ref().unwrap().name);
                                        if let Some(f) = f {
                                            return SymbolType::PLType(
                                                f.typenode
                                                    .get_type(ctx, builder, true)
                                                    .unwrap_or(unknown_arc()),
                                            );
                                        }
                                    }
                                    _ => (),
                                };
                            }
                            _ => (),
                        }
                    }
                    SymbolType::PLType(_) => (),
                }
            }
            NodeEnum::StructInit(si) => {
                let ty = si
                    .typename
                    .get_type(ctx, builder, true)
                    .unwrap_or(unknown_arc());
                match &*ty.clone().borrow() {
                    PLType::Struct(s) => {
                        if s.generic_map.is_empty() {
                            return SymbolType::PLType(ty);
                        }
                    }
                    _ => (),
                };
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

            _ => (),
        }
        unknown()
    }
}

fn new_arc_refcell<T>(t: T) -> Arc<RefCell<T>> {
    Arc::new(RefCell::new(t))
}
