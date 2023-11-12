use std::{sync::Arc, cell::RefCell};

use ena::unify::{UnifyKey, UnifyValue, UnificationTable};
use rustc_hash::FxHashMap;
use salsa::function::Configuration;

use crate::ast::{pltype::PLType, node::{NodeEnum, statement::{DefVar, StatementsNode}, TypeNode}, ctx::Ctx, builder::BuilderEnum};



#[test]
fn test() {
    let mut table = ena::unify::UnificationTable::<ena::cc::Token>::new();

    // table.unify_var_var(a_id, b_id)
    // table.new_key(value)

}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TyVariable {
    id: u32,
}


impl UnifyKey for TyVariable {
    type Value = TyWrapper;

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyWrapper(pub Arc<RefCell<PLType>>, pub bool);// (ty, is err)

impl UnifyValue for TyWrapper {
    fn unify_values(value1: &Self, value2: &Self) -> Result<Self, (Self, Self)> {
        if value2.1 || value1.1 {
            // if there's error, reset type to unknown
            return Ok(TyWrapper(unknown_arc(), true));
        }
        // if there's no error, then set unknown to the real type
        if value1 == value2 {
            Ok(value1.clone())
        }else if value1.0 == unknown_arc(){
            Ok(value2.clone())
            
        }else {
            Ok(value1.clone())
        }
    }
}


pub struct InferenceCtx<'ctx> {
    unify_table: Arc<RefCell<UnificationTable<TyVariable>>>,
    symbol_table: FxHashMap<String, TyVariable>,
    father: Option<&'ctx InferenceCtx<'ctx>>

}

fn unknown() -> SymbolType {
    SymbolType::PLType( Arc::new(RefCell::new(PLType::Unknown))  )
}

fn unknown_arc() -> Arc<RefCell<PLType>> {
    Arc::new(RefCell::new(PLType::Unknown))
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolType {
    Var(TyVariable),
    PLType(Arc<RefCell<PLType>>)
}

impl <'ctx> InferenceCtx<'ctx> {
    pub fn new(table:Arc<RefCell<UnificationTable<TyVariable>>>) -> Self {
        Self {
            unify_table: table,
            symbol_table: FxHashMap::default(),
            father:None
        }
    }

    pub fn new_child(&'ctx self) -> Self {
        Self {
            unify_table: self.unify_table.clone(),
            symbol_table: FxHashMap::default(),
            father:Some(self)
        }
    }

    pub fn add_symbol(&mut self, name:&str, ty: TyVariable) {
        self.symbol_table.insert(name.to_string(), ty);
    }

    pub fn get_symbol(&self, name:&str) -> Option<TyVariable> {
        if let Some(ty) = self.symbol_table.get(name) {
            return Some(*ty);
        }
        if let Some(father) = self.father {
            return father.get_symbol(name);
        }
        None
    }

    pub fn unify(& self, var: TyVariable, value: SymbolType) {
        match value {
            SymbolType::Var(v) => {
                self.unify_table.borrow_mut().unify_var_var(var, v).unwrap();
            },
            SymbolType::PLType(ty) => {
                self.unify_table.borrow_mut().unify_var_value(var, TyWrapper(ty, false)).unwrap();
            }
        }
    }

    pub fn unify_two_symbol(& self, var1: SymbolType, var2: SymbolType) {
        match (var1,var2) {
            (SymbolType::Var(v), SymbolType::Var(v2)) => {
                self.unify_table.borrow_mut().unify_var_var(v, v2).unwrap();
            },
            (SymbolType::Var(v), SymbolType::PLType(tp)) => {
                self.unify_table.borrow_mut().unify_var_value(v, TyWrapper(tp, false)).unwrap();
            },
            (SymbolType::PLType(tp), SymbolType::Var(v)) =>{
                self.unify_table.borrow_mut().unify_var_value(v, TyWrapper(tp, false)).unwrap();
            },
            (SymbolType::PLType(_), SymbolType::PLType(_)) => (),
        }
    }

    pub fn inference_statements<'a, 'b>(&mut self, node: &mut StatementsNode, ctx:&'b mut Ctx<'a>,builder: &'b BuilderEnum<'a, '_>) {
        for s in &mut node.statements {
            self.inference(&mut *s, ctx, builder);
        }
    }

    pub fn new_key(&self) -> TyVariable {
        self.unify_table.borrow_mut().new_key(TyWrapper(unknown_arc(), false))
        
    }

    pub fn inference<'a, 'b>(&mut self, node: &mut NodeEnum, ctx:&'b mut Ctx<'a>,builder: &'b BuilderEnum<'a, '_>) -> SymbolType {
        match node {
            NodeEnum::Def(d) => {
                let mut ty = unknown();
                if let Some(exp) = &mut d.exp {
                    ty = self.inference(&mut *exp, ctx, builder);
                }
                if let Some(tp) = &d.tp {
                    let new_ty   =SymbolType::PLType( tp.get_type(ctx, builder, true).unwrap_or(unknown_arc()));
                    if ty != unknown() && new_ty != unknown() && ty != new_ty {
                        ty = unknown();
                    }else {
                        ty = new_ty;
                    }
                }
                match &mut *d.var {
                    DefVar::Identifier(v) => {
                        let id = self.new_key();
                        v.id = Some(id);
                        self.add_symbol(&v.name, id);
                    },
                    DefVar::TupleDeconstruct(_) => (),
                    DefVar::StructDeconstruct(_) => (),
                }
            },
            NodeEnum::Assign(a) => {
                let ty = self.inference(&mut *a.exp, ctx, builder);
                match &mut a.var {
                    crate::ast::node::statement::AssignVar::Pointer(p) => {
                        let re = self.inference(&mut * p, ctx, builder);
                        self.unify_two_symbol(re, ty);
                    },
                    crate::ast::node::statement::AssignVar::Raw(d) => {
                        match &mut **d {
                            DefVar::Identifier(v) => {
                                let id = self.new_key();
                                v.id = Some(id);
                                if let Some(ty) = self.get_symbol(&v.name) {
                                    self.unify_table.borrow_mut().unify_var_var(id, ty).unwrap();
                                }
                                self.unify(id, ty);
                            },
                            DefVar::TupleDeconstruct(_) => (),
                            DefVar::StructDeconstruct(_) => (),
                        }
                    },
                }
            },
            NodeEnum::Expr(e) =>(),
            NodeEnum::ExternIdNode(ex) =>{
                if ex.ns.len() == 0 {
                    let id = self.new_key();
                    ex.id.id = Some(id);
                    if let Some(t) = self.get_symbol(&ex.id.name) {
                        self.unify_table.borrow_mut().unify_var_var(id, t).unwrap() 
                    }
                    return SymbolType::Var(id);
                }
            },
            NodeEnum::Bool(_) => return SymbolType::PLType( new_arc_refcell(PLType::Primitive(crate::ast::pltype::PriType::BOOL))),
            NodeEnum::Num(n) => {
                match n.value {
                    crate::ast::node::Num::Int(_) =>  return SymbolType::PLType( new_arc_refcell(PLType::Primitive(crate::ast::pltype::PriType::I64))),
                    crate::ast::node::Num::Float(_) => return SymbolType::PLType( new_arc_refcell(PLType::Primitive(crate::ast::pltype::PriType::F64))),
                }
            },
            NodeEnum::Primary(p) => {
                return  self.inference(&mut *p.value, ctx, builder);
            }
            _ => (),
        }
        unknown()
    }

}



fn new_arc_refcell<T>(t: T) -> Arc<RefCell<T>> {
    Arc::new(RefCell::new(t))
}

