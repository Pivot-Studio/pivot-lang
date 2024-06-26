use std::{cell::RefCell, sync::Arc};

use crate::ast::builder::{BuilderEnum, IRBuilder};
use crate::ast::ctx::{Ctx, EqRes};
use crate::ast::node::node_result::NodeResultBuilder;
use crate::ast::node::RangeTrait;
use crate::ast::range::Range;
use internal_macro::node;
use linked_hash_map::LinkedHashMap;
use ustr::Ustr;

use crate::ast::pltype::{get_type_deep, Field};
use crate::ast::{
    node::{deal_line, tab},
    pltype::{PLType, STType},
    tokens::TokenType,
};

use super::{Node, NodeEnum, PrintTrait, TypeNode, TypeNodeEnum};

#[node]
pub struct TupleInitNode {
    // exprs holds the expressions inside a tuple separated by comma `,`
    pub exprs: Vec<Box<NodeEnum>>,
}

impl Node for TupleInitNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut crate::ast::ctx::Ctx<'a>,
        builder: &'b crate::ast::builder::BuilderEnum<'a, '_>,
    ) -> super::node_result::NodeResult {
        let mut expr_values = vec![];
        let mut fields = LinkedHashMap::new();
        let mut field_tps = vec![];
        let mut err = None;
        let mut name = String::new();
        let mut is_atomic = true;
        for (i, expr) in self.exprs.iter_mut().enumerate() {
            let expr = expr.emit(ctx, builder);
            match expr {
                Ok(value) => {
                    let ty = value.get_value().unwrap().get_ty();
                    field_tps.push(ty.clone());
                    let tp = ty.borrow();
                    if i != 0 {
                        name.push_str(", ");
                    }
                    name.push_str(&tp.get_name());
                    expr_values.push(value);
                    let f = new_tuple_field(i, &tp, &ctx.get_file());
                    if !tp.is_atomic() {
                        is_atomic = false;
                    }
                    fields.insert(i.to_string().into(), f);
                }
                Err(diag) => {
                    err = Some(diag);
                }
            }
        }
        // make global function lookup logic work for tuple types
        name = format!("@Tuple{}<{}>", self.exprs.len(), name);
        if let Some(err) = err {
            return Err(err);
        }
        let mut sttype = new_tuple_type(name.into(), fields, self.range);
        sttype.atomic = is_atomic;
        let mut offset = 1;
        // atomic struct has no gc pointer in it, so it doesn't need
        // gcrtti field.
        if is_atomic {
            sttype.fields.iter_mut().for_each(|(_, f)| {
                f.index -= 1;
            });
            offset = 0;
        }
        builder.gen_st_visit_function(ctx, &sttype, &field_tps);
        let stu = Arc::new(RefCell::new(PLType::Struct(sttype)));
        ctx.add_type_without_check(stu.clone());
        let mut v = builder.alloc("tuple_v", &stu.borrow(), ctx, Some(self.range().start));
        if ctx.generator_data.is_some() {
            v = builder.build_load(v, "load_ctx_var", &PLType::new_i8_ptr(), ctx);
        }

        // 初始化赋值
        for (i, value) in expr_values.into_iter().enumerate() {
            let field_ptr = builder
                .build_struct_gep(v, i as u32 + offset, &i.to_string(), &stu.borrow(), ctx)
                .unwrap();
            let vv = value.get_value().unwrap();
            let v = builder.try_load2var(self.range, vv.get_value(), &vv.get_ty().borrow(), ctx)?;
            builder.build_store(field_ptr, v);
        }
        v.new_output(stu).to_result()
    }
}

pub fn new_tuple_field(i: usize, tp: &PLType, f: &Ustr) -> Field {
    Field {
        index: i as u32 + 1,
        typenode: tp.get_typenode(f),
        name: i.to_string().into(),
        range: Default::default(),
        modifier: Some((TokenType::PUB, Default::default())),
    }
}

pub fn new_tuple_type(name: Ustr, fields: LinkedHashMap<Ustr, Field>, range: Range) -> STType {
    STType {
        name,
        path: "".to_string().into(),
        fields,
        range: Default::default(),
        doc: vec![],
        generic_map: Default::default(),
        derives: vec![],
        modifier: Some((TokenType::PUB, Default::default())),
        body_range: range,
        is_trait: false,
        is_tuple: true,
        generic_infer_types: Default::default(),
        // generic_infer: Default::default(),
        methods: Default::default(),
        trait_methods_impl: Default::default(),
        atomic: false,
    }
}
impl PrintTrait for TupleInitNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("TupleInitNode");
        for (i, exp) in self.exprs.iter().enumerate() {
            exp.print(tabs + 1, i == self.exprs.len() - 1, line.clone());
        }
    }
}

#[node]
pub struct TupleTypeNode {
    /// types hold all types in a tuple with order from left to right
    pub types: Vec<Box<TypeNodeEnum>>,
}

impl TypeNode for TupleTypeNode {
    fn get_type<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
        gen_code: bool,
    ) -> super::TypeNodeResult {
        let mut fields = LinkedHashMap::new();
        let mut field_tps = vec![];
        let mut err = None;
        let mut name = String::new();
        let mut is_atomic = true;
        for (i, tp) in self.types.iter().enumerate() {
            let tp = tp.get_type(ctx, builder, gen_code);
            match tp {
                Ok(tp) => {
                    let arctp = tp.clone();
                    let tp = tp.borrow();
                    if !name.is_empty() {
                        name.push_str(", ");
                    }
                    if !get_type_deep(arctp.clone()).borrow().is_atomic() {
                        is_atomic = false;
                    }
                    name.push_str(&tp.get_name());
                    field_tps.push(arctp.clone());
                    let f = Field {
                        index: i as u32 + 1,
                        typenode: tp.get_typenode(&ctx.get_file()),
                        name: i.to_string().into(),
                        range: Default::default(),
                        modifier: Some((TokenType::PUB, Default::default())),
                    };
                    fields.insert(i.to_string().into(), f);
                }
                Err(diag) => {
                    err = Some(diag);
                }
            }
        }
        name = format!("@Tuple{}<{}>", self.types.len(), name);
        if let Some(err) = err {
            return Err(err);
        }
        let mut sttype = new_tuple_type(name.into(), fields, self.range);
        sttype.atomic = is_atomic;
        if is_atomic {
            sttype.fields.iter_mut().for_each(|(_, f)| {
                f.index -= 1;
            });
        }
        builder.gen_st_visit_function(ctx, &sttype, &field_tps);
        let stu = Arc::new(RefCell::new(PLType::Struct(sttype)));
        ctx.add_type_without_check(stu.clone());
        Ok(stu)
    }

    fn emit_highlight(&self, ctx: &mut crate::ast::ctx::Ctx) {
        for tp in &self.types {
            tp.emit_highlight(ctx);
        }
    }

    fn eq_or_infer<'a, 'b>(
        &self,
        ctx: &'b mut crate::ast::ctx::Ctx<'a>,
        pltype: Arc<RefCell<PLType>>,
        builder: &'b crate::ast::builder::BuilderEnum<'a, '_>,
    ) -> Result<crate::ast::ctx::EqRes, crate::ast::diag::PLDiag> {
        let left = self.get_type(ctx, builder, true)?;
        let binding = left.borrow();
        let failed = Ok(EqRes {
            eq: false,
            need_up_cast: false,
            reason: None,
        });
        match (&*binding, &*pltype.borrow()) {
            (PLType::Struct(st1), PLType::Struct(st2)) => {
                if st1.fields.len() != st2.fields.len() || !st1.is_tuple || !st2.is_tuple {
                    return failed;
                }
                for ((_, f1), (_, f2)) in st1.fields.iter().zip(st2.fields.iter()) {
                    let ty2 = f2.typenode.get_type(ctx, builder, true)?;
                    let re = f1.typenode.eq_or_infer(ctx, ty2, builder)?;
                    if !re.eq {
                        return failed;
                    }
                }
                Ok(EqRes {
                    eq: true,
                    need_up_cast: false,
                    reason: None,
                })
            }
            _ => failed,
        }
    }
}

impl PrintTrait for TupleTypeNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("TupleTypeNode");
        for (i, tp) in self.types.iter().enumerate() {
            tp.print(tabs + 1, i == self.types.len() - 1, line.clone());
        }
    }
}
