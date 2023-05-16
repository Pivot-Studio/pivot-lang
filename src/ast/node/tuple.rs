use std::{cell::RefCell, sync::Arc};

use crate::ast::builder::{BuilderEnum, IRBuilder};
use crate::ast::ctx::Ctx;
use crate::ast::node::node_result::NodeResultBuilder;
use crate::ast::node::RangeTrait;
use crate::ast::range::Range;
use internal_macro::node;
use linked_hash_map::LinkedHashMap;

use crate::ast::pltype::Field;
use crate::ast::{
    node::{deal_line, tab},
    pltype::{PLType, STType},
    tokens::TokenType,
};

use super::{Node, NodeEnum, PrintTrait, TypeNode, TypeNodeEnum};

#[node]
pub struct TupleInitNode {
    pub exprs: Vec<Box<NodeEnum>>,
}

impl Node for TupleInitNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut crate::ast::ctx::Ctx<'a>,
        builder: &'b crate::ast::builder::BuilderEnum<'a, 'ctx>,
    ) -> super::node_result::NodeResult {
        let mut expr_values = vec![];
        let mut fields = LinkedHashMap::new();
        let mut field_tps = vec![];
        let mut err = None;
        let mut name = String::new();
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
                    let f = Field {
                        index: i as u32 + 1,
                        typenode: tp.get_typenode(),
                        name: i.to_string(),
                        range: Default::default(),
                        modifier: Some((TokenType::PUB, Default::default())),
                    };
                    fields.insert(i.to_string(), f);
                }
                Err(diag) => {
                    err = Some(diag);
                }
            }
        }
        name = format!("({})", name);
        if let Some(err) = err {
            return Err(err);
        }
        let sttype = new_tuple_type(name, ctx, fields, self.range);
        builder.gen_st_visit_function(ctx, &sttype, &field_tps);
        let stu = Arc::new(RefCell::new(PLType::Struct(sttype)));
        ctx.add_type_without_check(stu.clone());
        let v = builder.alloc("tuple_v", &stu.borrow(), ctx, Some(self.range().start));
        // 初始化赋值
        for (i, value) in expr_values.into_iter().enumerate() {
            let field_ptr = builder
                .build_struct_gep(v, i as u32 + 1, &i.to_string())
                .unwrap();
            let v =
                builder.try_load2var(self.range, value.get_value().unwrap().get_value(), ctx)?;
            builder.build_store(field_ptr, v);
        }
        v.new_output(stu).to_result()
    }
}

fn new_tuple_type(
    name: String,
    ctx: &mut crate::ast::ctx::Ctx,
    fields: LinkedHashMap<String, Field>,
    range: Range,
) -> STType {
    STType {
        name,
        path: ctx.plmod.path.clone(),
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
        methods: Default::default(),
        trait_methods_impl: Default::default(),
    }
}
impl PrintTrait for TupleInitNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("TupleInitNode");
        for exp in self.exprs.iter() {
            exp.print(tabs + 1, false, line.clone());
        }
    }
}

#[node]
pub struct TupleTypeNode {
    pub tps: Vec<Box<TypeNodeEnum>>,
}

impl TypeNode for TupleTypeNode {
    fn get_type<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
        gen_code: bool,
    ) -> super::TypeNodeResult {
        let mut fields = LinkedHashMap::new();
        let mut field_tps = vec![];
        let mut err = None;
        let mut name = String::new();
        for (i, tp) in self.tps.iter().enumerate() {
            let tp = tp.get_type(ctx, builder, gen_code);
            match tp {
                Ok(tp) => {
                    let arctp = tp.clone();
                    let tp = tp.borrow();
                    if !name.is_empty() {
                        name.push_str(", ");
                    }
                    name.push_str(&tp.get_name());
                    field_tps.push(arctp.clone());
                    let f = Field {
                        index: i as u32 + 1,
                        typenode: tp.get_typenode(),
                        name: i.to_string(),
                        range: Default::default(),
                        modifier: Some((TokenType::PUB, Default::default())),
                    };
                    fields.insert(i.to_string(), f);
                }
                Err(diag) => {
                    err = Some(diag);
                }
            }
        }
        name = format!("({})", name);
        if let Some(err) = err {
            return Err(err);
        }
        let sttype = new_tuple_type(name, ctx, fields, self.range);
        builder.gen_st_visit_function(ctx, &sttype, &field_tps);
        let stu = Arc::new(RefCell::new(PLType::Struct(sttype)));
        ctx.add_type_without_check(stu.clone());
        Ok(stu)
    }

    fn emit_highlight(&self, ctx: &mut crate::ast::ctx::Ctx) {
        for tp in &self.tps {
            tp.emit_highlight(ctx);
        }
    }

    fn eq_or_infer<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut crate::ast::ctx::Ctx<'a>,
        pltype: Arc<RefCell<PLType>>,
        builder: &'b crate::ast::builder::BuilderEnum<'a, 'ctx>,
    ) -> Result<crate::ast::ctx::EqRes, crate::ast::diag::PLDiag> {
        let left = self.get_type(ctx, builder, true)?;
        let eq = *left.borrow() == *pltype.borrow();
        Ok(crate::ast::ctx::EqRes {
            eq,
            need_up_cast: false,
        })
    }
}

impl PrintTrait for TupleTypeNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("TupleTypeNode");
        for tp in self.tps.iter() {
            tp.print(tabs + 1, false, line.clone());
        }
    }
}
