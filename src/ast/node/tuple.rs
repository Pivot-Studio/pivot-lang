use std::{cell::RefCell, sync::Arc};

use crate::ast::builder::IRBuilder;
use crate::ast::node::node_result::NodeResultBuilder;
use crate::ast::node::RangeTrait;
use internal_macro::node;
use linked_hash_map::LinkedHashMap;

use crate::ast::pltype::Field;
use crate::ast::{
    node::{deal_line, tab},
    pltype::{PLType, STType},
    tokens::TokenType,
};

use super::{Node, NodeEnum, PrintTrait};

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
            let range = expr.range();
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
                        range,
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
        let sttype = STType {
            name,
            path: ctx.plmod.path.clone(),
            fields,
            range: Default::default(),
            doc: vec![],
            generic_map: Default::default(),
            derives: vec![],
            modifier: Some((TokenType::PUB, Default::default())),
            body_range: self.range(),
            is_trait: false,
            is_tuple: true,
        };
        builder.gen_st_visit_function(ctx, &sttype, &field_tps);
        let stu = Arc::new(RefCell::new(PLType::Struct(sttype)));
        ctx.add_type_without_check(stu.clone());
        let v = builder.alloc("tuple_v", &stu.borrow(), ctx, Some(self.range().start));
        // 初始化赋值
        for (i, value) in expr_values.into_iter().enumerate() {
            let filed_ptr = builder
                .build_struct_gep(v, i as u32 + 1, &i.to_string())
                .unwrap();
            builder.build_store(filed_ptr, value.get_value().unwrap().get_value());
        }
        v.new_output(stu).to_result()
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
