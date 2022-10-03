use std::fmt::format;

use super::types::*;
use super::{
    alloc,
    types::{TypeNode, TypedIdentifierNode},
    Node,
};
use crate::ast::ctx::{FNType, PLType};
use crate::utils::tabs;
use inkwell::values::FunctionValue;
use internal_macro::range;

use string_builder::Builder;

#[derive(Clone)]
pub struct FuncTypeNode {
    pub id: String,
    pub paralist: Vec<Box<TypedIdentifierNode>>,
    pub ret: Box<TypeNameNode>,
}

#[range]
pub struct FuncDefNode {
    pub typenode: FuncTypeNode,
    pub body: Option<Box<dyn Node>>,
}

impl Node for FuncDefNode {
    fn string(&self, tabs: usize) -> String {
        let mut builder = Builder::default();
        tabs::print_tabs(&mut builder, tabs);
        builder.append("(FuncDefNode");
        builder.append(format!("id: {}", self.typenode.id));
        for para in &self.typenode.paralist {
            builder.append(para.string(tabs + 1));
        }
        builder.append(self.typenode.ret.string(tabs + 1));
        if let Some(body) = &self.body {
            builder.append(body.string(tabs + 1));
        }
        tabs::print_tabs(&mut builder, tabs);
        builder.append(")");
        builder.string().unwrap()
    }
    fn emit<'a, 'ctx>(
        &'a mut self,
        ctx: &mut crate::ast::ctx::Ctx<'a, 'ctx>,
    ) -> super::Value<'ctx> {
        // get the para's type vec & copy the para's name vec
        let mut para_names = Vec::new();
        for para in self.typenode.paralist.iter_mut() {
            para_names.push(para.id.clone());
        }
        // add function
        let func;
        if let Some(fu) = ctx.get_type(self.typenode.id.as_str()) {
            func = match fu {
                PLType::FN(fu) => fu.fntype,
                _ => panic!("type error"),
            };
        } else {
            panic!("fn not found");
        }
        ctx.function = Some(func);

        if let Some(body) = self.body.as_mut() {
            // copy para type
            let mut para_tps = Vec::new();
            for i in 0..para_names.len() {
                let para_type = func.get_nth_param(i as u32).unwrap();
                para_tps.push(para_type);
            }
            // add block
            let allocab = ctx.context.append_basic_block(func, "alloc");
            let entry = ctx.context.append_basic_block(func, "entry");
            ctx.block = Some(entry);
            ctx.builder.position_at_end(entry);
            // alloc para
            for (i, para) in para_tps.iter_mut().enumerate() {
                let alloca = alloc(ctx, para.get_type(), &para_names[i]);
                ctx.builder.build_store(alloca, *para);
                ctx.add_symbol(para_names[i].clone(), alloca);
            }
            // emit body
            body.emit(ctx);
            ctx.builder.position_at_end(allocab);
            ctx.builder.build_unconditional_branch(entry);
            return super::Value::None;
        }
        super::Value::None
    }
}

#[range]
pub struct FuncCallNode {
    pub id: String,
    pub paralist: Vec<Box<dyn Node>>,
}

impl Node for FuncCallNode {
    fn string(&self, tabs: usize) -> String {
        let mut builder = Builder::default();
        tabs::print_tabs(&mut builder, tabs);
        builder.append("(FuncCallNode");
        builder.append(format!("id: {}", self.id));
        for para in &self.paralist {
            builder.append(para.string(tabs + 1));
        }
        builder.append(")");
        builder.string().unwrap()
    }
    fn emit<'a, 'ctx>(
        &'a mut self,
        ctx: &mut crate::ast::ctx::Ctx<'a, 'ctx>,
    ) -> super::Value<'ctx> {
        let mut para_values = Vec::new();
        for para in self.paralist.iter_mut() {
            let v = para.emit(ctx);
            let load = ctx.try_load(v);
            para_values.push(load.as_basic_value_enum().into());
        }
        let func = ctx.module.get_function(self.id.as_str()).unwrap();
        let ret = ctx.builder.build_call(
            func,
            &para_values,
            format(format_args!("call_{}", self.id)).as_str(),
        );
        if let Some(v) = ret.try_as_basic_value().left() {
            return super::Value::LoadValue(v);
        } else {
            return super::Value::None;
        }
    }
}

impl FuncTypeNode {
    pub fn get_type<'a, 'ctx>(
        &'a self,
        ctx: &mut crate::ast::ctx::Ctx<'a, 'ctx>,
    ) -> FunctionValue<'ctx> {
        if let Some(func) = ctx.get_type(self.id.as_str()) {
            let f = match func {
                PLType::FN(func) => func.fntype,
                _ => panic!("type error"),
            };
            return f;
        }

        let mut para_types = Vec::new();
        for para in self.paralist.iter() {
            para_types.push(para.tp.get_type(ctx).unwrap().get_basic_type().into());
        }
        let ret_type = self.ret.get_type(ctx).unwrap().get_ret_type();
        let func_type = ret_type.fn_type(&para_types, false);
        let func = ctx.module.add_function(self.id.as_str(), func_type, None);
        ctx.add_type(
            self.id.clone(),
            PLType::FN(FNType {
                name: self.id.clone(),
                fntype: func,
            }),
        );
        func
    }
}
