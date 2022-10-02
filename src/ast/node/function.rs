use super::{
    alloc,
    types::{TypeNode, TypedIdentifierNode},
    Node,
};
use crate::utils::tabs;
use inkwell::types::BasicType;
use internal_macro::range;

use string_builder::Builder;

#[range]
pub struct FuncDefNode {
    pub paralist: Vec<Box<TypedIdentifierNode>>,
    pub id: String,
    pub ret: Box<dyn TypeNode>,
    pub body: Box<dyn Node>,
}

impl Node for FuncDefNode {
    fn string(&self, tabs: usize) -> String {
        let mut builder = Builder::default();
        tabs::print_tabs(&mut builder, tabs);
        builder.append("(FuncDefNode");
        builder.append(format!("id: {}", self.id));
        for para in &self.paralist {
            builder.append(para.string(tabs + 1));
        }
        builder.string().unwrap()
    }
    fn emit<'a, 'ctx>(
        &'a mut self,
        ctx: &mut crate::ast::ctx::Ctx<'a, 'ctx>,
    ) -> super::Value<'ctx> {
        // get the para's type vec & copy the para's name vec
        let mut para_types = Vec::new();
        let mut para_names = Vec::new();
        for para in self.paralist.iter_mut() {
            para_types.push(para.tp.get_type(ctx).unwrap().get_basic_type().into());
            para_names.push(para.id.clone());
        }
        // add function
        let ret_type = self.ret.get_type(ctx).unwrap().get_basic_type();
        let func_type = ret_type.fn_type(&para_types, false);
        let func = ctx.module.add_function(self.id.as_str(), func_type, None);
        // copy para type
        let mut para_tps = Vec::new();
        for i in 0..para_names.len() {
            let para_type = func.get_nth_param(i as u32).unwrap();
            para_tps.push(para_type);
        }
        // add block
        let entry = ctx.context.append_basic_block(func, "entry");
        ctx.builder.position_at_end(entry);
        // alloc para
        for (i, para) in para_tps.iter_mut().enumerate() {
            let alloca = alloc(ctx, para.get_type(), &para_names[i]);
            ctx.builder.build_store(alloca, *para);
            ctx.add_symbol(para_names[i].clone(), alloca);
        }
        // emit body
        self.body.emit(ctx);
        super::Value::None
    }
}
