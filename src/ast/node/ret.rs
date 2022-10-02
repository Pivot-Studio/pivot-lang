use super::*;
use crate::ast::ctx::Ctx;
use crate::utils::tabs;
use internal_macro::range;


#[range]
pub struct RetNode {
    pub value: Option<Box<dyn Node>>,
}

impl Node for RetNode {
    fn print(&self) {
        println!("RetNode:");
        if let Some(ret) = &self.value {
            ret.print();
        }else{
            println!("void");
        }
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        if let Some(ret) = &mut self.value {
            let ret = ret.emit(ctx);
            let ret = ctx.try_load(ret);
            ctx.builder.build_return(Some(&ret.as_basic_value_enum()));
        } else {
            ctx.builder.build_return(None);
        }
        Value::None
    }
}

