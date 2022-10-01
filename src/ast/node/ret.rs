use super::*;
use crate::ast::ctx::Ctx;
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
        } else {
            println!("void");
        }
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        if let Some(ret) = &mut self.value {
            let ret = ret.emit(ctx);
            match ret {
                Value::IntValue(v) => ctx.builder.build_return(Some(&v)),
                Value::BoolValue(v) => ctx.builder.build_return(Some(&v)),
                Value::FloatValue(v) => ctx.builder.build_return(Some(&v)),
                Value::VarValue(v) => ctx.builder.build_return(Some(&v)),
                Value::TypeValue(_) => panic!("not impl"),
                Value::None => ctx.builder.build_return(None),
            };
        } else {
            ctx.builder.build_return(None);
        }
        Value::None
    }
}
