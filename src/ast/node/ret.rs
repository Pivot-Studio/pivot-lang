use super::*;
use crate::ast::ctx::Ctx;
use internal_macro::range;
use crate::utils::tabs;

use string_builder::Builder;

#[range]
pub struct RetNode {
    pub value: Option<Box<dyn Node>>,
}

impl Node for RetNode {
    fn string(&self, tabs: usize) -> String {
        let mut builder = Builder::default();
        tabs::print_tabs(&mut builder, tabs);
        builder.append("(RetNode");
        if let Some(value) = &self.value {
            builder.append(value.string(tabs + 1));
        }
        tabs::print_tabs(&mut builder, tabs);
        builder.append(")");
        builder.string().unwrap()
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
