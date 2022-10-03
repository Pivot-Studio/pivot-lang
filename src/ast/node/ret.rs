use super::*;
use crate::ast::ctx::Ctx;
use crate::utils::tabs;
use internal_macro::range;

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
            let ret = ctx.try_load(ret);
            ctx.builder.build_return(Some(&ret.as_basic_value_enum()));
        } else {
            ctx.builder.build_return(None);
        }
        Value::None
    }
}
