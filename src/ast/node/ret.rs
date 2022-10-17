use super::*;
use crate::ast::ctx::Ctx;
use internal_macro::range;

#[range]
pub struct RetNode {
    pub value: Option<Box<dyn Node>>,
}

impl Node for RetNode {
    fn format(&self, tabs: usize, prefix: &str) {
        println!("hello");
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("RetNode");
        if let Some(value) = &self.value {
            value.print(tabs + 1, true, line.clone());
        }
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> (Value<'ctx>, Option<String>) {
        if let Some(ret) = &mut self.value {
            let (ret, _) = ret.emit(ctx);
            let ret = ctx.try_load(ret);
            ctx.builder.build_return(Some(&ret.as_basic_value_enum()));
        } else {
            ctx.builder.build_return(None);
        }
        (Value::None, None)
    }
}
