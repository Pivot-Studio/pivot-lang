use super::*;
use crate::{ast::ctx::Ctx, utils::read_config::enter};
use internal_macro::range;

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ImplNode {
    pub target: Box<TypeNodeEnum>,
    pub methods: Vec<Box<FuncDefNode>>,
}

impl Node for ImplNode {
    fn format(&self, _tabs: usize, _prefix: &str) -> String {
        let mut format_res = String::from(enter());
        format_res.push_str("impl ");
        format_res.push_str(&self.target.format(0, ""));
        format_res.push_str(" {");
        for method in &self.methods {
            format_res.push_str(&method.format(1, "    "));
        }
        format_res.push_str("}");
        format_res.push_str(enter());
        format_res
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ImplNode");
        self.target.print(tabs + 1, false, line.clone());
        for method in &self.methods {
            method.print(tabs + 1, false, line.clone());
        }
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        _ = self.target.emit_highlight(ctx);
        for method in &mut self.methods {
            _ = method.emit(ctx);
        }
        Ok((None, None, TerminatorEnum::NONE))
    }
}
