use internal_macro::node;

use crate::ast::node::{deal_line, tab};

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
        todo!()
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
