use super::function::{FuncDefNode, FuncTypeNode};
use super::types::StructDefNode;
use super::*;
use crate::ast::ctx::Ctx;

use internal_macro::range;

#[range]
pub struct ProgramNode {
    pub fns: Vec<FuncDefNode>,
    pub structs: Vec<StructDefNode>,
    pub fntypes: Vec<FuncTypeNode>,
    pub errs: Vec<Box<dyn Node>>,
}
impl Node for ProgramNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        println!("ProgramNode");
        let mut i = self.fns.len() + self.structs.len() + self.errs.len();
        for statement in &self.fns {
            i -= 1;
            statement.print(tabs, i == 0, line.clone());
        }
        for statement in &self.structs {
            i -= 1;
            statement.print(tabs, i == 0, line.clone());
        }
        for err in &self.errs {
            i -= 1;
            err.print(tabs, i == 0, line.clone());
        }
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> (Value<'ctx>, Option<String>) {
        // top level parser
        loop {
            let mut i = 0;
            self.structs.iter().for_each(|x| {
                if !x.emit_struct_def(ctx) {
                    i = i + 1;
                }
            });
            if i == 0 {
                break;
            }
        }
        self.fntypes.iter().for_each(|x| {
            x.emit_func_type(ctx);
        });
        // node parser
        self.fns.iter_mut().for_each(|x| {
            x.emit(ctx);
        });
        for e in self.errs.iter_mut() {
            e.emit(ctx);
        }

        (Value::None, None)
    }
}
