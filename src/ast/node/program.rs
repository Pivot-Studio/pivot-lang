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
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        // top level parser
        let mut prev = 1000000;
        loop {
            let mut i = 0;
            self.structs.iter().for_each(|x| {
                if x.emit_struct_def(ctx).is_err() {
                    i = i + 1;
                }
            });
            if i == 0 {
                break;
            }
            if i == prev {
                self.structs.iter().for_each(|x| {
                    if let Err(e) = x.emit_struct_def(ctx) {
                        ctx.add_diag(e);
                    }
                });
                break;
            }
            prev = i;
        }
        self.fntypes.iter_mut().for_each(|x| {
            let re = x.emit_func_type(ctx);
            if re.is_err() {
                ctx.add_diag(re.err().unwrap());
            }
        });
        // node parser
        self.fns.iter_mut().for_each(|x| {
            _ = x.emit_func_def(ctx);
        });
        for e in self.errs.iter_mut() {
            _ = e.emit(ctx);
        }

        Ok((Value::None, None))
    }
}
