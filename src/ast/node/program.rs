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
}
impl Node for ProgramNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        println!("ProgramNode");
        let mut i = 0;
        let len = self.fns.len()+self.structs.len();
        for statement in &self.fns {
            i += 1;
            statement.print(tabs, i==len, line.clone());
        }
        for statement in &self.structs {
            i += 1;
            statement.print(tabs, i==len, line.clone());
        }
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        loop {
            let mut i = 0;
            self.structs.iter().for_each(|x| {
                let re = x.get_type(ctx);
                match re {
                    Value::None => i = i + 1,
                    _ => {}
                }
            });
            if i == 0 {
                break;
            }
        }
        self.fntypes.iter().for_each(|x| {
            x.get_type(ctx);
        });
        self.fns.iter_mut().for_each(|x| {
            x.emit(ctx);
        });

        Value::None
    }
}
