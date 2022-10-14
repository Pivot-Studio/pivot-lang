use std::cell::RefCell;
use std::rc::Rc;

use super::function::FuncTypeNode;
use super::types::StructDefNode;
use super::*;
use crate::ast::ctx::Ctx;
use crate::lsp::semantic_tokens::SemanticTokensBuilder;

use internal_macro::range;

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ProgramNode {
    pub nodes: Vec<Box<NodeEnum>>,
    pub structs: Vec<StructDefNode>,
    pub fntypes: Vec<FuncTypeNode>,
}
impl Node for ProgramNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        println!("ProgramNode");
        let mut i = self.nodes.len();
        for statement in &self.nodes {
            i -= 1;
            statement.print(tabs, i == 0, line.clone());
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
        ctx.semantic_tokens_builder = Rc::new(RefCell::new(Box::new(SemanticTokensBuilder::new(
            ctx.src_file_path.to_string(),
        ))));
        // node parser
        self.nodes.iter_mut().for_each(|x| {
            _ = x.emit(ctx);
        });

        Ok((Value::None, None))
    }
}
