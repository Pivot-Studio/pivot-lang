use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{
        ctx::Ctx,
        node::{deal_line, tab},
        pltype::{ARRType, PLType, PriType},
    },
    plv,
};

use inkwell::values::AnyValueEnum;
use internal_macro::{fmt, range};
use lsp_types::SemanticTokenType;

use super::{Node, NodeResult, PLValue, TerminatorEnum};

#[range]
#[fmt]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct StringNode {
    pub content: String,
}

impl Node for StringNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StringNode: \"{}\"", self.content);
    }

    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult {
        ctx.push_semantic_token(self.range, SemanticTokenType::STRING, 0);
        let v = ctx.llbuilder.borrow().const_string(&self.content);
        Ok((
            Some({
                let mut res: PLValue = plv!(v);
                res.set_const(true);
                res
            }),
            Some(Rc::new(RefCell::new(PLType::ARR(ARRType {
                element_type: Rc::new(RefCell::new(PLType::PRIMITIVE(PriType::U8))),
                size: self.content.len() as u32,
            })))),
            TerminatorEnum::NONE,
        ))
    }
}
