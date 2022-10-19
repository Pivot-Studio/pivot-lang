use internal_macro::range;
use lsp_types::SemanticTokenType;

use crate::ast::{
    ctx::{Ctx, PLType},
    diag::ErrorCode,
    node::{deal_line, tab},
};

use super::{primary::VarNode, Node, NodeResult, TerminatorEnum, Value};

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct UseNode {
    pub ids: Vec<Box<VarNode>>,
    /// 是否完整
    /// use a::b 完整
    /// use a::b:: 不完整
    pub complete: bool,
}

impl Node for UseNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("UseNode");
        let mut i = self.ids.len();
        for id in &self.ids {
            i -= 1;
            id.print(tabs + 1, i == 0, line.clone());
        }
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        for id in &self.ids {
            ctx.push_semantic_token(id.range, SemanticTokenType::NAMESPACE, 0);
        }
        Ok((Value::None, None, TerminatorEnum::NONE, false))
    }
}

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ExternIDNode {
    pub ns: Vec<Box<VarNode>>,
    pub id: Box<VarNode>,
}

impl Node for ExternIDNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ExternIDNode");
        for id in &self.ns {
            id.print(tabs + 1, false, line.clone());
        }
        self.id.print(tabs + 1, true, line.clone());
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        for id in &self.ns {
            ctx.push_semantic_token(id.range, SemanticTokenType::NAMESPACE, 0);
        }
        let mut plmod = &ctx.plmod;
        for ns in self.ns.iter() {
            let re = plmod.submods.get(&ns.name);
            if let Some(re) = re {
                plmod = re;
            } else {
                return Err(ctx.add_err(ns.range, ErrorCode::UNRESOLVED_MODULE));
            }
        }
        let symbol = plmod.get_global_symbol(&self.id.name);
        if let Some(symbol) = symbol {
            ctx.push_semantic_token(self.id.range, SemanticTokenType::VARIABLE, 0);

            let g = ctx.get_or_add_global(&self.id.name, &plmod, &symbol.tp);
            return Ok((
                Value::VarValue(g),
                Some(symbol.tp.to_string()),
                TerminatorEnum::NONE,
                true,
            ));
        }
        if let Some(tp) = plmod.get_type(&self.id.name) {
            match tp {
                PLType::FN(f) => {
                    ctx.push_semantic_token(self.id.range, SemanticTokenType::FUNCTION, 0);
                    return Ok((
                        Value::FnValue(f.get_value(ctx)),
                        Some(f.name.clone()),
                        TerminatorEnum::NONE,
                        true,
                    ));
                }
                PLType::STRUCT(s) => {
                    ctx.push_semantic_token(self.id.range, SemanticTokenType::STRUCT, 0);
                    return Ok((
                        Value::STValue(s.struct_type(ctx)),
                        Some(s.name.clone()),
                        TerminatorEnum::NONE,
                        true,
                    ));
                }
                _ => unreachable!(),
            }
        }
        Err(ctx.add_err(self.range, ErrorCode::SYMBOL_NOT_FOUND))
    }
}
