use super::*;
use crate::ast::ctx::{Ctx, PLType};
use crate::ast::diag::ErrorCode;

use internal_macro::range;
use lsp_types::SemanticTokenType;

#[range]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct BoolConstNode {
    pub value: bool,
}

impl Node for BoolConstNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line, end);
        println!("BoolConstNode: {}", self.value);
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.push_semantic_token(self.range, SemanticTokenType::KEYWORD, 0);
        Ok((
            Value::BoolValue(ctx.context.i8_type().const_int(self.value as u64, true)),
            Some("bool".to_string()),
            TerminatorEnum::NONE,
            true,
        ))
    }
}

#[range]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct NumNode {
    pub value: Num,
}
impl Node for NumNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line, end);
        println!("NumNode: {:?}", self.value);
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.push_semantic_token(self.range, SemanticTokenType::NUMBER, 0);
        if let Num::INT(x) = self.value {
            let b = ctx.context.i64_type().const_int(x, true);
            return Ok((
                Value::IntValue(b),
                Some("i64".to_string()),
                TerminatorEnum::NONE,
                true,
            ));
        } else if let Num::FLOAT(x) = self.value {
            let b = ctx.context.f64_type().const_float(x);
            return Ok((
                Value::FloatValue(b),
                Some("f64".to_string()),
                TerminatorEnum::NONE,
                true,
            ));
        }
        panic!("not implemented")
    }
}

#[range]
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VarNode {
    pub name: String,
}
impl Node for VarNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("VarNode: {}", self.name);
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.if_completion(|ctx, a| {
            if a.0.is_in(self.range) {
                let completions = ctx.get_completions();
                ctx.completion_items.set(completions);
            }
        });
        let v = ctx.get_symbol(&self.name);
        if let Some((v, pltype, dst, refs, is_const)) = v {
            ctx.push_semantic_token(self.range, SemanticTokenType::VARIABLE, 0);
            let o = Ok((
                Value::VarValue(v.clone()),
                Some(pltype),
                TerminatorEnum::NONE,
                is_const,
            ));
            ctx.send_if_go_to_def(self.range, dst);
            ctx.set_if_refs(refs, self.range);
            return o;
        }
        if let Ok(tp) = ctx.get_type(&self.name, self.range) {
            if let PLType::FN(f) = tp {
                ctx.push_semantic_token(self.range, SemanticTokenType::FUNCTION, 0);
                return Ok((
                    Value::FnValue(f.get_value(ctx)),
                    Some(f.name.clone()),
                    TerminatorEnum::NONE,
                    true,
                ));
            } else if let PLType::STRUCT(s) = tp {
                ctx.push_semantic_token(self.range, SemanticTokenType::STRUCT, 0);
                return Ok((
                    Value::STValue(s.struct_type(ctx)),
                    Some(s.name.clone()),
                    TerminatorEnum::NONE,
                    true,
                ));
            }
        }
        Err(ctx.add_err(self.range, ErrorCode::VAR_NOT_FOUND))
    }
}
