use internal_macro::node;

use crate::ast::{range::Range, tokens::TokenType};

use super::*;
use super::{primary::VarNode, NodeEnum};

#[node]
pub struct MacroNode {
    pub id: VarNode,
    pub rules: Vec<MacroRuleNode>,
}

impl PrintTrait for MacroNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("MacroNode");
        for st in &self.rules {
            // st.0.print(tabs + 1, false, line.clone());
            for st in &st.body {
                st.print(tabs + 1, false, line.clone());
            }
        }
    }
}

impl Node for MacroNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        Ok((None, None, TerminatorEnum::NONE))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MacroMatchExp {
    Parameter(MacroMatchParameter),
    RawTokens((String, Range)),
    Parantheses((Vec<MacroMatchExp>, Range)),
    Looper((Vec<MacroMatchExp>, Range, TokenType)),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroMatchParameter {
    pub id: VarNode,
    pub tp: (TokenType, Range),
    pub range: Range,
}

#[node]
pub struct MacroRuleNode {
    pub match_exp: MacroMatchExp,
    pub body: Vec<Box<NodeEnum>>,
}

#[node]
pub struct MacroLoopStatementNode {
    pub statements: Vec<Box<NodeEnum>>,
}

impl PrintTrait for MacroLoopStatementNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("MacroLoopStatementNode");
        for st in &self.statements {
            st.print(tabs + 1, false, line.clone());
        }
    }
}

impl Node for MacroLoopStatementNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        Ok((None, None, TerminatorEnum::NONE))
    }
}

#[node]
pub struct MacroCallNode {
    pub args: String,
    pub callee: Box<NodeEnum>,
}

impl PrintTrait for MacroCallNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("MacroCallNode");
        self.callee.print(tabs + 1, false, line.clone());
    }
}

impl Node for MacroCallNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        Ok((None, None, TerminatorEnum::NONE))
    }
}
