use internal_macro::node;

use crate::ast::ctx::MacroReplaceNode;
use crate::ast::{range::Range, tokens::TokenType};
use crate::nomparser::identifier::identifier;
use crate::nomparser::Span;

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
        _builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        ctx.plmod.add_macro(&self);
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

impl MacroMatchExp {
    pub fn parse(&self, ctx: &mut Ctx, args: Span) {
        match self {
            MacroMatchExp::Parameter(p) => {
                p.parse(ctx, args);
            }
            MacroMatchExp::RawTokens(_) => todo!(),
            MacroMatchExp::Parantheses(_) => todo!(),
            MacroMatchExp::Looper(_) => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroMatchParameter {
    pub id: VarNode,
    pub tp: (TokenType, Range),
    pub range: Range,
}

impl MacroMatchParameter {
    pub fn parse(&self, ctx: &mut Ctx, args: Span) {
        match self.tp.0 {
            TokenType::MACRO_TYPE_ID => {
                let node = identifier(args).unwrap().1;
                ctx.macro_vars
                    .insert(self.id.name.clone(), MacroReplaceNode::VarNode(*node));
            }
            TokenType::MACRO_TYPE_STR => todo!(),
            TokenType::MACRO_TYPE_EXPR => todo!(),
            TokenType::MACRO_TYPE_STMT => todo!(),
            TokenType::MACRO_TYPE_STMTS => todo!(),
            _ => todo!(),
        }
    }
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
        _ctx: &'b mut Ctx<'a>,
        _builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        todo!()
        // Ok((None, None, TerminatorEnum::NONE))
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
        match &*self.callee {
            NodeEnum::ExternIdNode(ex_node) => {
                let m = ex_node.get_macro(ctx)?;
                m.rules[0].match_exp.parse(ctx, self.args.as_str().into());
                for n in m.rules[0].body.clone().iter_mut() {
                    n.emit(ctx, builder)?;
                }
            }
            _ => panic!("MacroCallNode::emit: callee is not an extern id node"),
        }

        Ok((None, None, TerminatorEnum::NONE))
        // Ok((None, None, TerminatorEnum::NONE))
    }
}
