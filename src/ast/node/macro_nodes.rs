use internal_macro::node;
use nom::IResult;

use super::*;
use super::{primary::VarNode, NodeEnum};
use crate::ast::ctx::MacroReplaceNode;
use crate::ast::{range::Range, tokens::TokenType};
use crate::nomparser::helper::tag_token_symbol_ex;
use crate::nomparser::identifier::identifier;
use crate::nomparser::{expression, string_literal, Span};
use crate::{del_newline_or_space, format_label};
use nom::bytes::complete::tag;

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
    pub fn parse<'a, 'b: 'a>(
        &'b self,
        ctx: &mut Ctx,
        args: Span<'a>,
    ) -> IResult<Span<'a>, (), PLDiag> {
        match self {
            MacroMatchExp::Parameter(p) => p.parse(ctx, args),
            MacroMatchExp::RawTokens((t, r)) => {
                let re: (Span, Span) =
                    del_newline_or_space!(tag(t.as_str()))(args).map_err(|_: nom::Err<()>| {
                        nom::Err::Error(
                            r.new_err(ErrorCode::UNEXPECTED_TOKEN)
                                .add_label(
                                    *r,
                                    format_label!("got {}, expect {}", *args.trim(), t.trim()),
                                )
                                .clone(),
                        )
                    })?;
                Ok((re.0, ()))
            }
            MacroMatchExp::Parantheses((ts, r)) => {
                let (new, _) = tag_token_symbol_ex(TokenType::LPAREN)(args)
                    .map_err(|_| nom::Err::Error(r.new_err(ErrorCode::UNEXPECTED_TOKEN)))?;
                for t in ts {
                    t.parse(ctx, new)?;
                }
                let (new, _) = tag_token_symbol_ex(TokenType::RPAREN)(args)
                    .map_err(|_| nom::Err::Error(r.new_err(ErrorCode::UNEXPECTED_TOKEN)))?;
                Ok((new, ()))
            }
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
    pub fn parse<'a, 'b: 'a>(
        &'b self,
        ctx: &mut Ctx,
        args: Span<'a>,
    ) -> IResult<Span<'a>, (), PLDiag> {
        match self.tp.0 {
            TokenType::MACRO_TYPE_ID => {
                let (new, node) = identifier(args).map_err(|_| {
                    nom::Err::Error(self.range.new_err(ErrorCode::EXPECT_IDENTIFIER))
                })?;
                ctx.macro_vars
                    .insert(self.id.name.clone(), MacroReplaceNode::VarNode(*node));
                Ok((new, ()))
            }
            TokenType::MACRO_TYPE_STR => {
                let (new, node) = del_newline_or_space!(string_literal::string_literal)(args)
                    .map_err(|_| nom::Err::Error(self.range.new_err(ErrorCode::EXPECT_STRING)))?;
                ctx.macro_vars
                    .insert(self.id.name.clone(), MacroReplaceNode::NodeEnum(*node));
                Ok((new, ()))
            }
            TokenType::MACRO_TYPE_EXPR => {
                let (new, node) = expression::logic_exp(args).map_err(|_| {
                    nom::Err::Error(self.range.new_err(ErrorCode::EXPECT_EXPRESSION))
                })?;
                ctx.macro_vars
                    .insert(self.id.name.clone(), MacroReplaceNode::NodeEnum(*node));
                Ok((new, ()))
            }
            TokenType::MACRO_TYPE_STMT => todo!(),
            TokenType::MACRO_TYPE_STMTS => todo!(),
            _ => todo!(),
        }
    }
}

#[node]
pub struct MacroRuleNode {
    pub match_exp: Vec<MacroMatchExp>,
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
                let mut span = self.args.as_str().into();
                for e in m.rules[0].match_exp.iter() {
                    (span, _) = e.parse(ctx, span).map_err(|e| {
                        if let nom::Err::Error(e) = e {
                            let mut e = e.clone();
                            e.set_range(self.range)
                                .add_label(self.range, format_label!("the macro is called here"))
                                .add_to_ctx(ctx)
                        } else {
                            unreachable!()
                        }
                    })?;
                }
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
