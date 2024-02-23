use internal_macro::node;
use nom::multi::many0;
use nom::IResult;

use super::*;
use super::{primary::VarNode, NodeEnum};
use crate::ast::ctx::MacroReplaceNode;
use crate::ast::diag::DiagCode;
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
    pub file: String,
}

impl PrintTrait for MacroNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("MacroNode");
        for st in &self.rules {
            st.body.print(tabs + 1, false, line.clone());
        }
    }
}

impl Node for MacroNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        _builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        ctx.send_if_go_to_def(self.range, self.range, ctx.get_file());
        ctx.set_glob_refs(&ctx.plmod.get_full_name(&self.id.name), self.id.range);
        ctx.push_semantic_token(self.id.range, SemanticTokenType::MACRO, 0);
        self.file = ctx.plmod.path.clone();
        ctx.plmod.add_macro(self);
        Ok(Default::default())
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
                                    ctx.get_file(),
                                    format_label!("got {}, expect {}", *args.trim(), t.trim()),
                                )
                                .clone(),
                        )
                    })?;
                Ok((re.0, ()))
            }
            MacroMatchExp::Parantheses((ts, r)) => {
                let (mut new, _) = tag_token_symbol_ex(TokenType::LPAREN)(args)
                    .map_err(|_| nom::Err::Error(r.new_err(ErrorCode::UNEXPECTED_TOKEN)))?;
                for t in ts {
                    (new, _) = t.parse(ctx, new)?;
                }
                let (new, _) = tag_token_symbol_ex(TokenType::RPAREN)(args)
                    .map_err(|_| nom::Err::Error(r.new_err(ErrorCode::UNEXPECTED_TOKEN)))?;
                Ok((new, ()))
            }
            MacroMatchExp::Looper((ts, _r, _m)) => {
                let mut new = args;
                loop {
                    for t in ts {
                        let re = ctx.with_macro_loop_parse(|ctx| t.parse(ctx, new));
                        if re.is_err() {
                            return Ok((new, ()));
                        }
                        (new, _) = re?;
                    }
                    if new.len() == 0 {
                        break;
                    }
                }
                Ok((new, ()))
            }
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
                self.add_to_macro_var(ctx, NodeEnum::Var(*node));
                Ok((new, ()))
            }
            TokenType::MACRO_TYPE_STR => {
                let (new, node) = del_newline_or_space!(string_literal::string_literal)(args)
                    .map_err(|_| nom::Err::Error(self.range.new_err(ErrorCode::EXPECT_STRING)))?;
                self.add_to_macro_var(ctx, node.into());
                Ok((new, ()))
            }
            TokenType::MACRO_TYPE_EXPR => {
                let (new, node) = expression::general_exp(args).map_err(|_| {
                    nom::Err::Error(self.range.new_err(ErrorCode::EXPECT_EXPRESSION))
                })?;
                self.add_to_macro_var(ctx, *node);
                Ok((new, ()))
            }
            TokenType::MACRO_TYPE_STMT => {
                let (new, node) = del_newline_or_space!(crate::nomparser::statement::statement)(
                    args,
                )
                .map_err(|_| nom::Err::Error(self.range.new_err(ErrorCode::EXPECT_STATEMENT)))?;
                self.add_to_macro_var(ctx, *node);
                Ok((new, ()))
            }
            TokenType::MACRO_TYPE_STMTS => {
                let (new, nodes) = many0(del_newline_or_space!(
                    crate::nomparser::statement::statement
                ))(args)
                .map_err(|_| nom::Err::Error(self.range.new_err(ErrorCode::EXPECT_STATEMENTS)))?;
                let sts = StatementsNode {
                    statements: nodes,
                    range: Default::default(),
                };
                self.add_to_macro_var(ctx, sts.into());
                Ok((new, ()))
            }
            _ => unreachable!(),
        }
    }

    fn add_to_macro_var(&self, ctx: &mut Ctx, node: NodeEnum) {
        if ctx.macro_loop {
            let mut default = MacroReplaceNode::LoopNodeEnum(vec![]);
            let n = ctx
                .macro_vars
                .get_mut(&self.id.name)
                .unwrap_or(&mut default);
            if let MacroReplaceNode::LoopNodeEnum(v) = n {
                v.push(node);
                if v.len() == 1 {
                    ctx.macro_vars.insert(self.id.name.clone(), default);
                }
            }
            return;
        }
        ctx.macro_vars
            .insert(self.id.name.clone(), MacroReplaceNode::NodeEnum(node));
    }
}

#[node]
pub struct MacroRuleNode {
    pub match_exp: Vec<MacroMatchExp>,
    pub body: NodeEnum,
}

#[node]
pub struct MacroLoopStatementNode {
    pub statements: Box<NodeEnum>,
}

impl PrintTrait for MacroLoopStatementNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("MacroLoopStatementNode");
        self.statements.print(tabs + 1, false, line.clone());
    }
}

impl Node for MacroLoopStatementNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        ctx.with_macro_loop(|ctx| self.statements.emit(ctx, builder), self.range)
    }
}

#[node]
pub struct MacroCallNode {
    pub args: String,
    pub callee: Box<NodeEnum>,
    pub inner_start: Pos,
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
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        let ctx = &mut ctx.new_child(self.range.start, builder);
        ctx.macro_loop_idx = Default::default();
        ctx.macro_loop_len = Default::default();
        ctx.macro_loop = false;
        match &*self.callee {
            NodeEnum::ExternIdNode(ex_node) => {
                for ns in &ex_node.namespace {
                    ctx.push_semantic_token(ns.range(), SemanticTokenType::NAMESPACE, 0);
                }
                ctx.push_semantic_token(ex_node.id.range(), SemanticTokenType::MACRO, 0);
                let m = ex_node.get_macro(ctx)?;
                ctx.send_if_go_to_def(self.range, m.range, m.file.clone());
                ctx.set_glob_refs(&format!("{}..{}", &m.file, &m.id.name), self.range);
                let src = m.file.clone();
                let span = unsafe {
                    Span::new_from_raw_offset(
                        self.inner_start.offset,
                        self.inner_start.line as u32,
                        &self.args,
                        false,
                    )
                };
                let mut last_err: Option<PLDiag> = None;
                for rule in &m.rules {
                    let mut span = span;
                    let mut next = false;
                    for e in rule.match_exp.iter() {
                        let re = e.parse(ctx, span).map_err(|e| {
                            if let nom::Err::Error(mut e) = e {
                                e.add_label(
                                    self.range,
                                    ctx.get_file(),
                                    format_label!("the macro is called here"),
                                );
                                e
                            } else {
                                unreachable!()
                            }
                        });
                        match re {
                            Ok((new, _)) => {
                                span = new;
                            }
                            Err(e) => {
                                last_err = Some(e);
                                next = true;
                                break;
                            }
                        }
                    }
                    if span.len() != 0 {
                        last_err = Some(
                            self.range
                                .new_err(ErrorCode::UNEXPECTED_TOKEN)
                                .set_source(&ctx.get_file())
                                .clone(),
                        );
                        continue;
                    }
                    if next {
                        continue;
                    }
                    let mut b = rule.clone().body;
                    let mut child = ctx.new_child(self.range.start, builder);
                    let re = child
                        .with_diag_src(&src, |ctx| ctx.with_macro_emit(|ctx| b.emit(ctx, builder)));
                    match re {
                        Ok(_) => {
                            return Ok(Default::default());
                        }
                        Err(mut e) => {
                            if e.get_diag_code()
                                == DiagCode::Err(ErrorCode::MACRO_EXPAND_DEPTH_TOO_DEEP)
                            {
                                e.set_range(b.range());
                            }
                            last_err = Some(e);
                            continue;
                        }
                    }
                }
                if let Some(e) = last_err {
                    ctx.with_diag_src(&src, |ctx| e.add_to_ctx(ctx));
                    return Err(e);
                }
            }
            _ => unreachable!(),
        }

        Ok(Default::default())
    }
}
