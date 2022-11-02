use std::fmt::Error;

use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map_res, opt, recognize},
    multi::many0,
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};
use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;
use crate::{
    ast::node::ret::RetNode,
    ast::range::Range,
    ast::tokens::TokenType,
    ast::{
        diag::ErrorCode,
        node::{
            error::{ErrorNode, STErrorNode},
            global::GlobalNode,
        },
    },
};
use internal_macro::{test_parser, test_parser_error};

use super::*;

fn empty_statement(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(tag_token(TokenType::SEMI), |_| {
        res_enum(
            EmptyNode {
                range: Range::new(input, input),
            }
            .into(),
        )
    })(input)
}

#[test_parser("{let a = 1;}")]
#[test_parser("{}")]
#[test_parser(
    "{

}"
)]
pub fn statement_block(input: Span) -> IResult<Span, StatementsNode> {
    delspace(delimited(
        del_newline_or_space!(tag_token(TokenType::LBRACE)),
        statements,
        del_newline_or_space!(tag_token(TokenType::RBRACE)),
    ))(input)
}

fn statements(input: Span) -> IResult<Span, StatementsNode> {
    map_res(many0(del_newline_or_space!(statement)), |v| {
        let mut range = Range::new(input, input);
        let la = v.last();
        if let Some(la) = la {
            range = range.start.to(la.range().end);
        }
        Ok::<_, Error>(StatementsNode {
            statements: v,
            range,
        })
    })(input)
}

/// ```ebnf
/// statement =
/// | new_variable newline
/// | assignment newline
/// | if_statement
/// | while_statement
/// | for_statement
/// | break_statement
/// | continue_statement
/// | return_statement
/// | newline
/// ;
/// ```
fn statement(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(alt((
        semi_statement!(new_variable),
        semi_statement!(assignment),
        if_statement,
        while_statement,
        for_statement,
        break_statement,
        continue_statement,
        return_statement,
        semi_statement!(pointer_exp),
        empty_statement,
        comment,
        except(
            "\n\r}",
            "failed to parse statement",
            ErrorCode::SYNTAX_ERROR_STATEMENT,
        ),
    )))(input)
}

#[test_parser("let a = 1")]
pub fn new_variable(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(preceded(
        tag_token(TokenType::LET),
        alt((
            map_res(
                tuple((identifier, tag_token(TokenType::ASSIGN), logic_exp)),
                |(a, _, v)| {
                    let range = a.range().start.to(v.range().end);
                    res_enum(
                        DefNode {
                            var: *a,
                            tp: None,
                            exp: Some(v),
                            range,
                        }
                        .into(),
                    )
                },
            ),
            map_res(
                tuple((identifier, tag_token(TokenType::COLON), type_name)),
                |(a, _, tp)| {
                    let range = a.range().start.to(tp.range().end);
                    res_enum(
                        DefNode {
                            var: *a,
                            tp: Some(tp),
                            exp: None,
                            range,
                        }
                        .into(),
                    )
                },
            ),
        )),
    ))(input)
}

#[test_parser("a = 1")]
pub fn assignment(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(map_res(
        tuple((pointer_exp, tag_token(TokenType::ASSIGN), logic_exp)),
        |(left, _op, right)| {
            let range = left.range().start.to(right.range().end);
            res_enum(
                AssignNode {
                    var: left,
                    exp: right,
                    range,
                }
                .into(),
            )
        },
    ))(input)
}

#[test_parser("return 1;")]
#[test_parser("return 1.1;")]
#[test_parser("return true;")]
#[test_parser("return;")]
#[test_parser("return a;")]
#[test_parser("return 1 + 2;")]
#[test_parser_error("return a = 2;")]
// ```
// return_statement = "return" logic_exp newline ;
// ```
fn return_statement(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(map_res(
        tuple((
            tag_token(TokenType::RETURN),
            opt(logic_exp),
            tag_token(TokenType::SEMI),
        )),
        |((_, range), val, _)| {
            if let Some(val) = val {
                let range = val.range();
                res_enum(
                    RetNode {
                        value: Some(val),
                        range,
                    }
                    .into(),
                )
            } else {
                res_enum(RetNode { value: None, range }.into())
            }
        },
    ))(input)
}

#[test_parser("const a = 1")]
pub fn global_variable(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(map_res(
        tuple((
            tag_token(TokenType::CONST),
            identifier,
            tag_token(TokenType::ASSIGN),
            logic_exp,
        )),
        |(_, var, _, exp)| {
            let range = var.range().start.to(exp.range().end);
            res_enum(
                GlobalNode {
                    var: *var,
                    exp,
                    range,
                }
                .into(),
            )
        },
    ))(input)
}
