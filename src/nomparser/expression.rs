use std::fmt::Error;

use nom::{
    branch::alt,
    combinator::{map_res, opt},
    multi::many0,
    sequence::{delimited, pair, preceded, tuple},
    IResult
};
use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;
use crate::{
    ast::node::function::FuncCallNode,
    ast::tokens::TokenType,
};
use internal_macro::{test_parser, test_parser_error};


use super::*;

#[test_parser("a&&b")]
#[test_parser("a||b")]
pub fn logic_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    parse_bin_ops!(compare_exp, AND, OR)(input)
}

#[test_parser("a>b")]
#[test_parser("a>=b")]
#[test_parser("a<b")]
#[test_parser("a<=b")]
#[test_parser("a==b")]
#[test_parser("a!=b")]
fn compare_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    parse_bin_ops!(add_exp, GEQ, LEQ, NE, EQ, LESS, GREATER)(input)
}

#[test_parser("a + 1")]
#[test_parser("a - 1")]
fn add_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    parse_bin_ops!(mul_exp, PLUS, MINUS)(input)
}

#[test_parser("1 * 1")]
#[test_parser("1 / 1")]
fn mul_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    parse_bin_ops!(unary_exp, MUL, DIV)(input)
}

#[test_parser("-1")]
#[test_parser("!a")]
#[test_parser_error("+a")]
fn unary_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(alt((
        complex_exp,
        map_res(
            tuple((
                alt((tag_token(TokenType::MINUS), tag_token(TokenType::NOT))),
                complex_exp,
            )),
            |((op, _), exp)| {
                let range = exp.range();
                res_enum(UnaryOpNode { op, exp, range }.into())
            },
        ),
    )))(input)
}




/// ```ebnf
/// complex_exp = primary_exp (take_exp_op|array_element_op|call_function_exp_op)*;
/// ```
#[test_parser("a[1][2]()[3].b()()[4].c")]
#[test_parser("a{}.d")]
#[test_parser("ad")]
pub fn complex_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(
        pair(
            primary_exp,
            many0(alt((take_exp_op, array_element_op, call_function_op))),
        ),
        |(head, ops)| {
            let mut res = head;
            for op in ops {
                res = match op {
                    ComplexOp::CallOp(args) => {
                        let mut range = res.range();
                        if args.len() > 0 {
                            range = res.range().start.to(args.last().unwrap().range().end);
                        }
                        Box::new(
                            FuncCallNode {
                                range,
                                id: res,
                                paralist: args,
                            }
                            .into(),
                        )
                    }
                    ComplexOp::IndexOp(index) => {
                        let range = res.range().start.to(index.range().end);
                        Box::new(
                            ArrayElementNode {
                                range,
                                arr: res,
                                index,
                            }
                            .into(),
                        )
                    }
                    ComplexOp::FieldOp(field) => {
                        let range;
                        if field.is_some() {
                            range = res.range().start.to(field.clone().unwrap().range().end);
                        } else {
                            let mut end = res.range().end;
                            end.column = end.column + 1;
                            range = res.range().start.to(end);
                        }
                        Box::new(
                            TakeOpNode {
                                range,
                                head: res,
                                field,
                            }
                            .into(),
                        )
                    }
                }
            }
            res_enum(*res)
        },
    )(input)
}

fn primary_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(alt((
        number,
        bool_const,
        delimited(
            tag_token(TokenType::LPAREN),
            logic_exp,
            tag_token(TokenType::RPAREN),
        ),
        struct_init,
        array_init,
        extern_identifier,
    )))(input)
}

/// ```ebnf
/// take_exp_op = ("." identifier?) ;
/// ```
fn take_exp_op(input: Span) -> IResult<Span, ComplexOp> {
    delspace(map_res(
        preceded(tag_token(TokenType::DOT), opt(identifier)),
        |idx| Ok::<_, Error>(ComplexOp::FieldOp(idx)),
    ))(input)
}