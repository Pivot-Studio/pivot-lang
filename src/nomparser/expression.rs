use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::space1,
    combinator::{map, not, opt, peek},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

use crate::ast::{
    node::{
        function::{ClosureNode, FuncCallNode, GeneratorType},
        pointer::{PointerOpEnum, PointerOpNode},
    },
    range::Pos,
    tokens::TokenType,
};
use crate::{ast::node::macro_nodes::MacroCallNode, nomparser::Span};
use internal_macro::{test_parser, test_parser_error};

use super::{cast::as_exp, macro_parse::macro_call_op, string_literal::string_literal, *};

#[test_parser("a")]
pub fn general_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    logic_exp(input)
}

#[test_parser("a&&b")]
#[test_parser("a||b")]
#[test_parser("a &&  b")]
#[test_parser("a  || b")]
#[test_parser("a&&b||c")]
#[test_parser("a||b&&c")]
pub fn logic_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    parse_bin_ops!(compare_exp_eq, AND, OR)(input)
}

#[test_parser("a==b")]
#[test_parser("a!=b")]
fn compare_exp_eq(input: Span) -> IResult<Span, Box<NodeEnum>> {
    parse_bin_ops!(compare_exp, NE, EQ)(input)
}

#[test_parser("a>b")]
#[test_parser("a>=b")]
#[test_parser("a<b")]
#[test_parser("a<=b")]
fn compare_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    parse_bin_ops!(bit_or, GEQ, LEQ, LESS, GREATER)(input)
}

#[test_parser("a|b")]
fn bit_or(input: Span) -> IResult<Span, Box<NodeEnum>> {
    parse_bin_ops!(bit_xor, BIT_OR)(input)
}

#[test_parser("a^b")]
fn bit_xor(input: Span) -> IResult<Span, Box<NodeEnum>> {
    parse_bin_ops!(bit_and, BIT_XOR)(input)
}

#[test_parser("a&b")]
#[test_parser_error("a&&b")]
fn bit_and(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(map(
        tuple((
            bit_move,
            many0(tuple((
                alt((terminated(
                    tag_token_symbol(TokenType::BIT_AND),
                    not(peek(tag_token_symbol(TokenType::BIT_AND))),
                ),)),
                bit_move,
            ))),
        )),
        create_bin,
    ))(input)
}

#[test_parser("a<<b")]
#[test_parser("a>>b")]
fn bit_move(input: Span) -> IResult<Span, Box<NodeEnum>> {
    parse_bin_ops!(add_exp, BIT_LEFT_SHIFT, BIT_RIGHT_SHIFT)(input)
}

#[test_parser("a + 1")]
#[test_parser("a - 1")]
fn add_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    parse_bin_ops!(mul_exp, PLUS, MINUS)(input)
}

#[test_parser("1 * 1")]
#[test_parser("1 / 1")]
#[test_parser("1 % 1")]
fn mul_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    parse_bin_ops!(unary_exp, MUL, DIV, MOD)(input)
}

#[test_parser("-1")]
#[test_parser("!a")]
#[test_parser("~a")]
#[test_parser("await a")]
#[test_parser_error("+a")]
pub fn unary_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    // todo: consider to aligh the implementation with UnaryExp EBNF
    delspace(alt((
        pointer_exp,
        map(
            tuple((
                alt((
                    tag_token_symbol(TokenType::MINUS),
                    tag_token_symbol(TokenType::NOT),
                    tag_token_symbol(TokenType::BIT_NOT),
                    terminated(
                        tag_token(TokenType::AWAIT),
                        alt((space1, tag("\r\n"), tag("\n"))),
                    ), // must follow by a space or newline
                )),
                pointer_exp,
            )),
            |((op, op_range), exp)| {
                let range = op_range.start.to(exp.range().end);

                Box::new(
                    UnaryOpNode {
                        op: (op, op_range),
                        exp,
                        range,
                    }
                    .into(),
                )
            },
        ),
    )))(input)
}

#[test_parser("&&a{}.d")]
#[test_parser("***ad")]
pub fn pointer_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map(
        delspace(pair(
            many0(alt((
                tag_token_symbol(TokenType::TAKE_PTR),
                tag_token_symbol(TokenType::TAKE_VAL),
            ))),
            as_exp,
        )),
        |(ops, exp)| {
            let exp_range = exp.range();
            let mut exp = exp;
            for op in ops.into_iter().rev() {
                let (op, op_range) = op;
                let op = match op {
                    TokenType::TAKE_PTR => PointerOpEnum::Addr,
                    TokenType::TAKE_VAL => PointerOpEnum::Deref,
                    _ => unreachable!(),
                };
                let range = op_range.start.to(exp_range.end);
                exp = Box::new(
                    PointerOpNode {
                        op,
                        value: exp,
                        range,
                        comments: vec![],
                    }
                    .into(),
                );
            }
            exp
        },
    )(input)
}

#[test_parser("adasda::c!(saddsada)")]
fn macro_call_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map(pair(extern_identifier, macro_call_op), |(name, op)| {
        Box::new(
            MacroCallNode {
                range: name.range(),
                callee: name,
                args: op.to_string(),
                inner_start: Pos::from_span(&op),
            }
            .into(),
        )
    })(input)
}

#[test_parser("a[1][2]()[3].b()()[4].c")]
#[test_parser("a{}.d")]
#[test_parser("ad")]
#[test_parser("a<i64>{}")]
#[test_parser("1.xxx()")]
pub fn complex_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map(
        pair(
            primary_exp,
            many0(alt((take_exp_op, array_element_op, call_function_op))),
        ),
        |(head, ops)| {
            let mut res = head;
            for op in ops {
                res = match op.0 {
                    ComplexOp::Call((args, params_range, generic_params)) => {
                        let mut range = res.range();
                        if !args.is_empty() {
                            range = res.range().start.to(params_range.end);
                        }
                        Box::new(
                            FuncCallNode {
                                generic_params,
                                range,
                                callee: res,
                                paralist: args,
                                comments: vec![op.1],
                                generic_infer: None,
                            }
                            .into(),
                        )
                    }
                    ComplexOp::Index(index) => {
                        let range = res.range().start.to(index.range().end);
                        Box::new(
                            ArrayElementNode {
                                range,
                                arr: res,
                                index,
                                comments: vec![op.1],
                            }
                            .into(),
                        )
                    }
                    ComplexOp::Field(field) => {
                        let range = if field.is_some() {
                            res.range().start.to(field.clone().unwrap().range().end)
                        } else {
                            let mut end = res.range().end;
                            end.column += 1;
                            end.offset += 1;
                            res.range().start.to(end)
                        };
                        Box::new(
                            TakeOpNode {
                                range,
                                head: res,
                                field,
                                comments: vec![op.1],
                            }
                            .into(),
                        )
                    }
                }
            }
            res
        },
    )(input)
}

fn primary_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(map(
        tuple((
            many0(comment),
            alt((
                closure,
                map(number, |n| Box::new(n.into())),
                map(bool_const, |n| Box::new(n.into())),
                parantheses_exp,
                struct_init,
                array_init,
                tuple_init,
                macro_call_exp,
                extern_identifier,
                map(string_literal, |n| Box::new(n.into())),
            )),
            many0(comment),
        )),
        |(lcoms, node, rcoms)| {
            let range = node.range();

            Box::new(
                PrimaryNode {
                    value: node,
                    comments: vec![lcoms, rcoms],
                    range,
                }
                .into(),
            )
        },
    ))(input)
}

#[test_parser(".0")]
fn take_exp_op(input: Span) -> IResult<Span, (ComplexOp, Vec<Box<NodeEnum>>)> {
    delspace(map(
        preceded(
            tag_token_symbol(TokenType::DOT),
            pair(
                opt(alt((identifier, tuple_field_identifier))),
                many0(comment),
            ),
        ),
        |(idx, coms)| (ComplexOp::Field(idx), coms),
    ))(input)
}

#[test_parser("(a)")]
#[test_parser("(a+a*b/c)")]
fn parantheses_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map(
        delimited(
            tag_token_symbol(TokenType::LPAREN),
            parse_with_ex(general_exp, false),
            tag_token_symbol(TokenType::RPAREN),
        ),
        |exp| {
            Box::new(
                ParanthesesNode {
                    range: exp.range(),
                    node: exp,
                }
                .into(),
            )
        },
    )(input)
}

#[test_parser("|a:i64| =>void{return a;}")]
#[test_parser("|a| =>void{return a;}")]
#[test_parser("|a| =>{return a;}")]
#[test_parser("async |a| =>{return a;}")]
fn closure(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map(
        tuple((
            tuple((
                opt(tag_modifier(TokenType::ASYNC_MARKER)),
                tag_token_symbol_ex(TokenType::GENERIC_SEP),
                separated_list0(
                    tag_token_symbol_ex(TokenType::COMMA),
                    pair(
                        identifier,
                        opt(preceded(tag_token_symbol_ex(TokenType::COLON), type_name)),
                    ),
                ),
                tag_token_symbol_ex(TokenType::GENERIC_SEP),
            )),
            tag_token_symbol_ex(TokenType::ARROW),
            opt(type_name),
            statement_block,
        )),
        |((modi, (_, sr), args, _), _, ret, body)| {
            let range = sr.start.to(body.range().end);

            Box::new(
                ClosureNode {
                    range,
                    paralist: args,
                    body,
                    ret,
                    ret_id: None,
                    generator_ty: match modi {
                        Some(_) => GeneratorType::Async,
                        None => GeneratorType::None,
                    },
                }
                .into(),
            )
        },
    )(input)
}
