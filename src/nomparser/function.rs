use std::fmt::Error;

use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map_res, opt},
    multi::many0,
    sequence::{delimited, preceded, tuple}, IResult,
};
use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;
use crate::{
    ast::node::function::{FuncDefNode, FuncTypeNode},
    ast::tokens::TokenType,
};

use internal_macro::test_parser;

use super::*;


/// ```ebnf
/// function_def = "fn" identifier "(" (typed_identifier (","typed_identifier)*)? ")" type_name (statement_block | newline) ;
/// ```
#[test_parser(
    "fn f(  x: int, y  : int  ) int {
        x = x+1;
        return 0;
    }
    "
)]
#[test_parser("fn   f (x: int ,\n y: int) int;")]
#[test_parser(
    "fn f(x: int) int {
        x = x+1;
        call();
        return 0;
    }
    "
)]
#[test_parser("             fn     f(x    :  int)    int;")]
#[test_parser(
    "fn f() int {
        x = x+1;
        return 0;
    }
    "
)]
#[test_parser("fn f( \n) int;")]
pub fn function_def(input: Span) -> IResult<Span, Box<TopLevel>> {
    map_res(
        tuple((
            many0(comment),
            tag_token(TokenType::FN),
            identifier,
            tag_token(TokenType::LPAREN),
            del_newline_or_space!(opt(tuple((
                del_newline_or_space!(typed_identifier),
                many0(preceded(
                    tag_token(TokenType::COMMA),
                    del_newline_or_space!(typed_identifier)
                )),
            )))),
            tag_token(TokenType::RPAREN),
            type_name,
            alt((
                map_res(statement_block, |b| Ok::<_, Error>(Some(b))),
                map_res(tag_token(TokenType::SEMI), |_| Ok::<_, Error>(None)),
            )),
        )),
        |(doc, _, id, _, paras, _, ret, body)| {
            let mut paralist = vec![];
            let range = id.range;
            if let Some(para) = paras {
                paralist.push(para.0);
                paralist.extend(para.1);
            }
            let node = FuncDefNode {
                typenode: FuncTypeNode {
                    id: id.name,
                    paralist,
                    ret,
                    range,
                    doc,
                    declare: body.is_none(),
                },
                body,
                range,
            };
            Ok::<_, Error>(Box::new(TopLevel::FuncDef(node)))
        },
    )(input)
}

/// ```ebnf
/// call_function_op = "(" (logic_exp (","logic_exp)*)? ")" ;
/// ```
pub fn call_function_op(input: Span) -> IResult<Span, ComplexOp> {
    delspace(map_res(
        tuple((
            tag_token(TokenType::LPAREN),
            opt(tuple((
                del_newline_or_space!(logic_exp),
                many0(preceded(
                    tag_token(TokenType::COMMA),
                    del_newline_or_space!(logic_exp),
                )),
            ))),
            tag_token(TokenType::RPAREN),
        )),
        |(_, paras, _)| {
            let mut paralist = vec![];
            if let Some(paras) = paras {
                paralist.push(paras.0);
                paralist.extend(paras.1);
            }
            Ok::<_, Error>(ComplexOp::CallOp(paralist))
        },
    ))(input)
}