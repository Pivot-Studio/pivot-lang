use std::fmt::Error;

use nom::{
    combinator::map_res,
    multi::many0,
    sequence::{delimited, terminated, tuple},
    IResult,
};
use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;
use crate::{ast::node::types::ArrayInitNode, ast::range::Range, ast::tokens::TokenType};

use super::*;

pub fn array_init(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(
        tuple((
            tag_token(TokenType::LBRACKET),
            many0(terminated(logic_exp, tag_token(TokenType::COMMA))),
            tag_token(TokenType::RBRACKET),
        )),
        |(_, exps, _)| {
            let range;
            if let Some(last) = exps.last() {
                range = last.range().start.to(last.range().end);
            } else {
                range = Range::default();
            }
            res_enum(ArrayInitNode { exps, range }.into())
        },
    )(input)
}

/// ```ebnf
/// array_element_op = ('[' logic_exp ']') ;
/// ```
pub fn array_element_op(input: Span) -> IResult<Span, ComplexOp> {
    delspace(map_res(
        delimited(
            tag_token(TokenType::LBRACKET),
            logic_exp,
            tag_token(TokenType::RBRACKET),
        ),
        |idx| Ok::<_, Error>(ComplexOp::IndexOp(idx)),
    ))(input)
}
