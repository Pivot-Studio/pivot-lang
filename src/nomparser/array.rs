use std::fmt::Error;

use nom::{
    combinator::{map_res, opt},
    multi::many0,
    sequence::{terminated, tuple},
    IResult,
};
use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;
use crate::{ast::node::types::ArrayInitNode, ast::{range::Range, node::error::ErrorNode, diag::ErrorCode}, ast::tokens::TokenType};

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
        tuple((
            tag_token(TokenType::LBRACKET),
            opt(logic_exp),
            tag_token(TokenType::RBRACKET),
        )),
        |(_,idx,(_,rr))| {
            if let Some(idx) = idx{
                Ok::<_, Error>(ComplexOp::IndexOp(idx))
            } else {
                Ok::<_, Error>(ComplexOp::IndexOp(Box::new(NodeEnum::Err(
                    ErrorNode{ 
                        msg: String::from("Nedded index for array element access"), 
                        src: String::from("[]"), 
                        code: ErrorCode::NEEDED_INDEX_FOR_ARRAY_ELEMENT_ACCESS, 
                        range: rr
                    }
                ))))
            }
        },
    ))(input)
}
