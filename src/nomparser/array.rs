use std::fmt::Error;

use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map_res, opt},
    multi::{many0, separated_list0},
    sequence::{delimited, tuple},
    IResult,
};

use crate::nomparser::Span;
use crate::{
    ast::node::types::ArrayInitNode,
    ast::tokens::TokenType,
    ast::{diag::ErrorCode, node::error::ErrorNode},
};

use super::*;

pub fn array_init(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(
        tuple((
            tag_token_symbol(TokenType::LBRACKET),
            separated_list0(
                tag_token_symbol(TokenType::COMMA),
                del_newline_or_space!(logic_exp),
            ),
            tag_token_symbol(TokenType::RBRACKET),
        )),
        |((_, lb), exps, (_, rb))| {
            let range = lb.start.to(rb.end);
            res_enum(ArrayInitNode { exps, range }.into())
        },
    )(input)
}

/// ```ebnf
/// array_element_op = ('[' logic_exp ']') ;
/// ```
pub fn array_element_op(input: Span) -> IResult<Span, (ComplexOp, Vec<Box<NodeEnum>>)> {
    delspace(map_res(
        tuple((
            tag_token_symbol(TokenType::LBRACKET),
            opt(logic_exp),
            tag_token_symbol(TokenType::RBRACKET),
            many0(comment),
        )),
        |(_, idx, (_, rr), com)| {
            if let Some(idx) = idx {
                Ok::<_, Error>((ComplexOp::IndexOp(idx), com))
            } else {
                Ok::<_, Error>((
                    ComplexOp::IndexOp(Box::new(NodeEnum::Err(ErrorNode {
                        msg: String::from("Nedded index for array element access"),
                        src: String::from("[]"),
                        code: ErrorCode::NEEDED_INDEX_FOR_ARRAY_ELEMENT_ACCESS,
                        range: rr,
                    }))),
                    com,
                ))
            }
        },
    ))(input)
}
