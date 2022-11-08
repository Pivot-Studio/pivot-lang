use std::fmt::Error;

use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map_res, opt},
    multi::many0,
    sequence::{delimited, preceded, tuple},
    IResult,
};
use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;
use crate::{
    ast::node::types::ArrayInitNode,
    ast::tokens::TokenType,
    ast::{diag::ErrorCode, node::error::ErrorNode},
};

use super::*;

pub fn array_init(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(
        tuple((
            tag_token(TokenType::LBRACKET),
            opt(tuple((
                del_newline_or_space!(logic_exp),
                many0(preceded(
                    tag_token(TokenType::COMMA),
                    del_newline_or_space!(logic_exp),
                )),
            ))),
            tag_token(TokenType::RBRACKET),
        )),
        |(_, exps, _)| {
            // TODO:get range from token
            let range = Default::default();
            let mut exp_res = vec![];
            if let Some((first, second)) = exps {
                exp_res.push(first);
                exp_res.extend(second);
            }
            res_enum(
                ArrayInitNode {
                    exps: exp_res,
                    range,
                }
                .into(),
            )
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
        |(_, idx, (_, rr))| {
            if let Some(idx) = idx {
                Ok::<_, Error>(ComplexOp::IndexOp(idx))
            } else {
                Ok::<_, Error>(ComplexOp::IndexOp(Box::new(NodeEnum::Err(ErrorNode {
                    msg: String::from("Nedded index for array element access"),
                    src: String::from("[]"),
                    code: ErrorCode::NEEDED_INDEX_FOR_ARRAY_ELEMENT_ACCESS,
                    range: rr,
                }))))
            }
        },
    ))(input)
}
