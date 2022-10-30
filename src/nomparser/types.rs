use std::fmt::Error;

use nom::{
    branch::alt,
    combinator::map_res,
    sequence::tuple, IResult
};
use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;
use crate::{
    ast::node::types::{
                ArrayTypeNameNode, TypeNameNode,
            },
    ast::tokens::TokenType,
};
use internal_macro::test_parser;


use super::*;

pub fn type_name(input: Span) -> IResult<Span, Box<TypeNodeEnum>> {
    delspace(alt((basic_type, array_type)))(input)
}

#[test_parser("kfsh")]
fn basic_type(input: Span) -> IResult<Span, Box<TypeNodeEnum>> {
    delspace(map_res(extern_identifier, |exid| {
        let exid = match *exid {
            NodeEnum::ExternIDNode(exid) => exid,
            _ => unreachable!(),
        };
        let range = exid.range;
        Ok::<_, Error>(Box::new(TypeNodeEnum::BasicTypeNode(TypeNameNode {
            id: Some(exid),
            range,
        })))
    }))(input)
}

fn array_type(input: Span) -> IResult<Span, Box<TypeNodeEnum>> {
    map_res(
        tuple((
            tag_token(TokenType::LBRACKET),
            type_name,
            tag_token(TokenType::MUL),
            number,
            tag_token(TokenType::RBRACKET),
        )),
        |(_, tp, _, size, _)| {
            let range = size.range().start.to(tp.range().end);

            Ok::<_, Error>(Box::new(TypeNodeEnum::ArrayTypeNode(
                ArrayTypeNameNode {
                    id: tp,
                    size,
                    range,
                }
                .into(),
            )))
        },
    )(input)
}