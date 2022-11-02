use std::fmt::Error;

use nom::{
    branch::alt,
    combinator::map_res,
    multi::many0,
    sequence::{pair, tuple},
    IResult,
};
use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;
use crate::{
    ast::node::types::{ArrayTypeNameNode, TypeNameNode},
    ast::{node::types::PointerTypeNode, tokens::TokenType},
};
use internal_macro::test_parser;

use super::*;

pub fn type_name(input: Span) -> IResult<Span, Box<TypeNodeEnum>> {
    delspace(map_res(
        pair(
            many0(tag_token(TokenType::TAKE_VAL)),
            alt((basic_type, array_type)),
        ),
        |(pts, n)| {
            let mut node = n;
            for _ in pts {
                let range = node.range();
                node = Box::new(TypeNodeEnum::PointerTypeNode(PointerTypeNode {
                    elm: node,
                    range,
                }));
            }
            res_box(node)
        },
    ))(input)
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
