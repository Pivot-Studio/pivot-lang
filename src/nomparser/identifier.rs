use std::fmt::Error;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1},
    combinator::{map_res, opt, recognize},
    multi::{many0_count, separated_list1},
    sequence::{pair, tuple},
    IResult, InputTake,
};
use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;
use crate::{
    ast::tokens::TokenType,
    ast::{
        node::{
            pkg::ExternIDNode,
            types::{TypeNameNode, TypedIdentifierNode},
        },
        tokens::TOKEN_STR_MAP,
    },
    ast::{
        node::{primary::VarNode, NodeEnum, RangeTrait, TypeNodeEnum},
        range::Range,
    },
};
use internal_macro::test_parser;

use super::*;

#[test_parser("a::a")]
#[test_parser("b::c::b")]
#[test_parser("a")]
#[test_parser("a:")]
#[test_parser("a::")]
pub fn extern_identifier(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(map_res(
        tuple((
            separated_list1(tag_token(TokenType::DOUBLE_COLON), delspace(identifier)),
            opt(tag_token(TokenType::DOUBLE_COLON)), // 容忍未写完的语句
            opt(tag_token(TokenType::COLON)),        // 容忍未写完的语句
        )),
        |(mut ns, opt, opt2)| {
            let id = ns.pop().unwrap();
            let mut range = id.range();
            if opt.is_some() {
                range = range.start.to(opt.unwrap().1.end);
            }
            if opt2.is_some() {
                range = range.start.to(opt2.unwrap().1.end);
            }
            res_enum(
                ExternIDNode {
                    ns,
                    id,
                    range,
                    complete: opt.is_none() && opt2.is_none(),
                    singlecolon: opt2.is_some(),
                }
                .into(),
            )
        },
    ))(input)
}

pub fn identifier(input: Span) -> IResult<Span, Box<VarNode>> {
    delspace(map_res(
        recognize(pair(
            alt((alpha1::<Span, nom::error::Error<Span>>, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_")))),
        )),
        |out| {
            let a = TOKEN_STR_MAP.get(out.fragment());
            if a.is_some() {
                return Err(Error {});
            }
            Ok(Box::new(VarNode {
                name: out.to_string(),
                range: Range::new(out, out.take_split(out.len()).0),
            }))
        },
    ))(input)
}

#[test_parser("myname: int")]
pub fn typed_identifier(input: Span) -> IResult<Span, Box<TypedIdentifierNode>> {
    delspace(map_res(
        tuple((
            identifier,
            tag_token(TokenType::COLON),
            opt(type_name),
            opt(comment),
        )),
        |(id, _, type_name, d)| {
            let mut range = id.range;
            let mut tprange = range;
            tprange.end.column += 1;
            tprange.start = tprange.end;
            let mut tp = Box::new(TypeNodeEnum::BasicTypeNode(TypeNameNode {
                id: None,
                range: tprange,
            }));

            let mut doc = None;
            if let Some(d1) = d {
                if let NodeEnum::Comment(d1) = *d1 {
                    doc = Some(d1);
                }
            }

            if let Some(type_name) = type_name {
                range = id.range.start.to(type_name.range().end);
                tp = type_name;
            }

            res_box(Box::new(TypedIdentifierNode {
                id: *id,
                tp,
                doc,
                range,
            }))
        },
    ))(input)
}
