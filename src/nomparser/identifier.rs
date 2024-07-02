use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, one_of},
    combinator::{map_res, opt, recognize},
    multi::{many0_count, many1, separated_list1},
    sequence::{pair, tuple},
    IResult, InputTake,
};
use ustr::Ustr;

use crate::nomparser::Span;
use crate::{
    ast::tokens::TokenType,
    ast::{
        node::{
            pkg::ExternIdNode,
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
#[test_parser("b::c::$b")]
#[test_parser("b::$c::$b")]
#[test_parser("a")]
#[test_parser("a:")]
#[test_parser("a::")]
pub fn extern_identifier(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(map_res(
        tuple((
            separated_list1(
                tag_token_symbol(TokenType::DOUBLE_COLON),
                delspace(identifier),
            ),
            opt(tag_token_symbol(TokenType::DOUBLE_COLON)), // 容忍未写完的语句
            opt(tag_token_symbol(TokenType::COLON)),        // 容忍未写完的语句
        )),
        |(mut identifier_with_namespace, opt, opt2)| {
            let id = identifier_with_namespace.first().unwrap();
            let lastid = identifier_with_namespace.last().unwrap().clone();

            let mut range = id.range().start.to(lastid.range().end);
            if let Some(opt) = opt {
                range = range.start.to(opt.1.end);
            }
            if let Some(opt2) = opt2 {
                range = range.start.to(opt2.1.end);
            }
            identifier_with_namespace.pop();
            res_enum(
                ExternIdNode {
                    // after poping, only namespaces are left
                    namespace: identifier_with_namespace,
                    id: lastid,
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
            alt((alpha1::<Span, nom::error::Error<Span>>, tag("_"), tag("$"))),
            many0_count(alt((alphanumeric1, tag("_")))),
        )),
        |out| {
            let a = TOKEN_STR_MAP.get(out.fragment());
            if a.is_some() {
                return Err(());
            }
            Ok(Box::new(VarNode {
                name: Ustr::from(&out),
                range: Range::new(&out, &out.take_split(out.len()).0),
                id: None,
            }))
        },
    ))(input)
}

pub fn tuple_field_identifier(input: Span) -> IResult<Span, Box<VarNode>> {
    delspace(map_res(
        recognize(many1(one_of("0123456789"))),
        |out: Span| {
            Ok::<_, ()>(Box::new(VarNode {
                name: Ustr::from(&out),
                range: Range::new(&out, &out.take_split(out.len()).0),
                id: None,
            }))
        },
    ))(input)
}

#[test_parser("myname: int")]
pub fn typed_identifier(input: Span) -> IResult<Span, Box<TypedIdentifierNode>> {
    delspace(map_res(
        tuple((
            identifier,
            tag_token_symbol(TokenType::COLON),
            opt(type_name),
            opt(comment),
        )),
        |(id, _, type_name, d)| {
            let mut range = id.range;
            let mut tprange = range;
            tprange.end.column += 1;
            tprange.start = tprange.end;
            let mut typenode = Box::new(TypeNodeEnum::Basic(TypeNameNode {
                id: None,
                range: tprange,
                generic_params: None,
                generic_infer: None,
            }));

            let mut doc = None;
            if let Some(d1) = d {
                if let NodeEnum::Comment(d1) = *d1 {
                    doc = Some(d1);
                }
            }

            if let Some(type_name) = type_name {
                range = id.range.start.to(type_name.range().end);
                typenode = type_name;
            }

            res_box(Box::new(TypedIdentifierNode {
                id: *id,
                typenode,
                doc,
                range,
            }))
        },
    ))(input)
}
