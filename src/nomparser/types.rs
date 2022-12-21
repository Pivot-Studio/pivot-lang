use std::fmt::Error;

use crate::nomparser::Span;
use crate::{
    ast::node::types::{ArrayTypeNameNode, TypeNameNode},
    ast::{
        node::{
            interface::TraitDefNode,
            types::{GenericDefNode, GenericParamNode, PointerTypeNode},
        },
        tokens::TokenType,
    },
};
use internal_macro::test_parser;
use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map_res, opt},
    multi::{many0, separated_list1},
    sequence::{delimited, pair, tuple},
    IResult,
};

use super::*;

pub fn type_name(input: Span) -> IResult<Span, Box<TypeNodeEnum>> {
    delspace(map_res(
        pair(
            many0(tag_token_symbol(TokenType::TAKE_VAL)),
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
#[test_parser("kfsh<a|b|c>")]
fn basic_type(input: Span) -> IResult<Span, Box<TypeNodeEnum>> {
    delspace(map_res(
        tuple((extern_identifier, opt(generic_param_def))),
        |(exid, generic_params)| {
            let exid = match *exid {
                NodeEnum::ExternIdNode(exid) => exid,
                _ => unreachable!(),
            };
            let range = exid.range;
            Ok::<_, Error>(Box::new(TypeNodeEnum::BasicTypeNode(TypeNameNode {
                generic_params,
                id: Some(exid),
                range,
            })))
        },
    ))(input)
}

fn array_type(input: Span) -> IResult<Span, Box<TypeNodeEnum>> {
    map_res(
        tuple((
            tag_token_symbol(TokenType::LBRACKET),
            type_name,
            tag_token_symbol(TokenType::MUL),
            number,
            tag_token_symbol(TokenType::RBRACKET),
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

/// ```enbf
/// generic_type_def = "<" identifier ("|" identifier)* ">" ;
/// ```
/// 形参
pub fn generic_type_def(input: Span) -> IResult<Span, Box<GenericDefNode>> {
    map_res(
        tuple((
            tag_token_symbol(TokenType::LESS),
            separated_list1(tag_token_symbol(TokenType::GENERIC_SEP), identifier),
            tag_token_symbol(TokenType::GREATER),
        )),
        |(lf, ids, ri)| {
            let range = lf.1.start.to(ri.1.end);
            Ok::<_, Error>(Box::new(GenericDefNode {
                range,
                generics: ids,
            }))
        },
    )(input)
}

/// ```enbf
/// generic_param_def = "<" (extern_id|"_") ("|"(extern_id|"_"))* ">" ;
/// ```
/// 实参
#[test_parser("<a|b|B::c>")]
#[test_parser("<a>")]
pub fn generic_param_def(input: Span) -> IResult<Span, Box<GenericParamNode>> {
    map_res(
        tuple((
            tag_token_symbol(TokenType::LESS),
            separated_list1(
                tag_token_symbol(TokenType::GENERIC_SEP),
                alt((
                    map_res(type_name, |x| Ok::<_, Error>(Some(x))),
                    map_res(tag_token_word(TokenType::INGNORE), |_| Ok::<_, Error>(None)),
                )),
            ),
            tag_token_symbol(TokenType::GREATER),
        )),
        |(lf, ids, ri)| {
            let range = lf.1.start.to(ri.1.end);
            Ok::<_, Error>(Box::new(GenericParamNode {
                range,
                generics: ids,
            }))
        },
    )(input)
}

/// ```enbf
/// trait_def = "trait" identifier generic_type_def? "{" function_def* "}" ;
/// ```
#[test_parser(
    "trait mytrait<A|B|C> {
    fn a() A;
}"
)]
pub fn trait_def(input: Span) -> IResult<Span, Box<TraitDefNode>> {
    map_res(
        tuple((
            tag_token_word(TokenType::TRAIT),
            identifier,
            opt(generic_type_def),
            del_newline_or_space!(tag_token_symbol(TokenType::LBRACE)),
            many0(del_newline_or_space!(function_def)),
            del_newline_or_space!(tag_token_symbol(TokenType::RBRACE)),
        )),
        |(_, id, generics, _, defs, (_, rr))| {
            let range = id.range().start.to(rr.end);
            Ok::<_, Error>(Box::new(TraitDefNode {
                id,
                generics,
                methods: defs
                    .into_iter()
                    .map(|x| match *x {
                        TopLevel::FuncType(f) => f,
                        _ => unreachable!(),
                    })
                    .collect(),
                range,
            }))
        },
    )(input)
}
