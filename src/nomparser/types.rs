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
use internal_macro::{test_parser, test_parser_error};
use nom::sequence::preceded;
use nom::{
    branch::alt,
    combinator::{map_res, opt},
    multi::{many0, separated_list1},
    sequence::{pair, tuple},
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
pub fn basic_type(input: Span) -> IResult<Span, Box<TypeNodeEnum>> {
    delspace(map_res(
        tuple((extern_identifier, opt(generic_param_def))),
        |(exid, generic_params)| {
            let exid = match *exid {
                NodeEnum::ExternIdNode(exid) => exid,
                _ => unreachable!(),
            };
            let range = exid.range;
            Ok::<_, ()>(Box::new(TypeNodeEnum::BasicTypeNode(TypeNameNode {
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

            Ok::<_, ()>(Box::new(TypeNodeEnum::ArrayTypeNode(ArrayTypeNameNode {
                id: tp,
                size,
                range,
            })))
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
            Ok::<_, ()>(Box::new(GenericDefNode {
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
                    map_res(type_name, |x| Ok::<_, ()>(Some(x))),
                    map_res(tag_token_word(TokenType::INGNORE), |_| Ok::<_, ()>(None)),
                )),
            ),
            tag_token_symbol(TokenType::GREATER),
        )),
        |(lf, ids, ri)| {
            let range = lf.1.start.to(ri.1.end);
            Ok::<_, ()>(Box::new(GenericParamNode {
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
#[test_parser_error(
    "traitmytrait<A|B|C> {
    fn a() A;
}"
)]
pub fn trait_def(input: Span) -> IResult<Span, Box<TraitDefNode>> {
    map_res(
        tuple((
            modifiable(tag_token_word(TokenType::TRAIT), TokenType::PUB),
            identifier,
            opt(generic_type_def),
            opt(preceded(tag_token_symbol(TokenType::COLON), type_add)),
            del_newline_or_space!(tag_token_symbol(TokenType::LBRACE)),
            many0(del_newline_or_space!(function_def)),
            del_newline_or_space!(tag_token_symbol(TokenType::RBRACE)),
        )),
        |((modifier, _), id, generics, derives, _, defs, (_, rr))| {
            let range = id.range().start.to(rr.end);
            let mut de = vec![];
            if let Some(derives) = derives {
                for d in derives {
                    de.push(d);
                }
            }
            Ok::<_, ()>(Box::new(TraitDefNode {
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
                derives: de,
                modifier,
            }))
        },
    )(input)
}

/// ```enbf
/// type_add = type_name ("+" type_name)* ;
/// ```
#[test_parser("a+b+c")]
#[test_parser("a")]
#[test_parser(
    "a + b
    +
    c"
)]
fn type_add(input: Span) -> IResult<Span, Vec<Box<TypeNodeEnum>>> {
    separated_list1(
        del_newline_or_space!(tag_token_symbol(TokenType::PLUS)),
        type_name,
    )(input)
}
