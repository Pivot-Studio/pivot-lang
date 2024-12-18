use crate::ast::node::interface::{MultiTraitNode, TraitBoundNode};
use crate::ast::node::tuple::TupleTypeNode;
use crate::ast::node::types::ClosureTypeNode;
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
use nom::combinator::map;
use nom::multi::separated_list0;
use nom::sequence::{preceded, terminated};
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
            alt((basic_type, array_type, closure_type, tuple_type)),
        ),
        |(pts, n)| {
            let mut node = n;
            for _ in pts {
                let range = node.range();
                node = Box::new(TypeNodeEnum::Pointer(PointerTypeNode { elm: node, range }));
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
            Ok::<_, ()>(Box::new(TypeNodeEnum::Basic(TypeNameNode {
                generic_params,
                id: Some(exid),
                range,
                generic_infer: None,
            })))
        },
    ))(input)
}

#[test_parser("[i64]")]
#[test_parser("[[[i64]]]")]
fn array_type(input: Span) -> IResult<Span, Box<TypeNodeEnum>> {
    map_res(
        tuple((
            tag_token_symbol(TokenType::LBRACKET),
            type_name,
            tag_token_symbol(TokenType::RBRACKET),
        )),
        |(_, tp, _)| {
            let range = tp.range();
            Ok::<_, ()>(Box::new(TypeNodeEnum::Array(ArrayTypeNameNode {
                id: tp,
                range,
            })))
        },
    )(input)
}

pub fn generic_type_def(input: Span) -> IResult<Span, Box<GenericDefNode>> {
    map_res(
        tuple((
            tag_token_symbol(TokenType::LESS),
            separated_list1(tag_token_symbol(TokenType::GENERIC_SEP), trait_bound),
            tag_token_symbol(TokenType::GREATER),
        )),
        |(lf, ids, ri)| {
            let range = lf.1.start.to(ri.1.end);
            Ok::<_, ()>(Box::new(GenericDefNode {
                range,
                generics_size: ids.len(),
                generics: ids,
            }))
        },
    )(input)
}

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

#[test_parser(
    "trait mytrait {
    fn a() A;
}"
)]
#[test_parser_error(
    "traitmytrait {
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
                methods: defs
                    .into_iter()
                    .map(|x| match *x {
                        TopLevel::FuncType(mut f) => {
                            f.in_trait_def = true;
                            f
                        }
                        _ => unreachable!(),
                    })
                    .collect(),
                range,
                derives: MultiTraitNode {
                    traits: de,
                    range: Default::default(),
                },
                modifier,
                generics,
            }))
        },
    )(input)
}

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

pub fn trait_bound(input: Span) -> IResult<Span, Box<TraitBoundNode>> {
    map_res(
        tuple((
            identifier,
            opt(preceded(tag_token_symbol(TokenType::COLON), multi_trait)),
        )),
        |(ident, impl_trait)| {
            let range = if let Some(impl_trait) = &impl_trait {
                ident.range.start.to(impl_trait.range().end)
            } else {
                ident.range
            };
            res_box(Box::new(TraitBoundNode {
                identifier: ident,
                impl_trait,
                range,
            }))
        },
    )(input)
}

pub fn multi_trait(input: Span) -> IResult<Span, Box<MultiTraitNode>> {
    map(type_add, |traits| {
        Box::new(MultiTraitNode {
            range: traits
                .first()
                .unwrap()
                .range()
                .start
                .to(traits.last().unwrap().range().end),
            traits,
        })
    })(input)
}

#[test_parser("(i32,i64,(i32,i64))")]
#[test_parser("()")]
#[test_parser_error("(,)")]
fn tuple_type(input: Span) -> IResult<Span, Box<TypeNodeEnum>> {
    map_res(
        tuple((
            tag_token_symbol(TokenType::LPAREN),
            opt(terminated(
                separated_list1(tag_token_symbol_ex(TokenType::COMMA), type_name),
                opt(tag_token_symbol_ex(TokenType::COMMA)),
            )),
            tag_token_symbol(TokenType::RPAREN),
        )),
        |((_, rs), tps, (_, re))| {
            let range = rs.start.to(re.end);
            let types = tps.unwrap_or_default();
            let node = Box::new(TypeNodeEnum::Tuple(TupleTypeNode { types, range }));
            res_box(node)
        },
    )(input)
}

#[test_parser("|i32,i64,(i32,i64)| => i32")]
fn closure_type(input: Span) -> IResult<Span, Box<TypeNodeEnum>> {
    map_res(
        tuple((
            tag_token_symbol_ex(TokenType::GENERIC_SEP),
            separated_list0(tag_token_symbol_ex(TokenType::COMMA), type_name),
            tag_token_symbol_ex(TokenType::GENERIC_SEP),
            tag_token_symbol_ex(TokenType::ARROW),
            type_name,
        )),
        |((_, lrange), arg_types, _, _, ret)| {
            let range = lrange.start.to(ret.range().end);
            let node = Box::new(TypeNodeEnum::Closure(ClosureTypeNode {
                arg_types,
                ret_type: ret,
                range,
            }));
            res_box(node)
        },
    )(input)
}
