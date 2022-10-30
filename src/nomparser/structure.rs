use std::fmt::Error;

use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map_res, opt},
    multi::many0,
    sequence::{delimited, tuple},
    IResult,
};
use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;
use crate::{
    ast::node::types::StructDefNode,
    ast::node::{types::StructInitNode, NodeEnum, RangeTrait},
    ast::{node::types::StructInitFieldNode, tokens::TokenType},
};
use internal_macro::test_parser;

use super::*;

#[test_parser(
    "struct mystruct {
    myname: int;//123
    myname2: int;
}"
)]
pub fn struct_def(input: Span) -> IResult<Span, Box<TopLevel>> {
    map_res(
        tuple((
            many0(comment),
            tag_token(TokenType::STRUCT),
            identifier,
            del_newline_or_space!(tag_token(TokenType::LBRACE)),
            many0(tuple((
                del_newline_or_space!(typed_identifier),
                opt(tag_token(TokenType::SEMI)),
                opt(comment),
            ))),
            del_newline_or_space!(tag_token(TokenType::RBRACE)),
        )),
        |(doc, _, id, _, fields, _)| {
            let range = id.range;
            let mut fieldlist = vec![];
            for mut f in fields {
                f.0.doc = None;
                if let Some(c) = &f.2 {
                    if let NodeEnum::Comment(c) = *c.clone() {
                        f.0.doc = Some(c);
                    }
                }
                fieldlist.push((f.0.clone(), f.1.is_some()));
            }
            Ok::<_, Error>(Box::new(TopLevel::StructDef(StructDefNode {
                doc,
                id: id.name,
                fields: fieldlist,
                range,
            })))
        },
    )(input)
}

#[test_parser("a : 1,")]
/// ```enbf
/// struct_init_field = identifier ":" logic_exp "," ;
/// ```
/// special: del newline or space
fn struct_init_field(input: Span) -> IResult<Span, Box<NodeEnum>> {
    del_newline_or_space!(map_res(
        tuple((
            identifier,
            tag_token(TokenType::COLON),
            logic_exp,
            opt(tag_token(TokenType::COMMA))
        )),
        |(id, _, exp, has_comma)| {
            let range = id.range.start.to(exp.range().end);
            res_enum(
                StructInitFieldNode {
                    id: *id,
                    exp,
                    range,
                    has_comma: has_comma.is_some(),
                }
                .into(),
            )
        },
    ))(input)
}

#[test_parser("a{a : 1,}")]
#[test_parser("a{a : 1,b:2,}")]
#[test_parser("a{}")]
/// ```enbf
/// struct_init = type_name "{" struct_init_field "}" ;
/// ```
pub fn struct_init(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(
        tuple((
            type_name,
            tag_token(TokenType::LBRACE),
            many0(struct_init_field),
            tag_token(TokenType::RBRACE),
        )),
        |(name, _, fields, _)| {
            let range;
            if let Some(last) = fields.last() {
                range = name.range().start.to(last.range().end);
            } else {
                range = name.range();
            }
            res_enum(
                StructInitNode {
                    tp: name,
                    fields,
                    range,
                }
                .into(),
            )
        },
    )(input)
}
