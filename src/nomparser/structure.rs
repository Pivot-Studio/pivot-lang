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

#[test_parser("a : 1")]
/// ```enbf
/// struct_init_field = identifier ":" logic_exp "," ;
/// ```
/// special: del newline or space
fn struct_init_field(input: Span) -> IResult<Span, Box<StructInitFieldNode>> {
    del_newline_or_space!(map_res(
        tuple((identifier, tag_token(TokenType::COLON), logic_exp,)),
        |(id, _, exp)| {
            let range = id.range.start.to(exp.range().end);
            Ok::<_, Error>(Box::new(StructInitFieldNode {
                id: *id,
                exp,
                range,
            }))
        },
    ))(input)
}

#[test_parser("a{a : 1}")]
#[test_parser("a{a : 1,b:2}")]
#[test_parser("a{}")]
/// ```enbf
/// struct_init = type_name "{" (struct_init_field ("," struct_init_field)* )? "}" ;
/// ```
pub fn struct_init(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(
        tuple((
            type_name,
            tag_token(TokenType::LBRACE),
            opt(tuple((
                del_newline_or_space!(struct_init_field),
                many0(preceded(
                    tag_token(TokenType::COMMA),
                    del_newline_or_space!(struct_init_field),
                )),
            ))),
            tag_token(TokenType::RBRACE),
        )),
        |(name, _, fields, _)| {
            let range;
            let mut fields_res = vec![];
            if let Some((first, second)) = fields {
                let last_range = if second.is_empty() {
                    first.range().end
                } else {
                    second.last().unwrap().range().end
                };
                range = name.range().start.to(last_range);
                fields_res.push(first);
                fields_res.extend(second);
            } else {
                range = name.range();
            }
            res_enum(
                StructInitNode {
                    tp: name,
                    fields: fields_res,
                    range,
                }
                .into(),
            )
        },
    )(input)
}
