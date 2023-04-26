use crate::ast::node::tuple::TupleInitNode;
use crate::ast::node::types::StructField;
use crate::nomparser::Span;
use crate::{
    ast::node::types::StructDefNode,
    ast::node::{types::StructInitNode, NodeEnum, RangeTrait},
    ast::{node::types::StructInitFieldNode, tokens::TokenType},
};
use internal_macro::{test_parser, test_parser_error};
use nom::multi::separated_list1;
use nom::{
    branch::alt,
    combinator::{map_res, opt},
    multi::many0,
    sequence::{pair, terminated, tuple},
    IResult,
};
use nom_locate::LocatedSpan;

use super::*;

#[test_parser(
    "struct mystruct<A|B|C> {
    myname: int;//123
    myname2: int;
}"
)]
#[test_parser(
    "pub struct mystruct<A|B|C> {
    myname: int;//123
    myname2: int;
}"
)]
#[test_parser_error(
    "structmystruct<A|B|C> {
    myname: int;//123
    myname2: int;
}"
)]
#[test_parser_error(
    "pubstruct mystruct<A|B|C> {
    myname: int;//123
    myname2: int;
}"
)]
pub fn struct_def(input: Span) -> IResult<Span, Box<TopLevel>> {
    map_res(
        tuple((
            many0(del_newline_or_space!(comment)),
            modifiable(tag_token_word(TokenType::STRUCT), TokenType::PUB),
            identifier,
            opt(generic_type_def),
            del_newline_or_space!(tag_token_symbol(TokenType::LBRACE)),
            many0(tuple((
                del_newline_or_space!(modifiable(typed_identifier, TokenType::PUB)),
                opt(tag_token_symbol(TokenType::SEMI)),
                opt(comment),
            ))),
            del_newline_or_space!(tag_token_symbol(TokenType::RBRACE)),
        )),
        |(doc, (modifier, (_, start)), id, generics, _, fields, (_, end))| {
            let range = start.start.to(end.end);
            let mut fieldlist = vec![];
            for mut f in fields {
                f.0 .1.doc = None;
                if let Some(c) = &f.2 {
                    if let NodeEnum::Comment(c) = *c.clone() {
                        f.0 .1.doc = Some(c);
                    }
                }
                fieldlist.push(StructField {
                    id: f.0 .1.clone(),
                    has_semi: f.1.is_some(),
                    modifier: f.0 .0,
                });
            }
            let mut docs = vec![];
            let mut precoms = vec![];
            for d in doc {
                if let NodeEnum::Comment(com) = *d {
                    if com.is_doc {
                        docs.push(Box::new(NodeEnum::Comment(com.clone())));
                    }
                    precoms.push(Box::new(NodeEnum::Comment(com)));
                }
            }
            Ok::<_, ()>(Box::new(TopLevel::StructDef(StructDefNode {
                pre_comments: precoms,
                doc: docs,
                id,
                fields: fieldlist,
                range,
                generics,
                modifier,
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
        tuple((identifier, tag_token_symbol(TokenType::COLON), general_exp,)),
        |(id, _, exp)| {
            let range = id.range.start.to(exp.range().end);
            Ok::<_, ()>(Box::new(StructInitFieldNode {
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
#[test_parser("a<i64|B>{}")]
/// ```enbf
/// struct_init = type_name "{" (struct_init_field ("," struct_init_field)* )? "}" ;
/// ```
pub fn struct_init(input: Span) -> IResult<Span, Box<NodeEnum>> {
    if input.extra {
        // extra为true代表在if的逻辑表达区域内，为了避免二义性 跳过 struct init
        return Err(nom::Err::Error(
            nom::error::Error::<LocatedSpan<&str, bool>>::new(input, nom::error::ErrorKind::Fail),
        ));
    }
    map_res(
        tuple((
            basic_type,
            del_newline_or_space!(tag_token_symbol(TokenType::LBRACE)),
            alt((
                map_res(
                    pair(
                        many0(tuple((
                            terminated(
                                del_newline_or_space!(struct_init_field),
                                tag_token_symbol(TokenType::COMMA),
                            ),
                            many0(comment),
                        ))),
                        del_newline_or_space!(struct_init_field),
                    ),
                    |(lfields, rfield)| {
                        let mut fields = vec![];
                        let mut coms = vec![];
                        for f in lfields {
                            fields.push(f.0);
                            coms.push(f.1);
                        }
                        fields.push(rfield);
                        Ok::<_, ()>((fields, coms))
                    },
                ),
                map_res(opt(del_newline_or_space!(struct_init_field)), |field| {
                    if field.is_some() {
                        Ok::<_, ()>((vec![field.unwrap()], vec![]))
                    } else {
                        Ok::<_, ()>((vec![], vec![]))
                    }
                }),
            )),
            many0(comment),
            del_newline_or_space!(tag_token_symbol(TokenType::RBRACE)),
        )),
        |(typename, _, (fields, lcomment), rcomment, _)| {
            let range = if !fields.is_empty() {
                typename
                    .range()
                    .start
                    .to(fields.last().unwrap().range().end)
            } else {
                typename.range()
            };
            let mut comments = lcomment;
            comments.push(rcomment);
            res_enum(
                StructInitNode {
                    typename,
                    fields,
                    range,
                    comments,
                }
                .into(),
            )
        },
    )(input)
}

#[test_parser("(1,2,a)")]
#[test_parser("()")]
#[test_parser("(2,)")]
#[test_parser_error("(,)")]
pub fn tuple_init(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(
        tuple((
            tag_token_symbol(TokenType::LPAREN),
            opt(terminated(
                separated_list1(tag_token_symbol_ex(TokenType::COMMA), general_exp),
                opt(tag_token_symbol_ex(TokenType::COMMA)),
            )),
            tag_token_symbol(TokenType::RPAREN),
        )),
        |((_, rs), exprs, (_, re))| {
            let range = rs.start.to(re.end);
            let exprs = exprs.unwrap_or(vec![]);
            res_enum(TupleInitNode { exprs, range }.into())
        },
    )(input)
}
