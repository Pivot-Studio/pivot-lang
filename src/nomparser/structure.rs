use std::fmt::Error;

use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map_res, opt},
    multi::many0,
    sequence::{delimited, tuple, pair, terminated},
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
    "struct mystruct<A|B|C> {
    myname: int;//123
    myname2: int;
}"
)]
pub fn struct_def(input: Span) -> IResult<Span, Box<TopLevel>> {
    map_res(
        tuple((
            many0(del_newline_or_space!(comment)),
            tag_token(TokenType::STRUCT),
            identifier,
            opt(generic_type_def),
            del_newline_or_space!(tag_token(TokenType::LBRACE)),
            many0(tuple((
                del_newline_or_space!(typed_identifier),
                opt(tag_token(TokenType::SEMI)),
                opt(comment),
            ))),
            del_newline_or_space!(tag_token(TokenType::RBRACE)),
        )),
        |(doc, (_, start), id, generics, _, fields, (_, end))| {
            let range = start.start.to(end.end);
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
            let mut docs = vec![];
            let mut precoms = vec![];
            for d in doc{
                if let NodeEnum::Comment(com) = *d{
                    if com.is_doc {
                        docs.push(Box::new(NodeEnum::Comment(com.clone())));
                    }
                    precoms.push(Box::new(NodeEnum::Comment(com)));
                }
            }
            Ok::<_, Error>(Box::new(TopLevel::StructDef(StructDefNode {
                precom: precoms,
                doc: docs,
                id,
                fields: fieldlist,
                range,
                generics,
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
#[test_parser("a<i64|B>{}")]
/// ```enbf
/// struct_init = type_name "{" (struct_init_field ("," struct_init_field)* )? "}" ;
/// ```
pub fn struct_init(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(
        tuple((
            type_name,
            opt(generic_param_def),
            tag_token(TokenType::LBRACE),
            alt((
                map_res(pair(
                    many0(tuple((terminated(del_newline_or_space!(struct_init_field),tag_token(TokenType::COMMA)), many0(comment)))),
                    del_newline_or_space!(struct_init_field)
                ),|(lfields, rfield)|{
                    let mut fields = vec![];
                    let mut coms = vec![];
                    for f in lfields{
                        fields.push(f.0);
                        coms.push(f.1);
                    }
                    fields.push(rfield);
                    Ok::<_, Error>((fields, coms))
                }
                ),
                map_res(
                    opt(del_newline_or_space!(struct_init_field)),
                    |field|{
                        if field.is_some(){
                            Ok::<_, Error>((vec![field.unwrap()], vec![]))
                        }else{
                            Ok::<_, Error>((vec![], vec![]))
                        }
                    }
                )
            )),
            many0(comment),
            tag_token(TokenType::RBRACE),
        )),
        |(name, generic_params, _, (fields, lcomment), rcomment, _)| {
            let range = if fields.len() > 0 {
                name.range().start.to(fields.last().unwrap().range().end)
            } else {
                name.range()
            };
            let mut comments = lcomment;
            comments.push(rcomment); 
            res_enum(
                StructInitNode {
                    generic_params,
                    typename: name,
                    fields,
                    range,
                    comments
                }
                .into(),
            )
        },
    )(input)
}
