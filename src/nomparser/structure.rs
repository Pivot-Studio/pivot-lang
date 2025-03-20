use crate::ast::diag::ErrorCode;
use crate::ast::node::error::ErrorNode;
use crate::ast::node::tuple::TupleInitNode;
use crate::ast::node::types::ParsedField;
use crate::ast::node::types::StructField;
use crate::ast::node::types::TypedIdentifierNode;
use crate::nomparser::Span;
use crate::{
    ast::node::types::StructDefNode,
    ast::node::{types::StructInitNode, NodeEnum, RangeTrait},
    ast::range::{Pos, Range},
    ast::{node::types::StructInitFieldNode, tokens::TokenType},
};
use internal_macro::{test_parser, test_parser_error};
use nom::branch::alt;
use nom::combinator::map;
use nom::multi::separated_list1;
use nom::{
    bytes::complete::take,
    combinator::{map_res, opt},
    multi::many0,
    sequence::{terminated, tuple},
    IResult,
};

use super::*;

// 定义take_until_either函数，用于捕获直到特定符号的所有内容
fn take_until_either(
    token_type1: TokenType,
    token_type2: TokenType,
) -> impl Fn(Span) -> IResult<Span, (String, Range)> {
    move |input: Span| {
        let start_pos = Pos {
            line: input.location_line() as usize,
            column: input.get_utf8_column(), // 默认值
            offset: input.location_offset(),
        };

        let mut current = input.clone();
        let mut content = String::new();

        while !current.is_empty() {
            if let Ok((_rest, _)) = tag_token_symbol(token_type1)(current.clone()) {
                let end_pos = Pos {
                    line: current.location_line() as usize,
                    column: current.get_utf8_column(), // 默认值
                    offset: current.location_offset(),
                };

                let range = Range {
                    start: start_pos,
                    end: end_pos,
                };

                if content.is_empty() {
                    return Err(nom::Err::Error(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::Fail,
                    )));
                }

                return Ok((current, (content, range)));
            }

            if let Ok((_rest, _)) = tag_token_symbol(token_type2)(current.clone()) {
                let end_pos = Pos {
                    line: current.location_line() as usize,
                    column: current.get_utf8_column(), // 默认值
                    offset: current.location_offset(),
                };

                let range = Range {
                    start: start_pos,
                    end: end_pos,
                };

                if content.is_empty() {
                    return Err(nom::Err::Error(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::Fail,
                    )));
                }

                return Ok((current, (content, range)));
            }

            if current.is_empty() {
                break;
            }

            let (rest, c) = take(1usize)(current)?;
            content.push_str(c.fragment());
            current = rest;
        }

        Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Fail,
        )))
    }
}

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
                alt((
                    map(
                        del_newline_or_space!(modifiable(typed_identifier, TokenType::PUB)),
                        Ok::<_, Box<NodeEnum>>,
                    ),
                    map(
                        del_newline_or_space!(take_until_either(
                            TokenType::SEMI,
                            TokenType::RBRACE
                        )),
                        |(s, r)| {
                            let err = ErrorNode {
                                msg: "invalid field definition".to_string(),
                                src: s,
                                range: r,
                                code: ErrorCode::INVALID_STRUCT_FIELD,
                            };
                            Err::<
                                (Option<(TokenType, Range)>, Box<TypedIdentifierNode>),
                                Box<NodeEnum>,
                            >(Box::new(NodeEnum::Err(err)))
                        },
                    ),
                )),
                opt(tag_token_symbol(TokenType::SEMI)),
                opt(comment),
            ))),
            del_newline_or_space!(tag_token_symbol(TokenType::RBRACE)),
        )),
        |(doc, (modifier, (_, start)), id, generics, _, fields, (_, end))| {
            let range = start.start.to(end.end);
            let mut fieldlist = vec![];

            for (field_res, semi, comment) in fields {
                match field_res {
                    Ok((modifier, mut typed_id)) => {
                        typed_id.doc = None;
                        if let Some(c) = &comment {
                            if let NodeEnum::Comment(c) = &**c {
                                typed_id.doc = Some(c.clone());
                            }
                        }
                        fieldlist.push(ParsedField::Normal(StructField {
                            id: typed_id,
                            has_semi: semi.is_some(),
                            modifier,
                        }));
                    }
                    Err(err_node) => {
                        fieldlist.push(ParsedField::Err(err_node));
                    }
                }
            }
            let mut docs = vec![];
            let mut precoms = vec![];
            for d in doc {
                if let NodeEnum::Comment(com) = &*d {
                    if com.is_doc {
                        docs.push(Box::new(NodeEnum::Comment(com.clone())));
                    }
                    precoms.push(Box::new(NodeEnum::Comment(com.clone())));
                }
            }
            Ok::<_, ()>(Box::new(TopLevel::StructDef(StructDefNode {
                docs,
                pre_comments: precoms,
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
fn struct_init_field(input: Span) -> IResult<Span, Box<NodeEnum>> {
    del_newline_or_space!(map_res(
        tuple((identifier, tag_token_symbol(TokenType::COLON), general_exp,)),
        |(id, _, exp)| {
            let range = id.range.start.to(exp.range().end);
            Ok::<_, ()>(Box::new(
                StructInitFieldNode {
                    id: *id,
                    exp,
                    range,
                }
                .into(),
            ))
        },
    ))(input)
}

#[test_parser("a {a : 1}")]
#[test_parser("a{a : 1,b:2}")]
#[test_parser("a{a : 1,b:2,}")]
#[test_parser("a{}")]
#[test_parser("a<i64|B>{}")]
#[test_parser(
    "a{a : 1,b:2,dsadasd   
}"
)] // fault tolerant
#[test_parser(
    "a{a : 1,b:2,dsadasd   

    }"
)] // fault tolerant
#[test_parser(
    "a{a : 1,dsadasd  
    
    ,b:2 
}"
)] // fault tolerant
#[test_parser(
    "a{a : 1,dsadasd  ,,,
}"
)] // fault tolerant

pub fn struct_init(input: Span) -> IResult<Span, Box<NodeEnum>> {
    if input.extra.extra {
        // extra为true代表在if的逻辑表达区域内，为了避免二义性 跳过 struct init
        return Err(nom::Err::Error(nom::error::Error::<Span>::new(
            input,
            nom::error::ErrorKind::Fail,
        )));
    }
    map_res(
        tuple((
            basic_type,
            tag_token_symbol_ex(TokenType::LBRACE),
            opt(terminated(
                separated_list1(
                    tag_token_symbol_ex(TokenType::COMMA),
                    alt((
                        del_newline_or_space!(alt_except(
                            struct_init_field,
                            "},",
                            "unexpected token",
                            ErrorCode::INVALID_STRUCT_INIT
                        )),
                        map(tag_token_symbol_ex(TokenType::COMMA), |(_, r)| {
                            Box::new(
                                ErrorNode {
                                    msg: "duplicate comma".to_string(),
                                    src: ",".to_string(),
                                    range: r,
                                    code: ErrorCode::REDUNDANT_COMMA,
                                }
                                .into(),
                            )
                        }),
                    )),
                ),
                opt(tag_token_symbol_ex(TokenType::COMMA)),
            )),
            tag_token_symbol(TokenType::RBRACE),
        )),
        |(typename, _, body, (_, e))| {
            let range = typename.range().start.to(e.end);
            res_enum(
                StructInitNode {
                    typename,
                    fields: body.unwrap_or_default(),
                    range,
                }
                .into(),
            )
        },
    )(input)
}

#[test_parser("(1,2,a)")]
#[test_parser("(1,2,(a,b, (dd,)))")]
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
            let exprs = exprs.unwrap_or_default();
            res_enum(TupleInitNode { exprs, range }.into())
        },
    )(input)
}
