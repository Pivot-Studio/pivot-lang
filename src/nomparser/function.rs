use std::fmt::Error;

use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map_res, opt},
    multi::{many0, separated_list0},
    sequence::{delimited, tuple},
    IResult,
};
use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;
use crate::{ast::node::function::FuncDefNode, ast::tokens::TokenType};

use internal_macro::test_parser;

use super::*;

/// ```ebnf
/// function_def = "fn" identifier "(" (typed_identifier (","typed_identifier)*)? ")" type_name (statement_block | newline) ;
/// ```
#[test_parser(
    "fn f(  x: int, y  : int  ) int {
        x = x+1;
        return 0;
    }
    "
)]
#[test_parser("fn   f (x: int ,\n y: int) int;")]
#[test_parser(
    "fn f(x: int) int {
        x = x+1;
        call();
        return 0;
    }
    "
)]
#[test_parser("             fn     f(x    :  int)    int;")]
#[test_parser(
    "fn f() int {
        x = x+1;
        return 0;
    }
    "
)]
#[test_parser("fn f( \n) int;")]
pub fn function_def(input: Span) -> IResult<Span, Box<TopLevel>> {
    map_res(
        tuple((
            many0(del_newline_or_space!(comment)),
            tag_token(TokenType::FN),
            identifier,
            opt(generic_type_def),
            tag_token(TokenType::LPAREN),
            del_newline_or_space!(separated_list0(
                tag_token(TokenType::COMMA),
                del_newline_or_space!(typed_identifier),
            )),
            tag_token(TokenType::RPAREN),
            type_name,
            alt((
                map_res(statement_block, |b| {
                    Ok::<_, Error>((Some(b.clone()), b.range))
                }),
                map_res(tag_token(TokenType::SEMI), |(_, range)| {
                    Ok::<_, Error>((None, range))
                }),
            )),
        )),
        |(doc, (_, start), id, generics, _, paras, _, ret, (body, end))| {
            let range = start.start.to(end.end);
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
            let node = FuncDefNode {
                id,
                paralist: paras,
                ret,
                range,
                doc: docs,
                precom: precoms,
                declare: body.is_none(),
                generics,
                body,
            };
            Ok::<_, Error>(Box::new(TopLevel::FuncType(node)))
        },
    )(input)
}

/// ```ebnf
/// call_function_op = "(" (logic_exp (","logic_exp)*)? ")" ;
/// ```
#[test_parser("(a,c,c)")]
#[test_parser("<T|S::k|i64>(a,c,c)")]
pub fn call_function_op(input: Span) -> IResult<Span, (ComplexOp, Vec<Box<NodeEnum>>)> {
    delspace(map_res(
        tuple((
            opt(generic_param_def),
            tag_token(TokenType::LPAREN),
            del_newline_or_space!(separated_list0(
                tag_token(TokenType::COMMA),
                del_newline_or_space!(logic_exp)
            )),
            tag_token(TokenType::RPAREN),
            many0(comment)
        )),
        |(generic, (_, st), paras, (_, end), com)| {
            Ok::<_, Error>((ComplexOp::CallOp((paras, st.start.to(end.end), generic)), com))
        },
    ))(input)
}
