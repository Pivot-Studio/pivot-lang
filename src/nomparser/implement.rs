use crate::ast::{node::implement::ImplNode, tokens::TokenType};
use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::map_res,
    multi::many0,
    sequence::{delimited, tuple},
    IResult,
};

use internal_macro::test_parser;

use super::*;

/// ```ebnf
/// impl_def = "impl" extern_identifier "{" (function_def)* "}" ;
/// ```
#[test_parser(
    "impl a::b::c {
        fn f(x: int) int {
            x = x+1;
            return 0;
        }
    }"
)]
#[test_parser(
    "impl c {
        fn f(x: int) int {
            x = x+1;
            return 0;
        }
        fn f2(x: int) int {
            x = x+1;
            return 0;
        }
    }"
)]
pub fn impl_def(input: Span) -> IResult<Span, Box<TopLevel>> {
    map_res(
        tuple((
            tag_token(TokenType::IMPL),
            type_name,
            del_newline_or_space!(tag_token(TokenType::LBRACE)),
            many0(del_newline_or_space!(function_def)),
            del_newline_or_space!(tag_token(TokenType::RBRACE)),
        )),
        |(_, tp, (_, start), func_def, (_, end))| {
            res_box(Box::new(TopLevel::ImplDef(ImplNode {
                range: start.start.to(end.end),
                target: tp,
                methods: func_def
                    .iter()
                    .map(|x| {
                        let d = (*x).clone();
                        match *d {
                            TopLevel::FuncDef(x) => x.clone().into(),
                            _ => unreachable!(),
                        }
                    })
                    .collect(),
            })))
        },
    )(input)
}
