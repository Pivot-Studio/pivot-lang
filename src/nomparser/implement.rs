use crate::ast::{node::implement::ImplNode, tokens::TokenType};
use error::eat_any_err_block_before;
use nom::{
    combinator::{map_res, opt},
    multi::many0,
    sequence::{pair, tuple},
    IResult,
};

use internal_macro::{test_parser, test_parser_error};

use super::*;

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
#[test_parser(
    "impl <T> c {
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
#[test_parser(
    "impl A for c {
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
#[test_parser_error(
    "impla::b::c {
        fn f(x: int) int {
            x = x+1;
            return 0;
        }
    }"
)]
pub fn impl_def(input: Span) -> IResult<Span, Box<TopLevel>> {
    map_res(
        tuple((
            tag_token_word(TokenType::IMPL),
            opt(generic_type_def),
            opt(pair(type_name, tag_token_word(TokenType::FOR))),
            type_name,
            del_newline_or_space!(tag_token_symbol(TokenType::LBRACE)),
            many0(eat_any_err_block_before(
                function_def,
                function_def,
                "\n",
                crate::ast::diag::ErrorCode::SYNTAX_ERROR_TOP_STATEMENT,
                "failed to parse",
            )),
            many0(comment),
            eat_any_err_block_before(
                del_newline_or_space!(tag_token_symbol(TokenType::RBRACE)),
                del_newline_or_space!(tag_token_symbol(TokenType::RBRACE)),
                "}",
                crate::ast::diag::ErrorCode::SYNTAX_ERROR_TOP_STATEMENT,
                "failed to parse",
            ),
        )),
        |(_, generics, impl_trait, structure, (_, start), func_def, comment0, (_, end))| {
            res_box(Box::new(TopLevel::ImplDef(ImplNode {
                range: start.start.to(end.end),
                target: structure,
                generics,
                methods: func_def
                    .iter()
                    .map(|x| {
                        let d = (*x).clone();
                        match *d {
                            TopLevel::FuncType(x) => x.into(),
                            _ => unreachable!(),
                        }
                    })
                    .collect(),
                comments: vec![comment0],
                impl_trait,
            })))
        },
    )(input)
}
