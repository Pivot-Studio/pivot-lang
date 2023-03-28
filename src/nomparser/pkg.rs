use nom::{
    combinator::{map_res, opt},
    multi::separated_list1,
    sequence::{preceded, tuple},
    IResult,
};

use crate::nomparser::Span;
use crate::{ast::node::pkg::UseNode, ast::tokens::TokenType};
use internal_macro::{test_parser, test_parser_error};

use super::*;

/// ```enbf
/// use_statement = "use" identifier ("::" identifier)* ;
/// ```
#[test_parser("use a::b")]
#[test_parser("use a::")]
#[test_parser("use a")]
#[test_parser("use a:")]
#[test_parser_error("usea")]
#[test_parser_error("usea:")]
pub fn use_statement(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(
        preceded(
            tag_token_word(TokenType::USE),
            delspace(tuple((
                separated_list1(tag_token_symbol(TokenType::DOUBLE_COLON), identifier),
                opt(tag_token_symbol(TokenType::DOUBLE_COLON)),
                opt(tag_token_symbol(TokenType::COLON)),
            ))),
        ),
        |(ns, opt, opt2)| {
            let mut range = ns
                .first()
                .unwrap()
                .range()
                .start
                .to(ns.last().unwrap().range().end);
            if let Some(opt) = opt {
                range = range.start.to(opt.1.end);
            }
            if let Some(opt2) = opt2 {
                range = range.start.to(opt2.1.end);
            }
            res_enum(NodeEnum::UseNode(UseNode {
                ids: ns,
                range,
                complete: opt.is_none() && opt2.is_none(),
                singlecolon: opt2.is_some(),
            }))
        },
    )(input)
}
