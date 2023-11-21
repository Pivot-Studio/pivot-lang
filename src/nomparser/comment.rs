use crate::nomparser::Span;
use crate::{ast::node::comment::CommentNode, ast::range::Range};
use internal_macro::{test_parser, test_parser_error};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    combinator::{map_res, rest},
    sequence::{pair, terminated},
    IResult, InputTake,
};
use nom_locate::LocatedSpan;

use super::*;

#[test_parser("//123")]
#[test_parser("//123\t")]
#[test_parser("//123 \u{1234}")]
#[test_parser("/// 123\n")]
#[test_parser("//12\r //3")]
#[test_parser("//12\r \\//3")]
#[test_parser_error("//12\r\n /3")]
#[test_parser("/// 123\r 456")]
#[test_parser_error("/// 12\n 3 ///")]
#[test_parser_error("/ / 123\n")]
#[test_parser_error("//123 \n \n ///")]
pub fn comment(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(
        pair(
            alt((tag("///"), tag("//"))),
            alt((terminated(take_until("\n"), tag("\n")), rest)),
        ),
        |(a, c): (LocatedSpan<&str, bool>, LocatedSpan<&str, bool>)| {
            res_enum(
                CommentNode {
                    comment: c.trim_end_matches('\r').to_string(),
                    range: Range::new(input, c.take_split(c.len()).0),
                    is_doc: a.contains("///"),
                }
                .into(),
            )
        },
    )(input)
}
