use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    combinator::{map_res, rest},
    sequence::{pair, terminated},
    IResult, InputTake,
};
use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;
use crate::{ast::node::comment::CommentNode, ast::range::Range};

use super::*;

pub fn comment(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(
        pair(
            alt((tag("///"), tag("//"))),
            alt((terminated(take_until("\n"), tag("\n")), rest)),
        ),
        |(a, c): (LocatedSpan<&str>, LocatedSpan<&str>)| {
            res_enum(
                CommentNode {
                    comment: c.to_string(),
                    range: Range::new(input, c.take_split(c.len()).0),
                    is_doc: a.contains("///"),
                }
                .into(),
            )
        },
    )(input)
}
