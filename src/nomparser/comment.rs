use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    combinator::map_res,
    sequence::delimited,
    IResult, InputTake,
};
use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;
use crate::{ast::node::comment::CommentNode, ast::range::Range};

use super::*;

pub fn comment(input: Span) -> IResult<Span, Box<NodeEnum>> {
    alt((
        map_res(
            delimited(tag("///"), take_until("\n"), tag("\n")),
            |c: LocatedSpan<&str>| {
                res_enum(
                    CommentNode {
                        comment: c.to_string(),
                        range: Range::new(input, c.take_split(c.len()).0),
                        is_doc: true,
                    }
                    .into(),
                )
            },
        ),
        map_res(
            delimited(tag("//"), take_until("\n"), tag("\n")),
            |c: LocatedSpan<&str>| {
                res_enum(
                    CommentNode {
                        comment: c.to_string(),
                        range: Range::new(input, c.take_split(c.len()).0),
                        is_doc: false,
                    }
                    .into(),
                )
            },
        ),
    ))(input)
}
