use std::fmt::Error;

use nom::{
    bytes::complete::tag, character::complete::space0, combinator::map_res, error::ParseError,
    sequence::delimited, AsChar, IResult, InputTake, InputTakeAtPosition, Parser,
};
use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;
use crate::{ast::range::Range, ast::tokens::TokenType};

use super::*;

pub fn tag_token(token: TokenType) -> impl Fn(Span) -> IResult<Span, (TokenType, Range)> {
    move |input| {
        map_res(delspace(tag(token.get_str())), |_out: Span| {
            let end = _out.take_split(token.get_str().len()).0;
            Ok::<(TokenType, Range), Error>((token, Range::new(_out, end)))
        })(input)
    }
}
pub fn delspace<I, O, E, G>(parser: G) -> impl FnMut(I) -> IResult<I, O, E>
where
    G: Parser<I, O, E>,
    E: ParseError<I>,
    I: InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    delimited(space0, parser, space0)
}

pub fn take_utf8_split<'a>(sp: &Span<'a>) -> (Span<'a>, Span<'a>) {
    let mut i = 1;
    let l = sp.len();
    if l == 0 {
        return sp.take_split(0);
    }
    while !sp.is_char_boundary(i) {
        i = i + 1;
    }
    sp.take_split(i)
}

pub fn res_enum(t: NodeEnum) -> Result<Box<NodeEnum>, Error> {
    res_box(Box::new(t))
}

pub fn res_box<T: ?Sized>(i: Box<T>) -> Result<Box<T>, Error> {
    Ok::<_, Error>(i)
}

pub fn create_bin(
    (mut left, rights): (Box<NodeEnum>, Vec<((TokenType, Range), Box<NodeEnum>)>),
) -> Result<Box<NodeEnum>, Error> {
    for ((op, _), right) in rights {
        let range = left.range().start.to(right.range().end);
        left = Box::new(
            BinOpNode {
                op,
                left,
                right,
                range,
            }
            .into(),
        );
    }
    res_box(left)
}
