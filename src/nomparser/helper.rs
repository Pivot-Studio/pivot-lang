use crate::nomparser::Span;
use crate::{ast::range::Range, ast::tokens::TokenType};
use nom::character::complete::space1;
use nom::character::is_alphanumeric;
use nom::combinator::opt;
use nom::sequence::{pair, preceded, terminated};
use nom::{
    bytes::complete::tag, character::complete::space0, combinator::map_res, error::ParseError,
    sequence::delimited, AsChar, IResult, InputTake, InputTakeAtPosition, Parser,
};

use super::*;

/// parse token with space trimed
pub fn tag_token_symbol(token: TokenType) -> impl Fn(Span) -> IResult<Span, (TokenType, Range)> {
    move |input| {
        map_res(delspace(tag(token.get_str())), |_out: Span| {
            let end = _out.take_split(token.get_str().len()).0;
            Ok::<(TokenType, Range), ()>((token, Range::new(_out, end)))
        })(input)
    }
}

/// parse raw token (no space trimed)
pub fn tag_token(token: TokenType) -> impl Fn(Span) -> IResult<Span, (TokenType, Range)> {
    move |input| {
        map_res(tag(token.get_str()), |_out: Span| {
            let end = _out.take_split(token.get_str().len()).0;
            Ok::<(TokenType, Range), ()>((token, Range::new(_out, end)))
        })(input)
    }
}

/// parse modifier, basically same as `tag_token`, but ensure there is at least one space after it
pub fn tag_modifier(token: TokenType) -> impl Fn(Span) -> IResult<Span, (TokenType, Range)> {
    move |input| {
        map_res(terminated(tag(token.get_str()), space1), |_out: Span| {
            let end = _out.take_split(token.get_str().len()).0;
            Ok::<(TokenType, Range), ()>((token, Range::new(_out, end)))
        })(input)
    }
}

pub fn modifiable<'a, O, G>(
    parser: G,
    token: TokenType,
) -> impl FnMut(
    Span<'a>,
) -> Result<
    (Span<'a>, (Option<(TokenType, Range)>, O)),
    nom::Err<nom::error::Error<Span<'a>>>,
>
where
    G: Parser<Span<'a>, O, nom::error::Error<Span<'a>>>,
{
    pair(opt(tag_modifier(token)), parser)
}

/// 不能直接接 `字母`、`数字` 或 `_`，用于关键字
pub fn tag_token_word(token: TokenType) -> impl Fn(Span) -> IResult<Span, (TokenType, Range)> {
    move |input| {
        let (s1, s2): (LocatedSpan<&str, bool>, LocatedSpan<&str, bool>) =
            preceded(space0, tag(token.get_str()))(input)?;
        if s1.starts_with(|c: char| is_alphanumeric(c as u8) || c == '_') {
            Err(nom::Err::Error(nom::error::Error::new(
                s2,
                nom::error::ErrorKind::Tag,
            )))
        } else {
            let end = s2.take_split(token.get_str().len()).0;
            Ok((s1, (token, Range::new(s2, end))))
        }
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
pub fn parse_with_ex<'a, O, E, G>(
    mut parser: G,
    extra: bool,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, O, E>
where
    G: Parser<Span<'a>, O, E>,
    E: ParseError<Span<'a>>,
{
    move |i| {
        let mut newi = i;
        let prevex = newi.extra;
        newi.extra = extra;
        let (mut newi, re) = parser.parse(newi)?;
        newi.extra = prevex;
        Ok((newi, re))
    }
}

pub fn take_utf8_split<'a>(sp: &Span<'a>) -> (Span<'a>, Span<'a>) {
    let mut i = 1;
    let l = sp.len();
    if l == 0 {
        return sp.take_split(0);
    }
    while !sp.is_char_boundary(i) {
        i += 1;
    }
    sp.take_split(i)
}

pub fn res_enum(t: NodeEnum) -> Result<Box<NodeEnum>, ()> {
    res_box(Box::new(t))
}

pub fn res_box<T: ?Sized>(i: Box<T>) -> Result<Box<T>, ()> {
    Ok::<_, ()>(i)
}

pub fn create_bin(
    (mut left, rights): (Box<NodeEnum>, Vec<((TokenType, Range), Box<NodeEnum>)>),
) -> Result<Box<NodeEnum>, ()> {
    for ((op, orange), right) in rights {
        let range = left.range().start.to(right.range().end);
        left = Box::new(
            BinOpNode {
                op: (op, orange),
                left,
                right,
                range,
            }
            .into(),
        );
    }
    res_box(left)
}
