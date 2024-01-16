use nom::{
    branch::alt,
    bytes::complete::is_not,
    combinator::*,
    error::{FromExternalError, ParseError},
    IResult,
};

use crate::ast::{
    node::{error::ErrorNode, NodeEnum},
    range::Range,
};

use super::{take_utf8_split, Span};

pub fn except<'a, E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::fmt::Error>>(
    except: &'static str,
    msg: &'static str,
    code: crate::ast::diag::ErrorCode,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Box<NodeEnum>, E> {
    move |i| {
        let (mut i, sp) = recognize(is_not(Span::from(except)))(i)?;
        let mut r = vec![];
        for c in sp.chars() {
            if c == '{' {
                r.push('}');
            }
            if c == '}' {
                r.pop();
            }
        }
        let mut src = sp.fragment().to_string();
        let mut next;
        while !r.is_empty() {
            if i.len() == 0 {
                break;
            }

            (i, next) = take_utf8_split(&i);
            let nextch = next.fragment().char_indices().next().unwrap().1;
            src.push(next.fragment().char_indices().next().unwrap().1);
            if nextch == '{' {
                r.push('}');
            }
            if nextch == '}' {
                r.pop();
            }
        }
        let msg = msg.to_string();
        let node = Box::new(
            ErrorNode {
                msg,
                src,
                range: Range::new(sp, i),
                code,
            }
            .into(),
        );
        Ok((i, node))
    }
}

pub fn alt_except<'a, E, F>(
    parser: F,
    ex: &'static str,
    msg: &'static str,
    code: crate::ast::diag::ErrorCode,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Box<NodeEnum>, E>
where
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::fmt::Error>,
    F: FnMut(Span<'a>) -> IResult<Span<'a>, Box<NodeEnum>, E>,
{
    alt((parser, except(ex, msg, code)))
}
