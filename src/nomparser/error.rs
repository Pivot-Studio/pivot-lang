use nom::{
    branch::alt,
    bytes::complete::is_not,
    combinator::*,
    error::{FromExternalError, ParseError},
    multi::{separated_list0, separated_list1},
    sequence::{pair, preceded, terminated},
    IResult,
};

use crate::ast::{
    node::{error::ErrorNode, NodeEnum},
    range::Range,
    tokens::TokenType,
};

use super::{tag_token_symbol_ex, take_utf8_split, Span};

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
                range: Range::new(&sp, &i),
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

/// match all token with paired `(` or `{` or `[` until find given token out of the paired token
///
/// will report an error
pub fn match_paired_until<
    'a,
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::fmt::Error>,
>(
    token: &'static str,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, (), E> {
    move |i| {
        let origin = i.clone();
        let mut i = i;
        let mut r = vec![];
        let mut src = String::new();
        loop {
            if i.len() == 0 {
                break;
            }
            if i.fragment().starts_with(token) && r.is_empty() {
                break;
            }
            if (i.fragment().starts_with(']')
                || i.fragment().starts_with('}') || i.fragment().starts_with(')')) && r.is_empty() && origin.location_offset() == i.location_offset() {
                return Ok((i, ()));
            }
            let (next, nextch) = take_utf8_split(&i);
            src.push_str(next.fragment());
            let nextch = nextch.fragment().char_indices().next().unwrap().1;
            if nextch == '{' {
                r.push('}');
            }
            if nextch == '}' {
                r.pop();
            }
            if nextch == '(' {
                r.push(')');
            }
            if nextch == ')' {
                r.pop();
            }
            if nextch == '[' {
                r.push(']');
            }
            if nextch == ']' {
                r.pop();
            }
            i = next;
        }
        let msg = format!(
            "syntax error: unexpected token: `{}`",
            origin
                .split_at(i.location_offset() - origin.location_offset())
                .0
        );
        i.extra.raise_error(ErrorNode {
            msg,
            src,
            range: Range::new(&origin, &i),
            code: crate::ast::diag::ErrorCode::UNEXPECTED_TOKEN,
        });
        Ok((i, ()))
    }
}

/// First try the parser, if failed, eat the error token and continue
///
/// this parser is wapped around match_paired_until
pub fn eat_if_error_and_continue<
    'a,
    R,
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::fmt::Error>,
>(
    parser: impl FnMut(Span<'a>) -> IResult<Span<'a>, R, E>,
    parser2: impl FnMut(Span<'a>) -> IResult<Span<'a>, R, E>,
    token: TokenType,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, R, E> {
    alt((
        parser,
        preceded(
            pair(
                match_paired_until(token.get_str()),
                tag_token_symbol_ex(token),
            ),
            parser2,
        ),
    ))
}

pub fn err_tolerable_seplist0<
    'a,
    R,
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::fmt::Error>,
    G,
>(
    parser: G,
    parser2: G,
    token: TokenType,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Vec<R>, E>
where
    G: FnMut(Span<'a>) -> IResult<Span<'a>, R, E>,
{
    terminated(
        separated_list0(
            tag_token_symbol_ex(token),
            eat_if_error_and_continue(parser, parser2, token),
        ),
        opt(map(recognize(tag_token_symbol_ex(token)), |r| {
            let mut range = Range::new(&r, &r);
            range.end.column += 1;
            range.end.offset += 1;
            r.extra.report_error(
                range
                    .new_err(crate::ast::diag::ErrorCode::SYNTAX_ERROR_REDUNDENT_SYMBOL)
                    .add_help("remove this redundant symbol")
                    .clone(),
            )
        })),
    )
}

#[allow(dead_code)]
pub fn err_tolerable_seplist1<
    'a,
    R,
    E: ParseError<Span<'a>> + FromExternalError<Span<'a>, std::fmt::Error>,
    G,
>(
    parser: G,
    parser2: G,
    token: TokenType,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Vec<R>, E>
where
    G: FnMut(Span<'a>) -> IResult<Span<'a>, R, E>,
{
    terminated(
        separated_list1(
            tag_token_symbol_ex(token),
            eat_if_error_and_continue(parser, parser2, token),
        ),
        opt(map(recognize(tag_token_symbol_ex(token)), |r| {
            let mut range = Range::new(&r, &r);
            range.end.column += 1;
            range.end.offset += 1;
            r.extra.report_error(
                range
                    .new_err(crate::ast::diag::ErrorCode::SYNTAX_ERROR_REDUNDENT_SYMBOL)
                    .add_help("remove this redundant symbol")
                    .clone(),
            )
        })),
    )
}
