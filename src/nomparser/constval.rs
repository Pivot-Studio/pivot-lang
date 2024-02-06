use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{one_of, space0},
    combinator::{map_res, opt, recognize},
    multi::{many0, many1},
    sequence::{preceded, terminated, tuple},
    IResult,
};

use crate::nomparser::Span;
use crate::{ast::range::Range, ast::tokens::TokenType};
use internal_macro::{test_parser, test_parser_error};
use nom::character::complete::char;

use super::*;
#[test_parser(".10")]
#[test_parser("10.10")]
#[test_parser("10")]
#[test_parser("10_00__3")]
#[test_parser("0X1a")]
#[test_parser("0o17")]
#[test_parser("0b101")]
#[test_parser_error("_10")]
#[test_parser_error("10.")]
#[test_parser_error("0x12g3")]
#[test_parser_error("0o12.3")]
#[test_parser_error("0b123")]
pub fn number(input: Span) -> IResult<Span, Box<NodeEnum>> {
    let (input, _) = space0(input)?;
    let (re, value) = alt((
        map_res(float, |out| {
            Ok::<Num, ()>(Num::Float(out.fragment().parse::<f64>().unwrap()))
        }),
        map_res(hexadecimal, |out| {
            let res =
                u64::from_str_radix(out.fragment().replace('_', "").get(2..).unwrap(), 16).unwrap();
            Ok::<Num, ()>(Num::Int(res))
        }),
        map_res(octal, |out| {
            let res =
                u64::from_str_radix(out.fragment().replace('_', "").get(2..).unwrap(), 8).unwrap();
            Ok::<Num, ()>(Num::Int(res))
        }),
        map_res(binary, |out| {
            let res =
                u64::from_str_radix(out.fragment().replace('_', "").get(2..).unwrap(), 2).unwrap();
            Ok::<Num, ()>(Num::Int(res))
        }),
        map_res(decimal, |out| {
            // TODO:err tolerate
            Ok::<Num, ()>(Num::Int(
                out.fragment().replace('_', "").parse::<u64>().unwrap(),
            ))
        }),
    ))(input)?;
    let range = Range::new(input, re);
    let node = NumNode { value, range };
    Ok((re, Box::new(node.into())))
}

#[test_parser(" true")]
#[test_parser("false")]
#[test_parser_error("tru")]
#[test_parser_error("fales")]
#[test_parser_error("TRUE")]
#[test_parser_error("FALSE")]
pub fn bool_const(input: Span) -> IResult<Span, Box<NodeEnum>> {
    alt((
        map_res(tag_token_word(TokenType::TRUE), |(_, range)| {
            res_enum(BoolConstNode { value: true, range }.into())
        }),
        map_res(tag_token_word(TokenType::FALSE), |(_, range)| {
            res_enum(
                BoolConstNode {
                    value: false,
                    range,
                }
                .into(),
            )
        }),
    ))(input)
}

#[test_parser("123")]
#[test_parser("12_3")]
#[test_parser("1_2_3")]
#[test_parser_error("1 23")]
#[test_parser_error("+123")]
#[test_parser_error("-123")]

// currently decimal handles [0-9_] only,
// the negative/postive mark will be treated as a unary operator
fn decimal(input: Span) -> IResult<Span, Span> {
    recognize(many1(terminated(one_of("0123456789"), many0(char('_')))))(input)
}

#[test_parser("0x10")]
#[test_parser("0X1a3")]
#[test_parser("0x1_2__3")]
#[test_parser_error("x123")]
#[test_parser_error("0x12g3")]
fn hexadecimal(input: Span) -> IResult<Span, Span> {
    recognize(preceded(
        alt((tag("0x"), tag("0X"))),
        many1(terminated(
            one_of("0123456789abcdefABCDEF"),
            many0(char('_')),
        )),
    ))(input)
}

#[test_parser("0o10")]
#[test_parser("0O173")]
#[test_parser("0o1_2__3")]
#[test_parser_error("o123")]
#[test_parser_error("0o1283")]
fn octal(input: Span) -> IResult<Span, Span> {
    recognize(preceded(
        alt((tag("0o"), tag("0O"))),
        many1(terminated(one_of("01234567"), many0(char('_')))),
    ))(input)
}

#[test_parser("0b101")]
#[test_parser("0B0_01_")]
#[test_parser_error("B001")]
#[test_parser_error("0b0201")]
fn binary(input: Span) -> IResult<Span, Span> {
    recognize(preceded(
        alt((tag("0b"), tag("0B"))),
        many1(terminated(one_of("01"), many0(char('_')))),
    ))(input)
}

#[test_parser(".12")]
#[test_parser(".1_2")]
#[test_parser(".12e1")]
#[test_parser(".12E1")]
#[test_parser("123e1")]
#[test_parser("123e-1")]
#[test_parser("123e+1")]
#[test_parser("12.3e1")]
#[test_parser("42.12")]
#[test_parser_error("42.")]
fn float(input: Span) -> IResult<Span, Span> {
    alt((
        // Case one: .42
        recognize(tuple((
            char('.'),
            decimal,
            opt(tuple((
                one_of("eE"),
                opt(alt((
                    tag_token_symbol(TokenType::PLUS),
                    tag_token_symbol(TokenType::MINUS),
                ))),
                decimal,
            ))),
        ))), // Case two: 42e42 and 42.42e42
        recognize(tuple((
            decimal,
            opt(preceded(char('.'), decimal)),
            one_of("eE"),
            opt(alt((
                tag_token_symbol(TokenType::PLUS),
                tag_token_symbol(TokenType::MINUS),
            ))),
            decimal,
        ))), // Case three: 42.42
        recognize(tuple((decimal, char('.'), decimal))),
    ))(input)
}
