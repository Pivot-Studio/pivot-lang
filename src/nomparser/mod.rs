use std::fmt::Error;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{multispace0, one_of, space0},
    combinator::{map_res, opt, recognize},
    multi::{many0, many1},
    number::{
        self,
        complete::{be_i64, double, i64},
    },
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};
use nom_locate::{position, LocatedSpan};
type Span<'a> = LocatedSpan<&'a str>;
use nom::character::complete::char;

use crate::{
    ast::{BinOpNode, Node, Num, NumNode, UnaryOpNode},
    lexer::{
        pos::{Pos, Range},
        types::Operator,
    },
};

pub struct Parser<'a> {
    input: Span<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let sp = Span::from(input);
        Parser { input: sp }
    }

    pub fn parse(&mut self) -> IResult<Span, Box<dyn Node>> {
        Self::add_exp(self.input)
    }

    pub fn number(input: Span) -> IResult<Span, Box<dyn Node>> {
        let (input, _) = space0(input)?;
        let (re, node) = alt((float, decimal))(input)?;
        let range = Range::new(input, re);
        let num = node.fragment().parse::<i64>();
        let value;
        if let Err(_) = num {
            value = Num::FLOAT(input.fragment().parse::<f64>().unwrap());
        } else {
            value = Num::INT(num.unwrap());
        }
        let node = NumNode { value, range };
        Ok((re, Box::new(node)))
    }

    pub fn add_exp(input: Span) -> IResult<Span, Box<dyn Node>> {
        delimited(
            space0,
            alt((
                map_res(
                    tuple((Self::mul_exp, alt((tag("+"), tag("-"))), Self::add_exp)),
                    |(left, op, right)| {
                        let range = left.range().start.to(right.range().end);
                        Ok::<Box<dyn Node>, Error>(Box::new(BinOpNode {
                            op: match op.fragment() {
                                &"+" => Operator::PLUS,
                                &"-" => Operator::MINUS,
                                _ => panic!("unreachable"),
                            },
                            left,
                            right,
                            range,
                        }) as Box<dyn Node>)
                    },
                ),
                Self::mul_exp,
            )),
            space0,
        )(input)
    }

    pub fn mul_exp(input: Span) -> IResult<Span, Box<dyn Node>> {
        delimited(
            space0,
            alt((
                map_res(
                    tuple((Self::unary_exp, alt((tag("*"), tag("/"))), Self::mul_exp)),
                    |(left, op, right)| {
                        let range = left.range().start.to(right.range().end);
                        Ok::<Box<dyn Node>, Error>(Box::new(BinOpNode {
                            op: match op.fragment() {
                                &"*" => Operator::MUL,
                                &"/" => Operator::DIV,
                                _ => panic!("unreachable"),
                            },
                            left,
                            right,
                            range,
                        }) as Box<dyn Node>)
                    },
                ),
                Self::unary_exp,
            )),
            space0,
        )(input)
    }

    pub fn unary_exp(input: Span) -> IResult<Span, Box<dyn Node>> {
        delimited(
            space0,
            alt((
                Self::primary_exp,
                map_res(preceded(tag("-"), Self::primary_exp), |out| {
                    let range = out.range();
                    Ok::<Box<dyn Node>, Error>(Box::new(UnaryOpNode {
                        op: Operator::MINUS,
                        exp: out,
                        range,
                    }) as Box<dyn Node>)
                }),
            )),
            space0,
        )(input)
    }

    pub fn primary_exp(input: Span) -> IResult<Span, Box<dyn Node>> {
        delimited(
            space0,
            alt((Self::number, delimited(tag("("), Self::add_exp, tag(")")))),
            space0,
        )(input)
    }
}

fn float(input: Span) -> IResult<Span, Span> {
    alt((
        // Case one: .42
        recognize(tuple((
            char('.'),
            decimal,
            opt(tuple((one_of("eE"), opt(one_of("+-")), decimal))),
        ))), // Case two: 42e42 and 42.42e42
        recognize(tuple((
            decimal,
            opt(preceded(char('.'), decimal)),
            one_of("eE"),
            opt(one_of("+-")),
            decimal,
        ))), // Case three: 42. and 42.42
        recognize(tuple((decimal, char('.'), opt(decimal)))),
    ))(input)
}

fn decimal(input: Span) -> IResult<Span, Span> {
    recognize(many1(terminated(one_of("0123456789"), many0(char('_')))))(input)
}
