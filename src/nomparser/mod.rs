use std::fmt::Error;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{one_of, space0},
    combinator::{map_res, opt, recognize},
    multi::{many0, many1},
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};
use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;
use nom::character::complete::char;

use crate::{
    ast::{BinOpNode, Node, Num, NumNode, UnaryOpNode},
    ast::{Range, TokenType},
};

fn res<T>(t: T) -> Result<Box<dyn Node>, Error>
where
    T: Node + 'static,
{
    Ok::<Box<dyn Node>, Error>(Box::new(t))
}
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
        let (re, value) = alt((
            map_res(Self::float, |out| {
                Ok::<Num, Error>(Num::FLOAT(out.fragment().parse::<f64>().unwrap()))
            }),
            map_res(Self::decimal, |out| {
                Ok::<Num, Error>(Num::INT(out.fragment().parse::<i64>().unwrap()))
            }),
        ))(input)?;
        let range = Range::new(input, re);
        let node = NumNode { value, range };
        Ok((re, Box::new(node)))
    }

    pub fn add_exp(input: Span) -> IResult<Span, Box<dyn Node>> {
        delimited(
            space0,
            alt((
                map_res(
                    tuple((
                        Self::mul_exp,
                        alt((
                            Self::tag_token(TokenType::PLUS),
                            Self::tag_token(TokenType::MINUS),
                        )),
                        Self::add_exp,
                    )),
                    |(left, op, right)| {
                        let range = left.range().start.to(right.range().end);
                        res(BinOpNode {
                            op,
                            left,
                            right,
                            range,
                        })
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
                    tuple((
                        Self::unary_exp,
                        alt((
                            Self::tag_token(TokenType::MUL),
                            Self::tag_token(TokenType::DIV),
                        )),
                        Self::mul_exp,
                    )),
                    |(left, op, right)| {
                        let range = left.range().start.to(right.range().end);
                        res(BinOpNode {
                            op,
                            left,
                            right,
                            range,
                        })
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
                map_res(
                    preceded(Self::tag_token(TokenType::MINUS), Self::primary_exp),
                    |out| {
                        let range = out.range();
                        res(UnaryOpNode {
                            op: TokenType::MINUS,
                            exp: out,
                            range,
                        })
                    },
                ),
            )),
            space0,
        )(input)
    }

    pub fn primary_exp(input: Span) -> IResult<Span, Box<dyn Node>> {
        delimited(
            space0,
            alt((
                Self::number,
                delimited(
                    Self::tag_token(TokenType::LPAREN),
                    Self::add_exp,
                    Self::tag_token(TokenType::RPAREN),
                ),
            )),
            space0,
        )(input)
    }
    fn float(input: Span) -> IResult<Span, Span> {
        alt((
            // Case one: .42
            recognize(tuple((
                char('.'),
                Self::decimal,
                opt(tuple((
                    one_of("eE"),
                    opt(alt((
                        Self::tag_token(TokenType::PLUS),
                        Self::tag_token(TokenType::MINUS),
                    ))),
                    Self::decimal,
                ))),
            ))), // Case two: 42e42 and 42.42e42
            recognize(tuple((
                Self::decimal,
                opt(preceded(char('.'), Self::decimal)),
                one_of("eE"),
                opt(alt((
                    Self::tag_token(TokenType::PLUS),
                    Self::tag_token(TokenType::MINUS),
                ))),
                Self::decimal,
            ))), // Case three: 42. and 42.42
            recognize(tuple((Self::decimal, char('.'), opt(Self::decimal)))),
        ))(input)
    }
    fn decimal(input: Span) -> IResult<Span, Span> {
        recognize(many1(terminated(one_of("0123456789"), many0(char('_')))))(input)
    }
    fn tag_token(token: TokenType) -> impl Fn(Span) -> IResult<Span, TokenType> {
        move |input| map_res(tag(token.get_str()), |_out| Ok::<TokenType, Error>(token))(input)
    }
}
