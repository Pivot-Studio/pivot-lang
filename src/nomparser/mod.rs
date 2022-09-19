use nom::{
    branch::alt,
    character::complete::{one_of, space0},
    combinator::{map_res, opt, recognize},
    multi::{many0, many1},
    number::{
        self,
        complete::{be_i64, double, i64},
    },
    sequence::{preceded, terminated, tuple},
    IResult,
};
use nom_locate::{position, LocatedSpan};
type Span<'a> = LocatedSpan<&'a str>;
use nom::character::complete::char;

use crate::{
    ast::{Node, Num, NumNode},
    lexer::pos::{Pos, Range},
};

pub struct Parser<'a> {
    input: Span<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let sp = Span::from(input);
        Parser { input: sp }
    }

    pub fn number(input: Span) -> IResult<Span, Box<dyn Node>> {
        space0(input)?;
        let (re, node) = alt((float, decimal))(input)?;
        let range = Range::new(input, re);
        let num = node.fragment().parse::<f64>();
        let value;
        if let Err(_) = num {
            value = Num::INT(input.fragment().parse::<i64>().unwrap());
        } else {
            value = Num::FLOAT(num.unwrap());
        }
        let node = NumNode { value, range };
        Ok((re, Box::new(node)))
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
