use std::fmt::Error;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, one_of, space0},
    combinator::{map_res, opt, recognize},
    error::ParseError,
    multi::{many0, many0_count, many1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    AsChar, IResult, InputTakeAtPosition, Parser,
};
use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;
use nom::character::complete::char;

use crate::{
    ast::node::{
        AssignNode, BinOpNode, BoolConstNode, DefNode, Node, Num, NumNode, StatementsNode,
        UnaryOpNode, VarNode,
    },
    ast::tokens::TokenType,
    ast::{
        node::{NLNode, WhileNode},
        range::Range,
    },
};

fn res<T>(t: T) -> Result<Box<dyn Node>, Error>
where
    T: Node + 'static,
{
    res_box(box_node(t))
}

fn box_node<T>(t: T) -> Box<dyn Node>
where
    T: Node + 'static,
{
    Box::new(t)
}

fn res_box(i: Box<dyn Node>) -> Result<Box<dyn Node>, Error> {
    Ok::<Box<dyn Node>, Error>(i)
}

fn create_bin(
    (mut left, rights): (Box<dyn Node>, Vec<(TokenType, Box<dyn Node>)>),
) -> Result<Box<dyn Node>, Error> {
    for (op, right) in rights {
        let range = left.range().start.to(right.range().end);
        left = Box::new(BinOpNode {
            op,
            left,
            right,
            range,
        });
    }
    res_box(left)
}

macro_rules! parse_bin_ops {
    ($exp:ident, $($op:ident),*) => {
        delspace(map_res(
            tuple((
                Self::$exp,
                many0(tuple((
                    alt((
                        $(
                            Self::tag_token(TokenType::$op),
                        )*
                    )),
                    Self::$exp,
                ))),
            )),
            create_bin,
        ))
    };
}

pub struct PLParser<'a> {
    input: Span<'a>,
}

impl<'a> PLParser<'a> {
    pub fn new(input: &'a str) -> Self {
        let sp = Span::from(input);
        PLParser { input: sp }
    }

    pub fn parse(&mut self) -> IResult<Span, Box<dyn Node>> {
        Self::statements(self.input)
    }

    /// ```enbf
    /// whilestatement = "while" logicexp statement_block ;
    /// ```
    pub fn while_statement(input: Span) -> IResult<Span, Box<dyn Node>> {
        map_res(
            tuple((
                Self::tag_token(TokenType::WHILE),
                Self::logic_exp,
                Self::statement_block,
            )),
            |(_, cond, body)| {
                let range = cond.range().start.to(body.range().end);
                res(WhileNode { cond, body, range })
            },
        )(input)
    }

    pub fn statement_block(input: Span) -> IResult<Span, Box<dyn Node>> {
        delimited(
            Self::tag_token(TokenType::LBRACE),
            Self::statements,
            Self::tag_token(TokenType::RBRACE),
        )(input)
    }

    /// ```ebnf
    /// statement =
    /// | assignment newline
    /// | newvariable newline
    /// | ifstatement
    /// | whilestatement
    /// | newline
    /// ;
    /// ```
    pub fn statement(input: Span) -> IResult<Span, Box<dyn Node>> {
        alt((
            terminated(Self::new_variable, Self::newline),
            terminated(Self::assignment, Self::newline),
            // Self::if_statement,
            Self::while_statement,
            // Self::eof,
            Self::newline,
        ))(input)
    }

    pub fn newline(input: Span) -> IResult<Span, Box<dyn Node>> {
        map_res(alt((tag("\n"), tag("\r\n"))), |_| {
            res(NLNode {
                range: Range::new(input, input),
            })
        })(input)
    }

    pub fn statements(input: Span) -> IResult<Span, Box<dyn Node>> {
        map_res(many0(Self::statement), |v| {
            let mut range = v[0].range();
            let la = v.last();
            if let Some(la) = la {
                range = range.start.to(la.range().end);
            }
            res(StatementsNode {
                statements: v,
                range,
            })
        })(input)
    }

    pub fn new_variable(input: Span) -> IResult<Span, Box<dyn Node>> {
        delspace(map_res(
            preceded(
                Self::tag_token(TokenType::LET),
                tuple((
                    Self::identifier,
                    Self::tag_token(TokenType::ASSIGN),
                    Self::logic_exp,
                )),
            ),
            |(out, _, v)| {
                let a = out.as_any().downcast_ref::<VarNode>().unwrap().clone();
                let range = out.range().start.to(v.range().end);
                res(DefNode {
                    var: a,
                    exp: v,
                    range,
                })
            },
        ))(input)
    }

    pub fn assignment(input: Span) -> IResult<Span, Box<dyn Node>> {
        delspace(map_res(
            tuple((
                Self::identifier,
                Self::tag_token(TokenType::ASSIGN),
                Self::logic_exp,
            )),
            |(left, _op, right)| {
                let range = left.range().start.to(right.range().end);
                res(AssignNode {
                    var: left.as_any().downcast_ref::<VarNode>().unwrap().clone(),
                    exp: right,
                    range,
                })
            },
        ))(input)
    }

    pub fn number(input: Span) -> IResult<Span, Box<dyn Node>> {
        let (input, _) = space0(input)?;
        let (re, value) = alt((
            map_res(Self::float, |out| {
                Ok::<Num, Error>(Num::FLOAT(out.fragment().parse::<f64>().unwrap()))
            }),
            map_res(Self::decimal, |out| {
                // TODO:err tolerate
                Ok::<Num, Error>(Num::INT(out.fragment().parse::<u64>().unwrap()))
            }),
        ))(input)?;
        let range = Range::new(input, re);
        let node = NumNode { value, range };
        Ok((re, Box::new(node)))
    }

    pub fn add_exp(input: Span) -> IResult<Span, Box<dyn Node>> {
        parse_bin_ops!(mul_exp, PLUS, MINUS)(input)
    }

    pub fn mul_exp(input: Span) -> IResult<Span, Box<dyn Node>> {
        parse_bin_ops!(unary_exp, MUL, DIV)(input)
    }

    pub fn unary_exp(input: Span) -> IResult<Span, Box<dyn Node>> {
        delspace(alt((
            Self::primary_exp,
            map_res(
                tuple((
                    alt((
                        Self::tag_token(TokenType::MINUS),
                        Self::tag_token(TokenType::NOT),
                    )),
                    Self::primary_exp,
                )),
                |(op, exp)| {
                    let range = exp.range();
                    res(UnaryOpNode { op, exp, range })
                },
            ),
        )))(input)
    }
    pub fn compare_exp(input: Span) -> IResult<Span, Box<dyn Node>> {
        parse_bin_ops!(add_exp, GEQ, LEQ, NE, EQ, LESS, GREATER)(input)
    }
    pub fn logic_exp(input: Span) -> IResult<Span, Box<dyn Node>> {
        parse_bin_ops!(compare_exp, AND, OR)(input)
    }
    pub fn identifier(input: Span) -> IResult<Span, Box<dyn Node>> {
        delspace(map_res(
            recognize(pair(
                alt((alpha1::<Span, nom::error::Error<Span>>, tag("_"))),
                many0_count(alt((alphanumeric1, tag("_")))),
            )),
            |out| {
                res(VarNode {
                    name: out.to_string(),
                    range: Range::new(input, out),
                })
            },
        ))(input)
    }
    pub fn primary_exp(input: Span) -> IResult<Span, Box<dyn Node>> {
        delspace(alt((
            Self::number,
            Self::bool_const,
            delimited(
                Self::tag_token(TokenType::LPAREN),
                Self::logic_exp,
                Self::tag_token(TokenType::RPAREN),
            ),
            Self::identifier,
        )))(input)
    }
    fn bool_const(input: Span) -> IResult<Span, Box<dyn Node>> {
        alt((
            map_res(tag("true"), |out| {
                res(BoolConstNode {
                    value: true,
                    range: Range::new(input, out),
                })
            }),
            map_res(tag("false"), |out| {
                res(BoolConstNode {
                    value: false,
                    range: Range::new(input, out),
                })
            }),
        ))(input)
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

fn delspace<I, O, E, G>(parser: G) -> impl FnMut(I) -> IResult<I, O, E>
where
    G: Parser<I, O, E>,
    E: ParseError<I>,
    I: InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    delimited(space0, parser, space0)
}
