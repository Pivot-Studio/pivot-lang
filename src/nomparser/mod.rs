use std::fmt::Error;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, one_of, space0},
    combinator::{eof, map_res, opt, recognize},
    error::ParseError,
    multi::{many0, many0_count, many1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    AsChar, IResult, InputTake, InputTakeAtPosition, Parser,
};
use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;
use crate::{
    ast::node::ret::RetNode,
    ast::node::*,
    ast::node::{
        function::{FuncCallNode, FuncDefNode, FuncTypeNode},
        types::{StructDefNode, TypeNameNode, TypedIdentifierNode},
    },
    ast::{node::types::StructInitNode, range::Range},
    ast::{
        node::{
            control::*, operator::*, primary::*, program::*, statement::*,
            types::StructInitFieldNode,
        },
        tokens::TokenType,
    },
};
use internal_macro::{test_parser, test_parser_error};
use nom::character::complete::char;

macro_rules! newline_or_eof {
    () => {
        alt((delspace(alt((tag("\n"), tag("\r\n")))), eof))
    };
}

macro_rules! del_newline_or_space {
    ($e:expr) => {
        delimited(
            many0(alt((tag("\n"), tag("\r\n"), tag(" "), tag("\t")))),
            $e,
            many0(alt((tag("\n"), tag("\r\n"), tag(" "), tag("\t")))),
        )
    };
}
macro_rules! delnl {
    ($parser:expr) => {
        delimited(many0(newline), $parser, many0(newline))
    };
}

fn res<T>(t: T) -> Result<Box<dyn Node>, Error>
where
    T: Node + 'static,
{
    res_box(box_node(t))
}

fn res_ori<T>(t: T) -> Result<Box<T>, Error>
where
    T: Node + 'static,
{
    res_box(box_node(t))
}

fn box_node<T>(t: T) -> Box<T>
where
    T: Node + 'static,
{
    Box::new(t)
}

fn res_box<T: ?Sized>(i: Box<T>) -> Result<Box<T>, Error> {
    Ok::<_, Error>(i)
}

fn create_bin(
    (mut left, rights): (Box<dyn Node>, Vec<((TokenType, Range), Box<dyn Node>)>),
) -> Result<Box<dyn Node>, Error> {
    for ((op, _), right) in rights {
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
                $exp,
                many0(tuple((
                    alt((
                        $(
                            tag_token(TokenType::$op),
                        )*
                    )),
                    $exp,
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
        program(self.input)
    }
}
#[test_parser("return 1\n")]
#[test_parser("return 1.1\n")]
#[test_parser("return true\n")]
#[test_parser("return\n")]
#[test_parser("return a\n")]
#[test_parser("return 1 + 2\n")]
#[test_parser_error("return a = 2\n")]
// ```
// return_statement = "return" logic_exp newline ;
// ```
pub fn return_statement(input: Span) -> IResult<Span, Box<dyn Node>> {
    delspace(map_res(
        tuple((
            tag_token(TokenType::RETURN),
            opt(logic_exp),
            newline_or_eof!(),
        )),
        |((_, range), val, _)| {
            if let Some(val) = val {
                let range = val.range();
                res(RetNode {
                    value: Some(val),
                    range,
                })
            } else {
                res(RetNode { value: None, range })
            }
        },
    ))(input)
}

#[test_parser(
    "if a > 1 { 
    a = 1
} else {
    a = 2
}"
)]
#[test_parser(
    "if true {
    a = 1
} else if false {
    a = 2
} else {
    a = 3
}"
)]
#[test_parser_error(
    "if true {
    a = 1
} else if false {
    a = 2
} else {
    a = 3
} else {
    a = 4
}"
)]
/// ```ebnf
/// if_statement = "if" logic_exp statement_block ("else" (if_statement | statement_block))? ;
/// ```
pub fn if_statement(input: Span) -> IResult<Span, Box<dyn Node>> {
    map_res(
        delspace(tuple((
            tag_token(TokenType::IF),
            logic_exp,
            statement_block,
            opt(preceded(
                tag_token(TokenType::ELSE),
                alt((if_statement, map_res(statement_block, |n| res(n)))),
            )),
        ))),
        |(_, cond, then, els)| {
            let mut range = cond.range().start.to(then.range.end);
            if let Some(el) = &els {
                range = range.start.to(el.range().end);
            }
            res(IfNode {
                cond,
                then: Box::new(then),
                els,
                range,
            })
        },
    )(input)
}
#[test_parser(
    "while true {
    let a = b
}"
)]
#[test_parser_error(
    "while true {
    let a = b
}
"
)]
/// ```ebnf
/// while_statement = "while" logic_exp statement_block ;
/// ```
pub fn while_statement(input: Span) -> IResult<Span, Box<dyn Node>> {
    map_res(
        delspace(tuple((
            tag_token(TokenType::WHILE),
            logic_exp,
            statement_block,
        ))),
        |(_, cond, body)| {
            let range = cond.range().start.to(body.range.end);
            res(WhileNode {
                cond,
                body: Box::new(body),
                range,
            })
        },
    )(input)
}

#[test_parser(
    " for ;true; {
    let a = b
}"
)]
#[test_parser(
    "for;true; {
    let a = b
}"
)]
#[test_parser(
    "for i = 0; i < 3; i = i + 1 {
    b = c + i
}"
)]
#[test_parser(
    "for i = 1; i <= 5; i = i + 1{
    b = b + 1
}"
)]
#[test_parser(
    "for let i = 0; i < 5; i = i + 1{
                
    }"
)]

/// ```enbf
/// for_statement = "for" (assignment | new_variable) ";" logic_exp ";" assignment statement_block;
/// ```
pub fn for_statement(input: Span) -> IResult<Span, Box<dyn Node>> {
    map_res(
        delspace(tuple((
            tag_token(TokenType::FOR),
            opt(alt((assignment, new_variable))),
            delspace(tag_token(TokenType::SEMI)),
            logic_exp,
            delspace(tag_token(TokenType::SEMI)),
            opt(assignment),
            statement_block,
        ))),
        |(_, pre, _, cond, _, opt, body)| {
            let mut range = cond.range().start.to(body.range.end);
            if let Some(pre) = &pre {
                range = range.end.from(pre.range().start);
            }
            res(ForNode {
                pre,
                cond,
                opt,
                body: Box::new(body),
                range,
            })
        },
    )(input)
}

pub fn statement_block(input: Span) -> IResult<Span, StatementsNode> {
    delspace(delimited(
        tag_token(TokenType::LBRACE),
        statements,
        tag_token(TokenType::RBRACE),
    ))(input)
}

/// ```ebnf
/// statement =
/// | new_variable newline
/// | assignment newline
/// | if_statement
/// | while_statement
/// | for_statement
/// | break_statement
/// | continue_statement
/// | return_statement
/// | newline
/// ;
/// ```
pub fn statement(input: Span) -> IResult<Span, Box<dyn Node>> {
    delspace(alt((
        terminated(new_variable, newline_or_eof!()),
        terminated(assignment, newline_or_eof!()),
        if_statement,
        while_statement,
        for_statement,
        break_statement,
        continue_statement,
        terminated(function_call, newline_or_eof!()),
        return_statement,
        newline,
    )))(input)
}

enum TopLevel {
    StructDef(StructDefNode),
    FuncDef(FuncDefNode),
}

fn top_level_statement(input: Span) -> IResult<Span, Box<TopLevel>> {
    delspace(alt((
        del_newline_or_space!(function_def),
        del_newline_or_space!(struct_def),
    )))(input)
}

pub fn newline(input: Span) -> IResult<Span, Box<dyn Node>> {
    map_res(delspace(alt((tag("\n"), tag("\r\n")))), |_| {
        res(NLNode {
            range: Range::new(input, input),
        })
    })(input)
}

pub fn statements(input: Span) -> IResult<Span, StatementsNode> {
    map_res(many0(statement), |v| {
        let mut range = v[0].range();
        let la = v.last();
        if let Some(la) = la {
            range = range.start.to(la.range().end);
        }
        Ok::<_, Error>(StatementsNode {
            statements: v,
            range,
        })
    })(input)
}

pub fn program(input: Span) -> IResult<Span, Box<dyn Node>> {
    // map_res(terminated(many0(top_level_statement), eof), |v| {
    //     let mut range = v[0].range();
    //     let la = v.last();
    //     if let Some(la) = la {
    //         range = range.start.to(la.range().end);
    //     }
    //     res(ProgramNode {
    //         statements: v,
    //         range,
    //     })
    // })(input)
    let old = input;
    let mut input = input;
    let mut fns = vec![];
    let mut sts = vec![];
    let mut fntypes = vec![];
    loop {
        let top = top_level_statement(input);
        if let Ok((i, t)) = top {
            match *t {
                TopLevel::FuncDef(f) => {
                    fntypes.push(f.typenode.clone());
                    fns.push(f);
                }
                TopLevel::StructDef(s) => {
                    sts.push(s);
                }
            }
            input = i;
        } else {
            eof(input)?;
            break;
        }
    }
    let node: Box<dyn Node> = Box::new(ProgramNode {
        fns,
        structs: sts,
        fntypes,
        range: Range::new(old, input),
    });
    Ok((input, node))
}

fn cast_to_var(n: &Box<dyn Node>) -> VarNode {
    n.as_any().downcast_ref::<VarNode>().unwrap().clone()
}

#[test_parser("let a = 1")]
pub fn new_variable(input: Span) -> IResult<Span, Box<dyn Node>> {
    delspace(map_res(
        preceded(
            tag_token(TokenType::LET),
            tuple((identifier, tag_token(TokenType::ASSIGN), logic_exp)),
        ),
        |(out, _, v)| {
            let a = cast_to_var(&out);
            let range = out.range().start.to(v.range().end);
            res(DefNode {
                var: a,
                exp: v,
                range,
            })
        },
    ))(input)
}

#[test_parser("a = 1")]
pub fn assignment(input: Span) -> IResult<Span, Box<dyn Node>> {
    delspace(map_res(
        tuple((take_exp, tag_token(TokenType::ASSIGN), logic_exp)),
        |(left, _op, right)| {
            let range = left.range().start.to(right.range().end);
            res(AssignNode {
                var: left,
                exp: right,
                range,
            })
        },
    ))(input)
}

#[test_parser(".10")]
#[test_parser("10.")]
#[test_parser("10.10")]
#[test_parser("10")]
pub fn number(input: Span) -> IResult<Span, Box<dyn Node>> {
    let (input, _) = space0(input)?;
    let (re, value) = alt((
        map_res(float, |out| {
            Ok::<Num, Error>(Num::FLOAT(out.fragment().parse::<f64>().unwrap()))
        }),
        map_res(decimal, |out| {
            // TODO:err tolerate
            Ok::<Num, Error>(Num::INT(out.fragment().parse::<u64>().unwrap()))
        }),
    ))(input)?;
    let range = Range::new(input, re);
    let node = NumNode { value, range };
    Ok((re, Box::new(node)))
}

#[test_parser("a + 1")]
#[test_parser("a - 1")]
pub fn add_exp(input: Span) -> IResult<Span, Box<dyn Node>> {
    parse_bin_ops!(mul_exp, PLUS, MINUS)(input)
}

#[test_parser("1 * 1")]
#[test_parser("1 / 1")]
pub fn mul_exp(input: Span) -> IResult<Span, Box<dyn Node>> {
    parse_bin_ops!(unary_exp, MUL, DIV)(input)
}

#[test_parser("break\n")]
pub fn break_statement(input: Span) -> IResult<Span, Box<dyn Node>> {
    terminated(
        delspace(map_res(tag_token(TokenType::BREAK), |_| {
            res(BreakNode {
                range: Range::new(input, input),
            })
        })),
        newline,
    )(input)
}
#[test_parser("continue\n")]
pub fn continue_statement(input: Span) -> IResult<Span, Box<dyn Node>> {
    terminated(
        delspace(map_res(tag_token(TokenType::CONTINUE), |_| {
            res(ContinueNode {
                range: Range::new(input, input),
            })
        })),
        newline,
    )(input)
}

#[test_parser("-1")]
#[test_parser("!a")]
#[test_parser_error("+a")]
pub fn unary_exp(input: Span) -> IResult<Span, Box<dyn Node>> {
    delspace(alt((
        take_exp,
        map_res(
            tuple((
                alt((tag_token(TokenType::MINUS), tag_token(TokenType::NOT))),
                take_exp,
            )),
            |((op, _), exp)| {
                let range = exp.range();
                res(UnaryOpNode { op, exp, range })
            },
        ),
    )))(input)
}
#[test_parser("a>b")]
#[test_parser("a>=b")]
#[test_parser("a<b")]
#[test_parser("a<=b")]
#[test_parser("a==b")]
#[test_parser("a!=b")]
pub fn compare_exp(input: Span) -> IResult<Span, Box<dyn Node>> {
    parse_bin_ops!(add_exp, GEQ, LEQ, NE, EQ, LESS, GREATER)(input)
}
#[test_parser("a&&b")]
#[test_parser("a||b")]
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
                range: Range::new(out, out.take(out.len())),
            })
        },
    ))(input)
}
pub fn primary_exp(input: Span) -> IResult<Span, Box<dyn Node>> {
    delspace(alt((
        number,
        bool_const,
        delimited(
            tag_token(TokenType::LPAREN),
            logic_exp,
            tag_token(TokenType::RPAREN),
        ),
        function_call,
        struct_init,
        identifier,
    )))(input)
}
#[test_parser("true")]
#[test_parser("false")]
#[test_parser_error("tru")]
#[test_parser_error("fales")]
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
            decimal,
            opt(tuple((
                one_of("eE"),
                opt(alt((
                    tag_token(TokenType::PLUS),
                    tag_token(TokenType::MINUS),
                ))),
                decimal,
            ))),
        ))), // Case two: 42e42 and 42.42e42
        recognize(tuple((
            decimal,
            opt(preceded(char('.'), decimal)),
            one_of("eE"),
            opt(alt((
                tag_token(TokenType::PLUS),
                tag_token(TokenType::MINUS),
            ))),
            decimal,
        ))), // Case three: 42. and 42.42
        recognize(tuple((decimal, char('.'), opt(decimal)))),
    ))(input)
}

#[test_parser("kfsh")]
fn type_name(input: Span) -> IResult<Span, Box<TypeNameNode>> {
    delspace(map_res(identifier, |o| {
        let o = cast_to_var(&o);
        res_ori(TypeNameNode {
            id: o.name,
            range: o.range,
        })
    }))(input)
}

#[test_parser("myname: int")]
fn typed_identifier(input: Span) -> IResult<Span, Box<TypedIdentifierNode>> {
    delspace(map_res(
        tuple((identifier, tag_token(TokenType::COLON), type_name)),
        |(id, _, type_name)| {
            let id = cast_to_var(&id);
            let range = id.range.start.to(type_name.range.end);
            res_ori(TypedIdentifierNode {
                id: id.name,
                tp: type_name,
                range,
            })
        },
    ))(input)
}

/// ```ebnf
/// function_def = "fn" identifier "(" (typed_identifier (","typed_identifier)*)? ")" type_name (statement_block | newline) ;
/// ```
#[test_parser(
    "fn f(  x: int, y  : int  ) int {
        x = x+1
        return 0
    }
    "
)]
#[test_parser("fn   f (x: int ,\n y: int) int\n")]
#[test_parser(
    "fn f(x: int) int {
        x = x+1
        call()
        return 0
    }
    "
)]
#[test_parser("             fn     f(x    :  int)    int\n")]
#[test_parser(
    "fn f() int {
        x = x+1
        return 0
    }
    "
)]
#[test_parser("fn f( \n) int\n")]
fn function_def(input: Span) -> IResult<Span, Box<TopLevel>> {
    delspace(map_res(
        tuple((
            tag_token(TokenType::FN),
            identifier,
            tag_token(TokenType::LPAREN),
            del_newline_or_space!(opt(tuple((
                del_newline_or_space!(typed_identifier),
                many0(preceded(
                    tag_token(TokenType::COMMA),
                    del_newline_or_space!(typed_identifier)
                )),
            )))),
            tag_token(TokenType::RPAREN),
            type_name,
            terminated(opt(statement_block), newline),
        )),
        |(_, id, _, paras, _, ret, body)| {
            let id = cast_to_var(&id);
            let mut paralist = vec![];
            let range = id.range;
            if let Some(para) = paras {
                paralist.push(para.0);
                paralist.extend(para.1);
            }
            let body: Option<StatementsNode> = body;
            let node = FuncDefNode {
                typenode: FuncTypeNode {
                    id: id.name,
                    paralist,
                    ret,
                },
                body,
                range,
            };
            Ok::<_, Error>(Box::new(TopLevel::FuncDef(node)))
        },
    ))(input)
}

#[test_parser("     x    (   1\n,0             ,\n       1      )")]
#[test_parser("     x    (   x)")]
pub fn function_call(input: Span) -> IResult<Span, Box<dyn Node>> {
    delspace(map_res(
        tuple((
            identifier,
            tag_token(TokenType::LPAREN),
            opt(tuple((
                del_newline_or_space!(logic_exp),
                many0(preceded(
                    tag_token(TokenType::COMMA),
                    del_newline_or_space!(logic_exp),
                )),
            ))),
            tag_token(TokenType::RPAREN),
        )),
        |(id, _, paras, _)| {
            let id = cast_to_var(&id);
            let mut paralist = vec![];
            if let Some(paras) = paras {
                paralist.push(paras.0);
                paralist.extend(paras.1);
            }
            res(FuncCallNode {
                id: id.name,
                paralist,
                range: id.range,
            })
        },
    ))(input)
}

#[test_parser("a.b.c")]
fn take_exp(input: Span) -> IResult<Span, Box<dyn Node>> {
    map_res(
        tuple((
            primary_exp,
            many0(preceded(tag_token(TokenType::DOT), identifier)),
        )),
        |(a, b)| {
            if b.is_empty() {
                return res_box(a);
            }
            let mut ids = vec![];
            let range = a.range().start.to(b.last().unwrap().range().end);
            for v in b {
                ids.push(Box::new(cast_to_var(&v)));
            }
            res(TakeOpNode {
                head: a,
                ids,
                range,
            })
        },
    )(input)
}

#[test_parser("jksa: int\n")]
fn struct_field(input: Span) -> IResult<Span, Box<TypedIdentifierNode>> {
    delnl!(terminated(typed_identifier, newline))(input)
}

#[test_parser(
    "struct mystruct {
    myname: int
    myname2: int
}"
)]
fn struct_def(input: Span) -> IResult<Span, Box<TopLevel>> {
    map_res(
        tuple((
            tag_token(TokenType::STRUCT),
            identifier,
            tag_token(TokenType::LBRACE),
            many0(struct_field),
            tag_token(TokenType::RBRACE),
        )),
        |(_, id, _, fields, _)| {
            let id = cast_to_var(&id);
            let range = id.range;
            Ok::<_, Error>(Box::new(TopLevel::StructDef(StructDefNode {
                id: id.name,
                fields,
                range,
            })))
        },
    )(input)
}

#[test_parser("a : 1,")]
/// ```enbf
/// struct_init_field = identifier ":" logic_exp "," ;
/// ```
/// special: del newline or space
fn struct_init_field(input: Span) -> IResult<Span, Box<dyn Node>> {
    del_newline_or_space!(terminated(
        map_res(
            tuple((identifier, tag_token(TokenType::COLON), logic_exp)),
            |(id, _, exp)| {
                let id = cast_to_var(&id);
                let range = id.range.start.to(exp.range().end);
                res(StructInitFieldNode {
                    id: id.name,
                    exp,
                    range,
                })
            },
        ),
        tag_token(TokenType::COMMA)
    ))(input)
}

#[test_parser("a{a : 1,}")]
#[test_parser("a{a : 1,b:2,}")]
#[test_parser("a{}")]
/// ```enbf
/// struct_init = type_name "{" struct_init_field "}" ;
/// ```
fn struct_init(input: Span) -> IResult<Span, Box<dyn Node>> {
    map_res(
        tuple((
            identifier,
            tag_token(TokenType::LBRACE),
            many0(struct_init_field),
            tag_token(TokenType::RBRACE),
        )),
        |(name, _, fields, _)| {
            let name = cast_to_var(&name);
            let range;
            if let Some(last) = fields.last() {
                range = name.range.start.to(last.range().end);
            } else {
                range = name.range;
            }
            res(StructInitNode {
                id: name.name,
                fields,
                range,
            })
        },
    )(input)
}

fn decimal(input: Span) -> IResult<Span, Span> {
    recognize(many1(terminated(one_of("0123456789"), many0(char('_')))))(input)
}
fn tag_token(token: TokenType) -> impl Fn(Span) -> IResult<Span, (TokenType, Range)> {
    move |input| {
        map_res(tag(token.get_str()), |_out: Span| {
            let end = _out.take(token.get_str().len());
            Ok::<(TokenType, Range), Error>((token, Range::new(_out, end)))
        })(input)
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
