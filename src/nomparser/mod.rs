use std::fmt::Error;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
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
    ast::{
        diag::ErrorCode,
        node::{
            comment::CommentNode,
            error::{ErrorNode, STErrorNode},
            function::{FuncCallNode, FuncDefNode, FuncTypeNode},
            global::GlobalNode,
            pkg::{ExternIDNode, UseNode},
            types::{
                ArrayInitNode, ArrayTypeNameNode, StructDefNode, TypeNameNode, TypedIdentifierNode,
            },
        },
        tokens::TOKEN_STR_MAP,
    },
    ast::{node::types::StructInitNode, range::Range},
    ast::{
        node::{
            control::*, operator::*, primary::*, program::*, statement::*,
            types::StructInitFieldNode,
        },
        tokens::TokenType,
    },
    Db,
};
use internal_macro::{test_parser, test_parser_error};
use nom::character::complete::char;

use self::error::{alt_except, except};

pub mod error;

// macro_rules! newline_or_eof {
//     () => {
//         alt((delspace(alt((tag("\n"), tag("\r\n")))), eof))
//     };
// }

macro_rules! del_newline_or_space {
    ($e:expr) => {
        delimited(
            many0(alt((tag("\n"), tag("\r\n"), tag(" "), tag("\t")))),
            $e,
            many0(alt((tag("\n"), tag("\r\n"), tag(" "), tag("\t")))),
        )
    };
}

// macro_rules! delnl {
//     ($parser:expr) => {
//         delimited(many0(newline), $parser, many0(newline))
//     };
// }

fn res_enum(t: NodeEnum) -> Result<Box<NodeEnum>, Error> {
    res_box(Box::new(t))
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

#[salsa::tracked]
pub struct SourceProgram {
    #[return_ref]
    pub text: String,
}

#[salsa::tracked(lru = 32)]
pub fn parse(db: &dyn Db, source: SourceProgram) -> Result<ProgramNodeWrapper, String> {
    let text = source.text(db);
    let re = program(Span::new(text));
    if let Err(e) = re {
        return Err(format!("{:?}", e));
    }
    // eprintln!("parse");
    Ok(ProgramNodeWrapper::new(db, re.unwrap().1))
}

/// ```enbf
/// use_statement = "use" identifier ("::" identifier)* ;
/// ```
#[test_parser("use a::b")]
#[test_parser("use a::")]
#[test_parser("use a")]
#[test_parser("use a:")]
pub fn use_statement(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(
        preceded(
            tag_token(TokenType::USE),
            delspace(tuple((
                identifier,
                many0(preceded(tag_token(TokenType::DOUBLE_COLON), identifier)),
                opt(tag_token(TokenType::DOUBLE_COLON)),
                opt(tag_token(TokenType::COLON)),
            ))),
        ),
        |(first, rest, opt, opt2)| {
            let mut path = vec![first];
            path.extend(rest);
            let mut range = path.first().unwrap().range().start.to(path
                .last()
                .unwrap()
                .range()
                .end);
            if opt.is_some() {
                range = range.start.to(opt.unwrap().1.end);
            }
            if opt2.is_some() {
                range = range.start.to(opt2.unwrap().1.end);
            }
            res_enum(NodeEnum::UseNode(UseNode {
                ids: path,
                range,
                complete: opt.is_none() && opt2.is_none(),
                singlecolon: opt2.is_some(),
            }))
        },
    )(input)
}

#[test_parser("return 1;")]
#[test_parser("return 1.1;")]
#[test_parser("return true;")]
#[test_parser("return;")]
#[test_parser("return a;")]
#[test_parser("return 1 + 2;")]
#[test_parser_error("return a = 2;")]
// ```
// return_statement = "return" logic_exp newline ;
// ```
pub fn return_statement(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(map_res(
        tuple((
            tag_token(TokenType::RETURN),
            opt(logic_exp),
            tag_token(TokenType::SEMI),
        )),
        |((_, range), val, _)| {
            if let Some(val) = val {
                let range = val.range();
                res_enum(
                    RetNode {
                        value: Some(val),
                        range,
                    }
                    .into(),
                )
            } else {
                res_enum(RetNode { value: None, range }.into())
            }
        },
    ))(input)
}

#[test_parser(
    "if a > 1 { 
    a = 1;
} else {
    a = 2;
}"
)]
#[test_parser(
    "if true {
    a = 1;
} else if false {
    a = 2;
} else {
    a = 3;
}"
)]
#[test_parser_error(
    "if true {
    a = 1;
} else if false {
    a = 2;
} else {
    a = 3;
} else {
    a = 4;
}"
)]
/// ```ebnf
/// if_statement = "if" logic_exp statement_block ("else" (if_statement | statement_block))? ;
/// ```
pub fn if_statement(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(
        delspace(tuple((
            tag_token(TokenType::IF),
            logic_exp,
            statement_block,
            opt(preceded(
                tag_token(TokenType::ELSE),
                alt((
                    if_statement,
                    map_res(statement_block, |n| res_enum(n.into())),
                )),
            )),
        ))),
        |(_, cond, then, els)| {
            let mut range = cond.range().start.to(then.range.end);
            if let Some(el) = &els {
                range = range.start.to(el.range().end);
            }
            res_enum(
                IfNode {
                    cond,
                    then: Box::new(then.into()),
                    els,
                    range,
                }
                .into(),
            )
        },
    )(input)
}
#[test_parser(
    "while true {
    let a = b;
}"
)]
#[test_parser(
    "while true {
    let a = b;
}
"
)]
/// ```ebnf
/// while_statement = "while" logic_exp statement_block ;
/// ```
pub fn while_statement(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(
        delspace(tuple((
            tag_token(TokenType::WHILE),
            alt_except(
                logic_exp,
                "{",
                "failed to parse while condition",
                ErrorCode::WHILE_CONDITION_MUST_BE_BOOL,
            ),
            statement_block,
        ))),
        |(_, cond, body)| {
            let range = cond.range().start.to(body.range.end);
            res_enum(
                WhileNode {
                    cond,
                    body: Box::new(body),
                    range,
                }
                .into(),
            )
        },
    )(input)
}

#[test_parser(
    " for ;true; {
    let a = b;
}"
)]
#[test_parser(
    "for;true; {
    let a = b;
}"
)]
#[test_parser(
    "for i = 0; i < 3; i = i + 1 {
    b = c + i;
}"
)]
#[test_parser(
    "for i = 1; i <= 5; i = i + 1{
    b = b + 1;
}"
)]
#[test_parser(
    "for let i = 0; i < 5; i = i + 1{
                
    }"
)]

/// ```enbf
/// for_statement = "for" (assignment | new_variable) ";" logic_exp ";" assignment statement_block;
/// ```
pub fn for_statement(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(
        delspace(tuple((
            tag_token(TokenType::FOR),
            opt(alt((assignment, new_variable))),
            tag_token(TokenType::SEMI),
            logic_exp,
            tag_token(TokenType::SEMI),
            opt(assignment),
            statement_block,
        ))),
        |(_, pre, _, cond, _, opt, body)| {
            let mut range = cond.range().start.to(body.range.end);
            if let Some(pre) = &pre {
                range = range.end.from(pre.range().start);
            }
            res_enum(
                ForNode {
                    pre,
                    cond,
                    opt,
                    body: Box::new(body),
                    range,
                }
                .into(),
            )
        },
    )(input)
}

pub fn empty_statement(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(tag_token(TokenType::SEMI), |_| {
        res_enum(
            EmptyNode {
                range: Range::new(input, input),
            }
            .into(),
        )
    })(input)
}

pub fn comment(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(
        delimited(tag("//"), take_until("\n"), tag("\n")),
        |c: LocatedSpan<&str>| {
            res_enum(
                CommentNode {
                    comment: c.to_string(),
                    range: Range::new(input, c.take_split(c.len()).0),
                }
                .into(),
            )
        },
    )(input)
}

#[test_parser("{let a = 1;}")]
#[test_parser("{}")]
#[test_parser(
    "{

}"
)]
pub fn statement_block(input: Span) -> IResult<Span, StatementsNode> {
    delspace(delimited(
        del_newline_or_space!(tag_token(TokenType::LBRACE)),
        statements,
        del_newline_or_space!(tag_token(TokenType::RBRACE)),
    ))(input)
}
/// # semi_statement
/// 将原始parser接上一个分号
///
/// 对缺少分号的情况能自动进行错误容忍
///
/// 所有分号结尾的statement都应该使用这个宏来处理分号
macro_rules! semi_statement {
    ($e:expr) => {
        alt((terminated($e, tag_token(TokenType::SEMI)), map_res(tuple(($e,recognize(many0(alt((tag("\n"), tag("\r\n"), tag(" "), tag("\t"))))))), |(node,e)|{
            let range = node.range();
            let r = Range::new(e,e);
            res_enum(STErrorNode{
                range: range,
                st: node,
                err:ErrorNode{
                    range: r,//Range{start: range.end, end: range.end},
                    msg: "missing semi".to_string(),
                    code: ErrorCode::MISSING_SEMI,
                    src:"".to_string(),
                },
            }.into())
        })))
    };
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
pub fn statement(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(alt((
        semi_statement!(new_variable),
        semi_statement!(assignment),
        if_statement,
        while_statement,
        for_statement,
        break_statement,
        continue_statement,
        return_statement,
        semi_statement!(function_call),
        semi_statement!(take_exp), // for completion
        empty_statement,
        comment,
        except(
            "\n\r}",
            "failed to parse statement",
            ErrorCode::SYNTAX_ERROR_STATEMENT,
        ),
    )))(input)
}

enum TopLevel {
    StructDef(StructDefNode),
    FuncDef(FuncDefNode),
    GlobalDef(GlobalNode),
    Common(Box<NodeEnum>),
    Use(Box<NodeEnum>),
}

fn top_level_statement(input: Span) -> IResult<Span, Box<TopLevel>> {
    delspace(alt((
        del_newline_or_space!(function_def),
        del_newline_or_space!(struct_def),
        map_res(
            del_newline_or_space!(semi_statement!(global_variable)),
            |node| {
                Ok::<_, Error>(Box::new(if let NodeEnum::Global(g) = *node {
                    TopLevel::GlobalDef(g)
                } else {
                    TopLevel::Common(node)
                }))
            },
        ),
        map_res(del_newline_or_space!(comment), |c| {
            Ok::<_, Error>(Box::new(TopLevel::Common(c)))
        }),
        map_res(del_newline_or_space!(semi_statement!(use_statement)), |c| {
            Ok::<_, Error>(Box::new(TopLevel::Use(c)))
        }),
        map_res(
            del_newline_or_space!(except(
                "\n\r",
                "failed to parse top level statement",
                ErrorCode::SYNTAX_ERROR_TOP_STATEMENT
            )),
            |e| Ok::<_, Error>(Box::new(TopLevel::Common(e))),
        ),
    )))(input)
}

pub fn statements(input: Span) -> IResult<Span, StatementsNode> {
    map_res(many0(del_newline_or_space!(statement)), |v| {
        let mut range = Range::new(input, input);
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

pub fn program(input: Span) -> IResult<Span, Box<NodeEnum>> {
    let old = input;
    let mut input = input;
    let mut nodes = vec![];
    let mut structs = vec![];
    let mut fntypes = vec![];
    let mut globaldefs = vec![];
    let mut uses = vec![];
    loop {
        let top = top_level_statement(input);
        if let Ok((i, t)) = top {
            match *t {
                TopLevel::FuncDef(f) => {
                    fntypes.push(f.typenode.clone());
                    nodes.push(Box::new(f.into()));
                }
                TopLevel::StructDef(s) => {
                    structs.push(s.clone());
                    nodes.push(Box::new(s.into()));
                }
                TopLevel::Common(c) => {
                    nodes.push(c);
                }
                TopLevel::GlobalDef(g) => {
                    globaldefs.push(g.clone());
                    nodes.push(Box::new(g.into()));
                }
                TopLevel::Use(b) => {
                    uses.push(b.clone());
                    nodes.push(b);
                }
            }
            input = i;
        } else if let Err(err) = top {
            let e: Result<
                (LocatedSpan<&str>, LocatedSpan<&str>),
                nom::Err<nom::error::Error<Span>>,
            > = eof(input);
            if e.is_ok() {
                break;
            }
            return Err(err);
        }
    }
    let node: Box<NodeEnum> = Box::new(
        ProgramNode {
            nodes,
            structs,
            fntypes,
            globaldefs,
            range: Range::new(old, input),
            uses,
        }
        .into(),
    );
    Ok((input, node))
}

#[test_parser("let a = 1")]
pub fn new_variable(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(preceded(
        tag_token(TokenType::LET),
        alt((
            map_res(
                tuple((identifier, tag_token(TokenType::ASSIGN), logic_exp)),
                |(a, _, v)| {
                    let range = a.range().start.to(v.range().end);
                    res_enum(
                        DefNode {
                            var: *a,
                            tp: None,
                            exp: Some(v),
                            range,
                        }
                        .into(),
                    )
                },
            ),
            map_res(
                tuple((identifier, tag_token(TokenType::COLON), type_name)),
                |(a, _, tp)| {
                    let range = a.range().start.to(tp.range().end);
                    res_enum(
                        DefNode {
                            var: *a,
                            tp: Some(tp),
                            exp: None,
                            range,
                        }
                        .into(),
                    )
                },
            ),
        )),
    ))(input)
}

#[test_parser("const a = 1")]
fn global_variable(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(map_res(
        tuple((
            tag_token(TokenType::CONST),
            identifier,
            tag_token(TokenType::ASSIGN),
            logic_exp,
        )),
        |(_, var, _, exp)| {
            let range = var.range().start.to(exp.range().end);
            res_enum(
                GlobalNode {
                    var: *var,
                    exp,
                    range,
                }
                .into(),
            )
        },
    ))(input)
}

#[test_parser("a = 1")]
pub fn assignment(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(map_res(
        tuple((take_exp, tag_token(TokenType::ASSIGN), logic_exp)),
        |(left, _op, right)| {
            let range = left.range().start.to(right.range().end);
            res_enum(
                AssignNode {
                    var: left,
                    exp: right,
                    range,
                }
                .into(),
            )
        },
    ))(input)
}

#[test_parser(".10")]
#[test_parser("10.")]
#[test_parser("10.10")]
#[test_parser("10")]
pub fn number(input: Span) -> IResult<Span, Box<NodeEnum>> {
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
    Ok((re, Box::new(node.into())))
}

#[test_parser("a + 1")]
#[test_parser("a - 1")]
pub fn add_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    parse_bin_ops!(mul_exp, PLUS, MINUS)(input)
}

#[test_parser("1 * 1")]
#[test_parser("1 / 1")]
pub fn mul_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    parse_bin_ops!(unary_exp, MUL, DIV)(input)
}

#[test_parser("break;")]
pub fn break_statement(input: Span) -> IResult<Span, Box<NodeEnum>> {
    terminated(
        map_res(tag_token(TokenType::BREAK), |_| {
            res_enum(
                BreakNode {
                    range: Range::new(input, input),
                }
                .into(),
            )
        }),
        tag_token(TokenType::SEMI),
    )(input)
}
#[test_parser("continue;")]
pub fn continue_statement(input: Span) -> IResult<Span, Box<NodeEnum>> {
    terminated(
        map_res(tag_token(TokenType::CONTINUE), |_| {
            res_enum(
                ContinueNode {
                    range: Range::new(input, input),
                }
                .into(),
            )
        }),
        tag_token(TokenType::SEMI),
    )(input)
}

#[test_parser("a::a")]
#[test_parser("b::c::b")]
#[test_parser("a")]
#[test_parser("a:")]
#[test_parser("a::")]
fn extern_identifier(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(map_res(
        tuple((
            pair(
                identifier,
                many0(preceded(
                    tag_token(TokenType::DOUBLE_COLON),
                    delspace(identifier),
                )),
            ),
            opt(tag_token(TokenType::DOUBLE_COLON)), // 容忍未写完的语句
            opt(tag_token(TokenType::COLON)),        // 容忍未写完的语句
        )),
        |((a, mut ns), opt, opt2)| {
            ns.insert(0, a);
            let id = ns.pop().unwrap();
            let mut range = id.range();
            if opt.is_some() {
                range = range.start.to(opt.unwrap().1.end);
            }
            if opt2.is_some() {
                range = range.start.to(opt2.unwrap().1.end);
            }
            res_enum(
                ExternIDNode {
                    ns,
                    id,
                    range,
                    complete: opt.is_none() && opt2.is_none(),
                    singlecolon: opt2.is_some(),
                }
                .into(),
            )
        },
    ))(input)
}

#[test_parser("-1")]
#[test_parser("!a")]
#[test_parser_error("+a")]
pub fn unary_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(alt((
        take_exp,
        map_res(
            tuple((
                alt((tag_token(TokenType::MINUS), tag_token(TokenType::NOT))),
                take_exp,
            )),
            |((op, _), exp)| {
                let range = exp.range();
                res_enum(UnaryOpNode { op, exp, range }.into())
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
pub fn compare_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    parse_bin_ops!(add_exp, GEQ, LEQ, NE, EQ, LESS, GREATER)(input)
}
#[test_parser("a&&b")]
#[test_parser("a||b")]
pub fn logic_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    parse_bin_ops!(compare_exp, AND, OR)(input)
}
pub fn identifier(input: Span) -> IResult<Span, Box<VarNode>> {
    delspace(map_res(
        recognize(pair(
            alt((alpha1::<Span, nom::error::Error<Span>>, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_")))),
        )),
        |out| {
            let a = TOKEN_STR_MAP.get(out.fragment());
            if a.is_some() {
                return Err(Error {});
            }
            Ok(Box::new(VarNode {
                name: out.to_string(),
                range: Range::new(out, out.take_split(out.len()).0),
            }))
        },
    ))(input)
}
pub fn primary_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(alt((
        number,
        bool_const,
        delimited(
            tag_token(TokenType::LPAREN),
            logic_exp,
            tag_token(TokenType::RPAREN),
        ),
        array_element,
        function_call,
        struct_init,
        array_init,
        extern_identifier,
    )))(input)
}
#[test_parser("true")]
#[test_parser("false")]
#[test_parser_error("tru")]
#[test_parser_error("fales")]
fn bool_const(input: Span) -> IResult<Span, Box<NodeEnum>> {
    alt((
        map_res(tag("true"), |out| {
            res_enum(
                BoolConstNode {
                    value: true,
                    range: Range::new(input, out),
                }
                .into(),
            )
        }),
        map_res(tag("false"), |out| {
            res_enum(
                BoolConstNode {
                    value: false,
                    range: Range::new(input, out),
                }
                .into(),
            )
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

fn type_name(input: Span) -> IResult<Span, Box<TypeNodeEnum>> {
    delspace(alt((basic_type, array_type)))(input)
}

#[test_parser("kfsh")]
fn basic_type(input: Span) -> IResult<Span, Box<TypeNodeEnum>> {
    delspace(map_res(extern_identifier, |exid| {
        let exid = match *exid {
            NodeEnum::ExternIDNode(exid) => exid,
            _ => unreachable!(),
        };
        let range = exid.range;
        Ok::<_, Error>(Box::new(TypeNodeEnum::BasicTypeNode(TypeNameNode {
            id: Some(exid),
            range,
        })))
    }))(input)
}

#[test_parser("myname: int")]
fn typed_identifier(input: Span) -> IResult<Span, Box<TypedIdentifierNode>> {
    delspace(map_res(
        tuple((
            identifier,
            tag_token(TokenType::COLON),
            opt(type_name),
            opt(comment),
        )),
        |(id, _, type_name, d)| {
            let mut range = id.range;
            let mut tprange = range;
            tprange.end.column += 1;
            tprange.start = tprange.end;
            let mut tp = Box::new(TypeNodeEnum::BasicTypeNode(TypeNameNode {
                id: None,
                range: tprange,
            }));

            let mut doc = None;
            if let Some(d1) = d {
                if let NodeEnum::Comment(d1) = *d1 {
                    doc = Some(d1);
                }
            }

            if let Some(type_name) = type_name {
                range = id.range.start.to(type_name.range().end);
                tp = type_name;
            }

            res_box(Box::new(TypedIdentifierNode {
                id: *id,
                tp,
                doc,
                range,
            }))
        },
    ))(input)
}

/// ```ebnf
/// function_def = "fn" identifier "(" (typed_identifier (","typed_identifier)*)? ")" type_name (statement_block | newline) ;
/// ```
#[test_parser(
    "fn f(  x: int, y  : int  ) int {
        x = x+1;
        return 0;
    }
    "
)]
#[test_parser("fn   f (x: int ,\n y: int) int;")]
#[test_parser(
    "fn f(x: int) int {
        x = x+1;
        call();
        return 0;
    }
    "
)]
#[test_parser("             fn     f(x    :  int)    int;")]
#[test_parser(
    "fn f() int {
        x = x+1;
        return 0;
    }
    "
)]
#[test_parser("fn f( \n) int;")]
fn function_def(input: Span) -> IResult<Span, Box<TopLevel>> {
    map_res(
        tuple((
            many0(comment),
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
            alt((
                map_res(statement_block, |b| Ok::<_, Error>(Some(b))),
                map_res(tag_token(TokenType::SEMI), |_| Ok::<_, Error>(None)),
            )),
        )),
        |(doc, _, id, _, paras, _, ret, body)| {
            let mut paralist = vec![];
            let range = id.range;
            if let Some(para) = paras {
                paralist.push(para.0);
                paralist.extend(para.1);
            }
            let node = FuncDefNode {
                typenode: FuncTypeNode {
                    id: id.name,
                    paralist,
                    ret,
                    range,
                    doc,
                    declare: body.is_none(),
                },
                body,
                range,
            };
            Ok::<_, Error>(Box::new(TopLevel::FuncDef(node)))
        },
    )(input)
}

#[test_parser("     x    (   1\n,0             ,\n       1      )")]
#[test_parser("     x    (   x)")]
pub fn function_call(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(map_res(
        tuple((
            extern_identifier,
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
            let range = id.range();
            let id = match *id {
                NodeEnum::ExternIDNode(id) => id,
                _ => unreachable!(),
            };
            let mut paralist = vec![];
            if let Some(paras) = paras {
                paralist.push(paras.0);
                paralist.extend(paras.1);
            }
            res_enum(
                FuncCallNode {
                    id,
                    paralist,
                    range,
                }
                .into(),
            )
        },
    ))(input)
}

#[test_parser("a.b.c")]
fn take_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    let re = delspace(tuple((
        primary_exp,
        many0(preceded(tag_token(TokenType::DOT), identifier)),
    )))(input);
    if let Err(e) = re {
        return Err(e);
    }
    let (mut input, (a, b)) = re.unwrap();
    let r = a.range();
    let mut node = TakeOpNode {
        head: a,
        ids: Vec::new(),
        range: r,
        complete: true,
    };
    let re = take_utf8_split(&input);
    if re.1.to_string().as_str() == "." {
        input = re.0;
        node.complete = false;
        node.range.end.column += 1;
    }
    if b.is_empty() {
        return Ok((input, box_node(node.into())));
    }
    node.range = r.start.to(b.last().unwrap().range().end);
    if !node.complete {
        node.range.end.column += 1;
    }
    node.ids = b;

    Ok((input, box_node(node.into())))
}

#[test_parser(
    "struct mystruct {
    myname: int;//123
    myname2: int;
}"
)]
fn struct_def(input: Span) -> IResult<Span, Box<TopLevel>> {
    map_res(
        tuple((
            many0(comment),
            tag_token(TokenType::STRUCT),
            identifier,
            del_newline_or_space!(tag_token(TokenType::LBRACE)),
            many0(tuple((
                del_newline_or_space!(typed_identifier),
                opt(tag_token(TokenType::SEMI)),
                opt(comment),
            ))),
            del_newline_or_space!(tag_token(TokenType::RBRACE)),
        )),
        |(doc, _, id, _, fields, _)| {
            let range = id.range;
            let mut fieldlist = vec![];
            for mut f in fields {
                f.0.doc = None;
                if let Some(c) = &f.2 {
                    if let NodeEnum::Comment(c) = *c.clone() {
                        f.0.doc = Some(c);
                    }
                }
                fieldlist.push((f.0.clone(), f.1.is_some()));
            }
            Ok::<_, Error>(Box::new(TopLevel::StructDef(StructDefNode {
                doc,
                id: id.name,
                fields: fieldlist,
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
fn struct_init_field(input: Span) -> IResult<Span, Box<NodeEnum>> {
    del_newline_or_space!(map_res(
        tuple((
            identifier,
            tag_token(TokenType::COLON),
            logic_exp,
            opt(tag_token(TokenType::COMMA))
        )),
        |(id, _, exp, has_comma)| {
            let range = id.range.start.to(exp.range().end);
            res_enum(
                StructInitFieldNode {
                    id: *id,
                    exp,
                    range,
                    has_comma: has_comma.is_some(),
                }
                .into(),
            )
        },
    ))(input)
}

#[test_parser("a{a : 1,}")]
#[test_parser("a{a : 1,b:2,}")]
#[test_parser("a{}")]
/// ```enbf
/// struct_init = type_name "{" struct_init_field "}" ;
/// ```
fn struct_init(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(
        tuple((
            type_name,
            tag_token(TokenType::LBRACE),
            many0(struct_init_field),
            tag_token(TokenType::RBRACE),
        )),
        |(name, _, fields, _)| {
            let range;
            if let Some(last) = fields.last() {
                range = name.range().start.to(last.range().end);
            } else {
                range = name.range();
            }
            res_enum(
                StructInitNode {
                    tp: name,
                    fields,
                    range,
                }
                .into(),
            )
        },
    )(input)
}

fn array_type(input: Span) -> IResult<Span, Box<TypeNodeEnum>> {
    map_res(
        tuple((
            tag_token(TokenType::LBRACKET),
            type_name,
            tag_token(TokenType::MUL),
            number,
            tag_token(TokenType::RBRACKET),
        )),
        |(_, tp, _, size, _)| {
            let range = size.range().start.to(tp.range().end);

            Ok::<_, Error>(Box::new(TypeNodeEnum::ArrayTypeNode(
                ArrayTypeNameNode {
                    id: tp,
                    size,
                    range,
                }
                .into(),
            )))
        },
    )(input)
}

fn array_init(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(
        tuple((
            tag_token(TokenType::LBRACKET),
            many0(terminated(logic_exp, tag_token(TokenType::COMMA))),
            tag_token(TokenType::RBRACKET),
        )),
        |(_, exps, _)| {
            let range;
            if let Some(last) = exps.last() {
                range = last.range().start.to(last.range().end);
            } else {
                range = Range::default();
            }
            res_enum(ArrayInitNode { exps, range }.into())
        },
    )(input)
}

/// ```ebnf
/// array_element = (function_call | extern_identifier) '[' logic_exp  ']' ;
/// ```
fn array_element(input: Span) -> IResult<Span, Box<NodeEnum>> {
    let re = delspace(tuple((
        alt((function_call, extern_identifier)), //TODO: support take_exp
        many1(delimited(
            tag_token(TokenType::LBRACKET),
            logic_exp,
            tag_token(TokenType::RBRACKET),
        )),
    )))(input);
    if let Err(e) = re {
        return Err(e);
    }
    let (input, (a, b)) = re.unwrap();
    let r = a.range();
    let mut arr_elem_node = ArrayElementNode {
        arr: a.into(),
        index: b[0].clone(),
        range: r.start.to(b[0].range().end),
    };
    for v in &b[1..] {
        arr_elem_node = ArrayElementNode {
            arr: box_node(arr_elem_node.into()),
            index: v.clone(),
            range: r.start.to(v.range().end),
        };
    }
    Ok((input, box_node(arr_elem_node.into())))
}

fn decimal(input: Span) -> IResult<Span, Span> {
    recognize(many1(terminated(one_of("0123456789"), many0(char('_')))))(input)
}
fn tag_token(token: TokenType) -> impl Fn(Span) -> IResult<Span, (TokenType, Range)> {
    move |input| {
        map_res(delspace(tag(token.get_str())), |_out: Span| {
            let end = _out.take_split(token.get_str().len()).0;
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
