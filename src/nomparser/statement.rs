use nom::{
    branch::alt,
    combinator::{map, map_res, opt},
    multi::{many0, separated_list1},
    sequence::{pair, preceded, tuple},
    IResult,
};

use crate::nomparser::Span;
use crate::{
    ast::node::ret::RetNode,
    ast::range::Range,
    ast::tokens::TokenType,
    ast::{diag::ErrorCode, node::global::GlobalNode},
};
use internal_macro::{test_parser, test_parser_error};

use super::*;
#[test_parser(";")]
fn empty_statement(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(
        preceded(tag_token_symbol(TokenType::SEMI), opt(delspace(comment))),
        |optcomment| {
            let comments = if let Some(com) = optcomment {
                vec![vec![com]]
            } else {
                vec![vec![]]
            };
            res_enum(
                EmptyNode {
                    range: Range::new(input, input),
                    comments,
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
    delspace(map_res(
        tuple((
            del_newline_or_space!(tag_token_symbol(TokenType::LBRACE)),
            many0(del_newline_or_space!(statement)),
            del_newline_or_space!(tag_token_symbol(TokenType::RBRACE)),
        )),
        |((_, start), v, (_, end))| {
            let range = start.start.to(end.end);
            Ok::<_, ()>(StatementsNode {
                statements: v,
                range,
            })
        },
    ))(input)
}

pub fn statement_block_node(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(statement_block, |s| res_enum(s.into()))(input)
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
#[test_parser("test!(a);")]
pub fn statement(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(alt((
        semi_stmt(new_variable, new_variable),
        semi_stmt(assignment, assignment),
        if_statement,
        while_statement,
        for_statement,
        break_statement,
        continue_statement,
        return_statement,
        semi_stmt(pointer_exp, pointer_exp),
        empty_statement,
        comment,
        except(
            "\n\r})",
            "failed to parse statement",
            ErrorCode::SYNTAX_ERROR_STATEMENT,
        ),
    )))(input)
}

#[test_parser("let a = 1")]
#[test_parser_error("leta = 1")]
#[test_parser("let (a, b) = 1")]
#[test_parser("let (a, (d,e)) = 1")]
#[test_parser("let (a, {d,e:(a,{d:f,g})}) = 1")]
pub fn new_variable(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(map_res(
        tuple((
            tag_token_word(TokenType::LET),
            deconstruct,
            opt(pair(tag_token_symbol(TokenType::COLON), type_name)),
            opt(pair(tag_token_symbol(TokenType::ASSIGN), general_exp)),
        )),
        |((_, start), var, tp, v)| {
            let mut end = var.range().end;
            if tp.is_some() {
                end = tp.as_ref().unwrap().1.range().end;
            }
            if v.is_some() {
                end = v.as_ref().unwrap().1.range().end;
            }
            let range = start.start.to(end);
            let tp = tp.map(|(_, tp)| tp);
            let exp = v.map(|(_, exp)| exp);
            res_enum(
                DefNode {
                    var,
                    tp,
                    exp,
                    range,
                    comments: vec![],
                }
                .into(),
            )
        },
    ))(input)
}

fn deconstruct(input: Span) -> IResult<Span, Box<DefVar>> {
    alt((
        map(identifier, |i| Box::new(DefVar::Identifier(*i))),
        tuple_deconstruct,
        struct_deconstruct,
    ))(input)
}

fn tuple_deconstruct(input: Span) -> IResult<Span, Box<DefVar>> {
    map(
        tuple((
            tag_token_symbol_ex(TokenType::LPAREN),
            separated_list1(tag_token_symbol_ex(TokenType::COMMA), deconstruct),
            tag_token_symbol_ex(TokenType::RPAREN),
        )),
        |((_, sr), ids, (_, er))| {
            let range = sr.start.to(er.end);
            Box::new(DefVar::TupleDeconstruct(TupleDeconstructNode {
                var: ids,
                range,
            }))
        },
    )(input)
}

fn struct_deconstruct(input: Span) -> IResult<Span, Box<DefVar>> {
    map(
        tuple((
            tag_token_symbol_ex(TokenType::LBRACE),
            separated_list1(
                tag_token_symbol_ex(TokenType::COMMA),
                struct_deconstruct_field,
            ),
            tag_token_symbol_ex(TokenType::RBRACE),
        )),
        |((_, sr), ids, (_, er))| {
            let range = sr.start.to(er.end);
            Box::new(DefVar::StructDeconstruct(StructDeconstructNode {
                var: ids,
                range,
            }))
        },
    )(input)
}

fn struct_deconstruct_field(input: Span) -> IResult<Span, StructFieldDeconstructEnum> {
    alt((
        map(
            tuple((
                identifier,
                tag_token_symbol_ex(TokenType::COLON),
                deconstruct,
            )),
            |(name, _, var)| StructFieldDeconstructEnum::Taged(*name, var),
        ),
        map(identifier, |var| StructFieldDeconstructEnum::Var(*var)),
    ))(input)
}

#[test_parser("(a,d) = 1")]
#[test_parser("a = 1")]
pub fn assignment(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(map_res(
        tuple((
            alt((
                map(pointer_exp, AssignVar::Pointer),
                map(deconstruct, AssignVar::Raw),
            )),
            tag_token_symbol(TokenType::ASSIGN),
            general_exp,
        )),
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

#[test_parser("return 1;")]
#[test_parser("return 1.1;")]
#[test_parser("return true;")]
#[test_parser("return;")]
#[test_parser("return a;")]
#[test_parser("return 1 + 2;")]
#[test_parser("yield return 1 + 2;")]
#[test_parser_error("returntrue;")]
#[test_parser_error("return1 + 2;")]
#[test_parser_error("return a = 2;")]
// ```
// return_statement = "return" logic_exp newline ;
// ```
fn return_statement(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(map_res(
        tuple((
            opt(tag_token_word(TokenType::YIELD)),
            tag_token_word(TokenType::RETURN),
            opt(general_exp),
            tag_token_symbol(TokenType::SEMI),
            opt(delspace(comment)),
        )),
        |(y, (_, range), value, (_, r2), optcomment)| {
            let comments = if let Some(com) = optcomment {
                vec![vec![com]]
            } else {
                vec![vec![]]
            };
            let mut range = range.start.to(r2.end);
            if let Some((_, r)) = y {
                range = r.start.to(range.end);
            }
            res_enum(
                RetNode {
                    value,
                    range,
                    comments,
                    yiel: y,
                }
                .into(),
            )
        },
    ))(input)
}

#[test_parser("const a = 1")]
#[test_parser_error("consta = 1")]
pub fn global_variable(input: Span) -> IResult<Span, Box<NodeEnum>> {
    delspace(map_res(
        tuple((
            tag_token_word(TokenType::CONST),
            identifier,
            tag_token_symbol(TokenType::ASSIGN),
            general_exp,
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
