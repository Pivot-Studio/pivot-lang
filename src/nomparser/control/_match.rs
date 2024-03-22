use control::string_literal::string_literal;
use nom::{
    branch::alt,
    combinator::{map, opt},
    multi::many0,
    sequence::{preceded, terminated, tuple},
    IResult,
};

use crate::nomparser::Span;
use crate::{ast::diag::ErrorCode, ast::range::Range, ast::tokens::TokenType};
use internal_macro::test_parser;

use self::error::{eat_if_error_and_continue, err_tolerable_seplist0};

use super::super::*;

#[test_parser("_")]
#[test_parser("adsada")]
#[test_parser("(a,b,,c)")]
fn match_cond(input: Span) -> IResult<Span, MatchArmCondition> {
    alt((
        map(tag_token_symbol_ex(TokenType::INGNORE), |(_, r)| {
            MatchArmCondition::Discard(r)
        }),
        map(literal, MatchArmCondition::Literal),
        map(
            tuple((
                basic_type,
                tag_token_symbol_ex(TokenType::LPAREN),
                match_cond,
                tag_token_symbol_ex(TokenType::RPAREN),
            )),
            |(bt, _, c, _)| MatchArmCondition::TypedVar(*bt, Box::new(c)),
        ),
        // map(tuple((basic_type, st_body_match)), |(bt, b)| {
        //     MatchArmCondition::TypedDeconstruct(*bt, b)
        // }), // TODO: mabye one day we need to match trait
        map(st_body_match, MatchArmCondition::Deconstruct),
        map(
            tuple((
                tag_token_symbol_ex(TokenType::LPAREN),
                err_tolerable_seplist0(match_cond, match_cond, TokenType::COMMA),
                tag_token_symbol_ex(TokenType::RPAREN),
            )),
            |((_, s), f, (_, e))| MatchArmCondition::Tuple(f, s.start.to(e.end)),
        ),
        map(identifier, |id| MatchArmCondition::Var(*id)),
    ))(input)
}

fn st_body_match(input: Span) -> IResult<Span, Vec<STMatchField>> {
    map(
        tuple((
            tag_token_symbol_ex(TokenType::LBRACE),
            err_tolerable_seplist0(st_match_field, st_match_field, TokenType::COMMA),
            opt(tag_token_symbol_ex(TokenType::COMMA)),
            tag_token_symbol_ex(TokenType::RBRACE),
        )),
        |(_, mut c, _, _)| c.drain(..).map(|(v, _, c)| (*v, c)).collect::<Vec<_>>(),
    )(input)
}

type STMatchRet = (Box<VarNode>, (TokenType, Range), MatchArmCondition);

fn st_match_field(input: Span) -> IResult<Span, STMatchRet> {
    tuple((
        identifier,
        tag_token_symbol_ex(TokenType::COLON),
        match_cond,
    ))(input)
}

#[test_parser("true")]
#[test_parser("1321324.122")]
#[test_parser(r#""hellowdshjkfhsdjk""#)]
fn literal(input: Span) -> IResult<Span, Literal> {
    alt((
        map(number, Literal::Number),
        map(string_literal, Literal::String),
        map(bool_const, Literal::Bool),
    ))(input)
}

#[test_parser(
    r#"match a {
    (a,n) => {
        let a = b;
    }
    A(d) => {
        let a = b;
    }
    {a:1, b:(1,{a:1, b:2})} => {
        let a = b;
    }
    _ => {
        let a = b;
    }

}"#
)]
pub fn match_statement(input: Span) -> IResult<Span, MatchNode> {
    map(
        delspace(tuple((
            tag_token_word(TokenType::MATCH),
            alt_except(
                parse_with_ex(general_exp, true),
                "{",
                "failed to parse match condition",
                ErrorCode::SYNTAX_ERROR_STATEMENT,
            ),
            tag_token_symbol_ex(TokenType::LBRACE),
            many0(tuple((
                eat_if_error_and_continue(
                    terminated(match_cond, tag_token_symbol_ex(TokenType::ARROW)),
                    preceded(
                        statement_block,
                        terminated(match_cond, tag_token_symbol_ex(TokenType::ARROW)),
                    ),
                    TokenType::ARROW,
                ),
                statement_block,
            ))),
            tag_token_symbol_ex(TokenType::RBRACE),
            opt(delspace(comment)),
        ))),
        |((_, rs), exp, _, arms, (_, re), _)| MatchNode {
            value: exp,
            arms,
            range: rs.start.to(re.end),
        },
    )(input)
}
