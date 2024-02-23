use control::string_literal::string_literal;
use nom::{
    branch::alt,
    combinator::{map, opt},
    multi::{many0, separated_list0},
    sequence::{preceded, tuple},
    IResult,
};

use crate::nomparser::Span;
use crate::{ast::diag::ErrorCode, ast::range::Range, ast::tokens::TokenType};
use internal_macro::test_parser;

use super::super::*;

#[test_parser("_")]
#[test_parser("adsada")]
fn match_cond(input: Span) -> IResult<Span, MatchArmCondition> {
    alt((
        map(tag_token_symbol_ex(TokenType::INGNORE), |(_, r)| {
            MatchArmCondition::Discard(r)
        }),
        map(literal, |lit| MatchArmCondition::Literal(lit)),
        map(
            tuple((
                basic_type,
                tag_token_symbol_ex(TokenType::LPAREN),
                match_cond,
                tag_token_symbol_ex(TokenType::RPAREN),
            )),
            |(bt, _, c, _)| MatchArmCondition::TypedVar(*bt, Box::new(c)),
        ),
        map(tuple((basic_type, st_body_match)), |(bt, b)| {
            MatchArmCondition::TypedDeconstruct(*bt, b)
        }),
        map(st_body_match, |b| MatchArmCondition::Deconstruct(b)),
        map(
            tuple((
                tag_token_symbol_ex(TokenType::LPAREN),
                separated_list0(tag_token_symbol_ex(TokenType::COMMA), tuple_match_field),
                opt(tag_token_symbol_ex(TokenType::COMMA)),
                tag_token_symbol_ex(TokenType::RPAREN),
            )),
            |(_, f, _, _)| MatchArmCondition::Tuple(f),
        ),
        map(identifier, |id| MatchArmCondition::Var(*id)),
    ))(input)
}

fn tuple_match_field(input: Span) -> IResult<Span, TupleField> {
    alt((
        map(literal, |lit| TupleField::Literal(lit)),
        map(match_cond, |c| TupleField::Var(c)),
    ))(input)
}

fn st_body_match(input: Span) -> IResult<Span, Vec<STMatchField>> {
    map(
        tuple((
            tag_token_symbol_ex(TokenType::LBRACE),
            separated_list0(tag_token_symbol_ex(TokenType::COMMA), st_match_field),
            opt(tag_token_symbol_ex(TokenType::COMMA)),
            tag_token_symbol_ex(TokenType::RBRACE),
        )),
        |(_, mut c, _, _)| c.drain(..).map(|(v, _, c)| (*v, c)).collect::<Vec<_>>(),
    )(input)
}

fn st_match_field(
    input: Span,
) -> IResult<Span, (Box<VarNode>, (TokenType, Range), MatchArmCondition)> {
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
        map(number, |n| Literal::Number(n)),
        map(string_literal, |s| Literal::String(s)),
        map(bool_const, |b| Literal::Bool(b)),
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
    D{a:1, b:d} => {
        let a = d;
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
                match_cond,
                preceded(tag_token_symbol_ex(TokenType::ARROW), statement_block),
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
