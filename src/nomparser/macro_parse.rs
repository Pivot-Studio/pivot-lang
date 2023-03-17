use nom::{
    branch::alt,
    bytes::complete::{is_a, take_while1},
    combinator::{map_res, recognize},
    multi::{many0, many1},
    sequence::{delimited, preceded, tuple},
    IResult,
};

use crate::{
    ast::node::macro_nodes::{
        MacroLoopStatementNode, MacroMatchExp, MacroMatchParameter, MacroNode, MacroRuleNode,
    },
    nomparser::Span,
};
use crate::{ast::range::Range, ast::tokens::TokenType};
use internal_macro::{test_parser, test_parser_error};

use super::*;

macro_rules! alt_tokens {
    ($($token:ident),+) => {
        alt_tokens!($($token,)+)
    };
    ($($token:ident,)+) => {
        alt((
            $(
                tag_token_symbol(TokenType::$token),
            )+
        ))
    }
}

/// macro_match_exp =
///     | any_exp_except_dollar_and_parantheses
///     | "$" identifier ":" tp
///     | "(" macro_match_exp * ")" "*"
///     | "(" macro_match_exp * ")" "+"
///     | "(" macro_match_exp * ")"
///     ;
#[test_parser("$a:@id")]
#[test_parser("()")]
#[test_parser("$($a:@id,)*")]
#[test_parser("($($fmt:@id,+1dasda,)* / $a:@expr)")]
fn macro_match_exp(input: Span) -> IResult<Span, MacroMatchExp> {
    delspace(alt((
        map_res(
            tuple((
                tag_token_symbol(TokenType::DOLLAR),
                identifier,
                tag_token_symbol(TokenType::COLON),
                alt_tokens!(
                    MACRO_TYPE_ID,
                    MACRO_TYPE_STR,
                    MACRO_TYPE_EXPR,
                    MACRO_TYPE_STMT,
                    MACRO_TYPE_STMTS
                ),
            )),
            |(dollar, id, _, tp)| {
                let range = dollar.1.start.to(tp.1.end);
                Ok::<_, ()>(MacroMatchExp::Parameter(MacroMatchParameter {
                    id: *id,
                    tp,
                    range,
                }))
            },
        ),
        map_res(
            tuple((
                tag_token_symbol(TokenType::DOLLAR),
                tag_token_symbol(TokenType::LPAREN),
                many0(macro_match_exp),
                tag_token_symbol(TokenType::RPAREN),
                alt_tokens!(MUL, PLUS,),
            )),
            |(dollar, _, exps, _, looper)| {
                let range = dollar.1.start.to(looper.1.end);
                Ok::<_, ()>(MacroMatchExp::Looper((exps, range, looper.0)))
            },
        ),
        map_res(
            tuple((
                tag_token_symbol(TokenType::LPAREN),
                many0(macro_match_exp),
                tag_token_symbol(TokenType::RPAREN),
            )),
            |(lparen, exps, rparen)| {
                let range = lparen.1.start.to(rparen.1.end);
                Ok::<_, ()>(MacroMatchExp::Parantheses((exps, range)))
            },
        ),
        map_res(
            alt((
                recognize(many1(is_a(
                    "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
                ))),
                recognize(is_a(",=|-;&^%#@!<>[]{}\\/~`.*+?")),
            )),
            |exp: Span| {
                let range = Range::new(input, exp);
                Ok::<_, ()>(MacroMatchExp::RawTokens((exp.to_string(), range)))
            },
        ),
    )))(input)
}

#[test_parser(
    r#"$(
    let a = 0;
    a = $b;
    return c;
)*"#
)]
fn macro_body_loop_parser(origin: Span) -> IResult<Span, Box<NodeEnum>> {
    let (input, _) = tag_token_symbol(TokenType::DOLLAR)(origin)?;
    let (input, _) = tag_token_symbol(TokenType::LPAREN)(input)?;
    let (input, statements) = many0(del_newline_or_space!(statement))(input)?;
    let (input, _) = tag_token_symbol(TokenType::RPAREN)(input)?;
    let (input, _) = tag_token_symbol(TokenType::MUL)(input)?;
    let range = Range::new(origin, input);
    Ok((
        input,
        Box::new(NodeEnum::MacroLoopStatementNode(MacroLoopStatementNode {
            statements,
            range,
        })),
    ))
}

#[test_parser(
    r#"macro test {

    }"#
)]
#[test_parser(r#"macro test {}"#)]
#[test_parser(
    r#"macro test {
        ($a:@id, $b:@id) => {
            let a = 0;
            a = $b;
            return c;
        };
    }"#
)]
pub fn macro_parser(origin: Span) -> IResult<Span, Box<TopLevel>> {
    // a macro node can have multiple rules
    let (input, _) = tag_token_symbol(TokenType::MACRO)(origin)?;
    let (input, id) = identifier(input)?;
    // rules
    let (input, _) = del_newline_or_space!(tag_token_symbol(TokenType::LBRACE))(input)?;
    let (input, rules) = many0(del_newline_or_space!(macro_rule_parser))(input)?;
    let (input, _) = del_newline_or_space!(tag_token_symbol(TokenType::RBRACE))(input)?;
    let range = Range::new(origin, input);
    Ok((
        input,
        Box::new(TopLevel::Common(Box::new(NodeEnum::MacroNode(MacroNode {
            id: *id,
            rules,
            range,
        })))),
    ))
}

fn macro_rule_parser(origin: Span) -> IResult<Span, MacroRuleNode> {
    let (input, _) = tag_token_symbol_ex(TokenType::LPAREN)(origin)?;
    let (input, match_exp) = many0(macro_match_exp)(input)?;
    let (input, _) = tag_token_symbol_ex(TokenType::RPAREN)(input)?;
    let (input, _) = tag_token_symbol_ex(TokenType::ARROW)(input)?;
    let (input, _) = tag_token_symbol_ex(TokenType::LBRACE)(input)?;
    let (input, body) = many0(del_newline_or_space!(alt((
        statement,
        macro_body_loop_parser,
    ))))(input)?;
    let (input, _) = tag_token_symbol_ex(TokenType::RBRACE)(input)?;
    let (input, _) = tag_token_symbol_ex(TokenType::SEMI)(input)?;
    let range = Range::new(origin, input);
    Ok((
        input,
        MacroRuleNode {
            range,
            match_exp,
            body,
        },
    ))
}

#[test_parser(r#"!(s = 1+2*(3+4))"#)]
#[test_parser(r#"!(a, (b) %@, c)"#)]
#[test_parser_error(r#"!(a, (b %@, c)"#)]
pub fn macro_call_op(origin: Span) -> IResult<Span, Span> {
    preceded(
        tag_token(TokenType::NOT),
        delimited(
            tag_token(TokenType::LPAREN),
            recognize(many0(any_exp_with_parens)),
            tag_token(TokenType::RPAREN),
        ),
    )(origin)
}

#[test_parser(r#"s = 1+2*3+4"#)]
fn any_exp_with_parens(origin: Span) -> IResult<Span, String> {
    alt((
        map_res(
            recognize(delimited(
                tag_token(TokenType::LPAREN),
                any_exp_with_parens,
                tag_token(TokenType::RPAREN),
            )),
            |exp| Ok::<_, ()>(exp.to_string()),
        ),
        map_res(take_while1(|f| f != ')' && f != '('), |exp: Span| {
            Ok::<_, ()>(exp.to_string())
        }),
    ))(origin)
}
