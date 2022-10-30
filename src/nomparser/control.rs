use nom::{
    branch::alt,
    combinator::{map_res, opt},
    sequence::{preceded, terminated, tuple}, IResult,
};
use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;
use crate::{
    ast::diag::ErrorCode,
    ast::range::Range,
    ast::tokens::TokenType,
};
use internal_macro::{test_parser, test_parser_error};


use super::*;


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