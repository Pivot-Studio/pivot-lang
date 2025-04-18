use crate::ast::{
    node::cast::{AsNode, ImplCastNode, IsNode},
    tokens::TokenType,
};
use internal_macro::test_parser;
use nom::{
    branch::alt,
    combinator::{map, not, opt, peek},
    multi::many0,
    sequence::{pair, terminated, tuple},
    IResult,
};

use super::*;

/// `as` keyword parser
///
/// `as` performs a *safe* type conversion
///
/// special case: `as` can be used to cast unions
/// however, the casted type is wrapped in an `Option` enum
///
/// # Example
///
/// ```pl
/// let a = 1 as i128 as f32;
/// ```
#[test_parser("1 as i128 as f32")]
#[test_parser("1 as i128 as f32 !")]
#[test_parser("1 as i128! as f32 !")]
#[test_parser(
    "(2.3+10-800*9).add(100)[0] as
 i128 as f32?"
)]
pub fn as_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map(
        tuple((
            complex_exp,
            many0(tuple((
                tag_modifier(TokenType::AS),
                type_name,
                opt(alt((
                    // look the next charactor after "!" to eliminate the "!=" case
                    terminated(
                        tag_token_symbol_ex(TokenType::NOT),
                        peek(not(tag_token(TokenType::ASSIGN))),
                    ),
                    tag_token_symbol_ex(TokenType::QUESTION),
                ))),
            ))),
            opt(pair(tag_modifier(TokenType::IS), type_name)),
            opt(tuple((
                tag_modifier(TokenType::IMPL),
                multi_trait,
                opt(alt((
                    tag_token_symbol_ex(TokenType::NOT),
                    tag_token_symbol_ex(TokenType::QUESTION),
                ))),
            ))),
        )),
        |(exp, casts, is, _impl)| {
            let mut exp = exp;
            let start = exp.range().start;

            // wrap the previous expression into a new 'as' expression
            for (_, target_type, tail) in casts {
                let range = start.to(target_type.range().end);

                exp = Box::new(NodeEnum::AsNode(AsNode {
                    expr: exp,
                    target_type,
                    range,
                    tail,
                }));
            }

            if let Some((_, target_type)) = is {
                let range = start.to(target_type.range().end);
                exp = Box::new(NodeEnum::IsNode(IsNode {
                    expr: exp,
                    target_type,
                    range,
                }));
            }
            if let Some((_impl, traits, tail)) = _impl {
                let range = start.to(traits.range().end);
                exp = Box::new(NodeEnum::ImplCastNode(ImplCastNode {
                    expr: exp,
                    target_type: traits,
                    range,
                    tail,
                }));
            }

            exp
        },
    )(input)
}
