use crate::ast::{node::cast::AsNode, tokens::TokenType};
use internal_macro::test_parser;
use nom::{combinator::map_res, multi::many0, sequence::pair, IResult};

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
#[test_parser("(2.3+10-800*9).add(100)[0] as i128 as f32")]
pub fn as_exp(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(
        pair(
            logic_exp,
            many0(pair(tag_modifier(TokenType::AS), type_name)),
        ),
        |(exp, casts)| {
            let mut exp = exp;
            let start = exp.range().start;
            for (_, ty) in casts {
                let end = ty.range().end;
                let range = start.to(end);
                exp = Box::new(NodeEnum::AsNode(AsNode {
                    expr: exp,
                    ty,
                    range,
                }));
            }
            res_box(exp)
        },
    )(input)
}
