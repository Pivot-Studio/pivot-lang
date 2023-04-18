use super::{
    helper::{modifiable, res_enum, semi_stmt, tag_token_symbol_ex},
    identifier::identifier,
    types::{generic_type_def, type_name},
    Span,
};
use crate::ast::node::RangeTrait;
use crate::ast::{
    node::{union::UnionDefNode, NodeEnum},
    tokens::TokenType,
};
use internal_macro::test_parser;
use nom::{
    combinator::{map_res, opt},
    multi::separated_list1,
    sequence::tuple,
    IResult, Parser,
};

#[test_parser("pub type A<T|F> = B<T> | C")]
#[test_parser("type A=B")]
#[test_parser(
    "pub
 type A<T|F> 
 = 
 B<T> | C"
)]
fn union(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(
        modifiable(
            tuple((
                tag_token_symbol_ex(TokenType::TYPE),
                identifier,
                opt(generic_type_def),
                tag_token_symbol_ex(TokenType::ASSIGN),
                separated_list1(tag_token_symbol_ex(TokenType::TYPE_OR), type_name),
            )),
            TokenType::PUB,
        ),
        |(modifier, ((_, st_r), name, generics, _, sum_types))| {
            let range = st_r.start.to(sum_types.last().unwrap().range().end);
            res_enum(
                UnionDefNode {
                    modifier,
                    name: *name,
                    generics,
                    sum_types,
                    range,
                }
                .into(),
            )
        },
    )(input)
}

/// # union
///
/// union is also called `sum type` in other languages
///
/// ## Syntax
///
/// ```pl
/// type A = B | C | D;
/// type A<T> = B<T> | C | D;
/// ```
pub fn union_stmt(input: Span) -> IResult<Span, Box<NodeEnum>> {
    semi_stmt(union, union).parse(input)
}
