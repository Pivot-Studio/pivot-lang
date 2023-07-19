use internal_macro::test_parser;
use nom::{
    combinator::{map_res, opt},
    multi::{many0, separated_list0},
    sequence::{tuple, delimited},
    IResult,
};
use nom::sequence::pair;

use crate::nomparser::Span;
use crate::{
    ast::node::types::ArrayInitNode,
    ast::tokens::TokenType,
    ast::{diag::ErrorCode, node::error::ErrorNode},
};

use super::*;

#[test_parser("[1,2,3]")]
#[test_parser("[test*test2;]")]
#[test_parser("[test*test2;2,3,4]")]
#[test_parser(
    "[
        1,
        2,
        x
    ]"
)]
pub fn array_init(input: Span) -> IResult<Span, Box<NodeEnum>> {
    map_res(
        tuple((
            tag_token_symbol(TokenType::LBRACKET),
            opt( pair(type_name,
             delimited(tag_token_symbol(TokenType::MUL), general_exp, tag_token_symbol(TokenType::SEMI)))),
            separated_list0(
                tag_token_symbol(TokenType::COMMA),
                del_newline_or_space!(general_exp),
            ),
            tag_token_symbol(TokenType::RBRACKET),
        )),
        |((_, lb),tp, exps, (_, rb))| {
            let range = lb.start.to(rb.end);
            res_enum(ArrayInitNode { exps, range, tp }.into())
        },
    )(input)
}

#[test_parser("[123]")]
/// ```ebnf
/// array_element_op = ('[' logic_exp ']') ;
/// ```
pub fn array_element_op(input: Span) -> IResult<Span, (ComplexOp, Vec<Box<NodeEnum>>)> {
    delspace(map_res(
        tuple((
            tag_token_symbol(TokenType::LBRACKET),
            opt(general_exp),
            tag_token_symbol(TokenType::RBRACKET),
            many0(comment),
        )),
        |(_, idx, (_, rr), com)| {
            if let Some(idx) = idx {
                Ok::<_, ()>((ComplexOp::Index(idx), com))
            } else {
                Ok::<_, ()>((
                    ComplexOp::Index(Box::new(NodeEnum::Err(ErrorNode {
                        msg: String::from("Nedded index for array element access"),
                        src: String::from("[]"),
                        code: ErrorCode::NEEDED_INDEX_FOR_ARRAY_ELEMENT_ACCESS,
                        range: rr,
                    }))),
                    com,
                ))
            }
        },
    ))(input)
}
