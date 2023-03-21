use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map_res, opt},
    multi::{many0, separated_list0},
    sequence::{delimited, preceded, tuple},
    IResult,
};

use crate::{ast::node::function::FuncDefNode, ast::tokens::TokenType};
use crate::{ast::node::interface::TraitBoundNode, nomparser::Span};

use internal_macro::{test_parser, test_parser_error};

use super::*;

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
#[test_parser(
    "fn f<T>() int 
    where 
        T:X,
        S:Y
    {
        x = x+1;
        return 0;
    }
    "
)]
#[test_parser("fn f( \n) int;")]
#[test_parser_error("fnf( \n) int;")]
pub fn function_def(input: Span) -> IResult<Span, Box<TopLevel>> {
    map_res(
        tuple((
            many0(del_newline_or_space!(comment)),
            modifiable(tag_token_word(TokenType::FN), TokenType::PUB),
            identifier,
            opt(generic_type_def),
            tag_token_symbol(TokenType::LPAREN),
            del_newline_or_space!(separated_list0(
                tag_token_symbol(TokenType::COMMA),
                del_newline_or_space!(typed_identifier),
            )),
            tag_token_symbol(TokenType::RPAREN),
            type_name,
            opt(del_newline_or_space!(preceded(
                tag_token_symbol(TokenType::WHERE),
                separated_list0(
                    tag_token_symbol(TokenType::COMMA),
                    del_newline_or_space!(trait_bound),
                ),
            ))),
            alt((
                map_res(statement_block, |b| Ok::<_, ()>((Some(b.clone()), b.range))),
                map_res(tag_token_symbol(TokenType::SEMI), |(_, range)| {
                    Ok::<_, ()>((None, range))
                }),
            )),
        )),
        |(
            doc,
            (modifier, (_, start)),
            id,
            generics,
            _,
            paras,
            _,
            ret,
            trait_bounds,
            (body, end),
        )| {
            let range = start.start.to(end.end);
            let mut docs = vec![];
            let mut precoms = vec![];
            for d in doc {
                if let NodeEnum::Comment(com) = *d {
                    if com.is_doc {
                        docs.push(Box::new(NodeEnum::Comment(com.clone())));
                    }
                    precoms.push(Box::new(NodeEnum::Comment(com)));
                }
            }
            let node = FuncDefNode {
                id,
                paralist: paras,
                ret,
                trait_bounds,
                range,
                doc: docs,
                precom: precoms,
                declare: body.is_none(),
                generics_size: generics.as_ref().map_or(0, |g| g.generics.len()),
                generics,
                body,
                modifier,
            };
            Ok::<_, ()>(Box::new(TopLevel::FuncType(node)))
        },
    )(input)
}

/// ```ebnf
/// call_function_op = "(" (logic_exp (","logic_exp)*)? ")" ;
/// ```
#[test_parser("(a,c,c)")]
#[test_parser("<T|S::k|i64>(a,c,c)")]
pub fn call_function_op(input: Span) -> IResult<Span, (ComplexOp, Vec<Box<NodeEnum>>)> {
    delspace(map_res(
        tuple((
            opt(generic_param_def),
            tag_token_symbol(TokenType::LPAREN),
            del_newline_or_space!(separated_list0(
                tag_token_symbol(TokenType::COMMA),
                del_newline_or_space!(logic_exp)
            )),
            tag_token_symbol(TokenType::RPAREN),
            many0(comment),
        )),
        |(generic, (_, st), paras, (_, end), com)| {
            Ok::<_, ()>((
                ComplexOp::CallOp((paras, st.start.to(end.end), generic)),
                com,
            ))
        },
    ))(input)
}

pub fn trait_bound(input: Span) -> IResult<Span, Box<TraitBoundNode>> {
    map_res(
        tuple((identifier, tag_token_symbol(TokenType::COLON), type_name)),
        |(generic, _, impl_trait)| {
            let range = generic.range().start.to(impl_trait.range().end);
            res_box(Box::new(TraitBoundNode {
                generic,
                impl_trait,
                range,
            }))
        },
    )(input)
}
