use error::{err_tolerable_seplist0, match_paired_until};
use nom::{
    branch::alt,
    combinator::{map, map_res, opt, peek},
    multi::{many0, separated_list0},
    sequence::{preceded, terminated, tuple},
    IResult,
};

use crate::ast::{diag::ErrorCode, node::function::FuncDefNode, tokens::TokenType};
use crate::{ast::node::function::GeneratorType, nomparser::Span};

use internal_macro::{test_parser, test_parser_error};

use super::*;

#[test_parser("fn f(a,) int;")]
#[test_parser(
    "
    /// this is a comment
    pub gen fn f(  x: int, y  : int  ) int {
        x = x+1;
        return 0;
    }
    "
)]
#[test_parser(
    "
    /// this is a comment
    pub async fn f(  x: int, y  : int  ) int {
        x = x+1;
        return 0;
    }
    "
)]
#[test_parser(
    "pub gen fn f(  x: int, y  : int  ) int {
        x = x+1;
        return 0;
    }
    "
)]
#[test_parser(
    "gen fn f(  x: int, y  : int  ) int {
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
            modifiable(
                opt(alt((
                    tag_token_word(TokenType::GENERATOR_MARKER),
                    tag_token_word(TokenType::ASYNC_MARKER),
                ))),
                TokenType::PUB,
            ),
            tag_token_word(TokenType::FN),
            identifier,
            opt(generic_type_def),
            tag_token_symbol(TokenType::LPAREN),
            alt((
                terminated(
                    del_newline_or_space!(err_tolerable_seplist0(
                        del_newline_or_space!(typed_identifier),
                        del_newline_or_space!(typed_identifier),
                        TokenType::COMMA,
                    )),
                    peek(tag_token_symbol(TokenType::RPAREN)),
                ),
                map(
                    match_paired_until(
                        ")",
                        ErrorCode::SYNTAX_ERROR_FUNC_PARAM,
                        "cannot recognize function parameters",
                    ),
                    |_| vec![],
                ),
            )),
            tag_token_symbol(TokenType::RPAREN),
            type_name,
            opt(del_newline_or_space!(preceded(
                tag_token_symbol(TokenType::WHERE),
                err_tolerable_seplist0(
                    del_newline_or_space!(trait_bound),
                    del_newline_or_space!(trait_bound),
                    TokenType::COMMA
                )
            ))),
            alt((
                map(statement_block, |b| (Some(b.clone()), b.range)),
                map_res(tag_token_symbol(TokenType::SEMI), |(_, range)| {
                    Ok::<_, ()>((None, range))
                }),
            )),
        )),
        |(
            doc,
            (modifier, g),
            (_, start),
            function_identifier,
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
                id: function_identifier,
                paralist: paras,
                ret,
                trait_bounds,
                range,
                doc: docs,
                pre_comments: precoms,
                is_declaration_only: body.is_none(),
                generics_size: generics.as_ref().map_or(0, |g| g.generics.len()),
                generics,
                body,
                modifier,
                impl_trait: None,
                is_method: false,
                target_range: Default::default(),
                in_trait_def: false,
                generator_ty: match g {
                    Some((TokenType::ASYNC_MARKER, _)) => GeneratorType::Async,
                    Some((TokenType::GENERATOR_MARKER, _)) => GeneratorType::Iter,
                    _ => GeneratorType::None,
                },
            };
            Ok::<_, ()>(Box::new(TopLevel::FuncType(node)))
        },
    )(input)
}

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
                del_newline_or_space!(alt((general_exp, statement_block_node)))
            )),
            tag_token_symbol(TokenType::RPAREN),
            many0(comment),
        )),
        |(generic, (_, st), paras, (_, end), com)| {
            Ok::<_, ()>((ComplexOp::Call((paras, st.start.to(end.end), generic)), com))
        },
    ))(input)
}
