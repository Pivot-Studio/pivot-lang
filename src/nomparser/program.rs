use std::fmt::Error;

use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{eof, map_res, recognize},
    multi::many0,
    sequence::{delimited, terminated, tuple},
    IResult,
};
use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;
use crate::{
    ast::range::Range,
    ast::tokens::TokenType,
    ast::{
        diag::ErrorCode,
        node::error::{ErrorNode, STErrorNode},
    },
};

use super::*;

pub fn program(input: Span) -> IResult<Span, Box<NodeEnum>> {
    let old = input;
    let mut input = input;
    let mut nodes = vec![];
    let mut structs = vec![];
    let mut fntypes = vec![];
    let mut globaldefs = vec![];
    let mut uses = vec![];
    loop {
        let top = top_level_statement(input);
        if let Ok((i, t)) = top {
            match *t {
                TopLevel::FuncDef(f) => {
                    fntypes.push(f.typenode.clone());
                    nodes.push(Box::new(f.into()));
                }
                TopLevel::StructDef(s) => {
                    structs.push(s.clone());
                    nodes.push(Box::new(s.into()));
                }
                TopLevel::Common(c) => {
                    nodes.push(c);
                }
                TopLevel::GlobalDef(g) => {
                    globaldefs.push(g.clone());
                    nodes.push(Box::new(g.into()));
                }
                TopLevel::Use(b) => {
                    uses.push(b.clone());
                    nodes.push(b);
                }
            }
            input = i;
        } else if let Err(err) = top {
            let e: Result<
                (LocatedSpan<&str>, LocatedSpan<&str>),
                nom::Err<nom::error::Error<Span>>,
            > = eof(input);
            if e.is_ok() {
                break;
            }
            return Err(err);
        }
    }
    let node: Box<NodeEnum> = Box::new(
        ProgramNode {
            nodes,
            structs,
            fntypes,
            globaldefs,
            range: Range::new(old, input),
            uses,
        }
        .into(),
    );
    Ok((input, node))
}

fn top_level_statement(input: Span) -> IResult<Span, Box<TopLevel>> {
    delspace(alt((
        del_newline_or_space!(function_def),
        del_newline_or_space!(struct_def),
        map_res(
            del_newline_or_space!(semi_statement!(global_variable)),
            |node| {
                Ok::<_, Error>(Box::new(if let NodeEnum::Global(g) = *node {
                    TopLevel::GlobalDef(g)
                } else {
                    TopLevel::Common(node)
                }))
            },
        ),
        map_res(del_newline_or_space!(comment), |c| {
            Ok::<_, Error>(Box::new(TopLevel::Common(c)))
        }),
        map_res(del_newline_or_space!(semi_statement!(use_statement)), |c| {
            Ok::<_, Error>(Box::new(TopLevel::Use(c)))
        }),
        map_res(
            del_newline_or_space!(except(
                "\n\r",
                "failed to parse top level statement",
                ErrorCode::SYNTAX_ERROR_TOP_STATEMENT
            )),
            |e| Ok::<_, Error>(Box::new(TopLevel::Common(e))),
        ),
    )))(input)
}
