use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{eof, map_res, recognize},
    multi::many0,
    sequence::{preceded, terminated, tuple},
    IResult,
};

use crate::nomparser::Span;
use crate::{
    ast::range::Range,
    ast::{
        diag::ErrorCode,
        fmt::FmtBuilder,
        node::{
            error::{ErrorNode, StErrorNode},
            types::PointerTypeNode,
        },
    },
    ast::{node::types::TypedIdentifierNode, tokens::TokenType},
};

use super::{implement::impl_def, macro_parse::macro_parser, *};

pub fn program(input: Span) -> IResult<Span, Box<NodeEnum>> {
    let old = input;
    let mut input = input;
    let mut nodes = vec![];
    let mut structs = vec![];
    let mut fntypes = vec![];
    let mut globaldefs = vec![];
    let mut uses = vec![];
    let mut traits = vec![];
    let mut trait_impls = vec![];
    loop {
        let top = top_level_statement(input);
        if let Ok((i, t)) = top {
            match *t {
                TopLevel::FuncType(f) => {
                    fntypes.push(f.clone());
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
                TopLevel::ImplDef(mut im) => {
                    let imname = FmtBuilder::generate_node(&im.target);
                    let target = *im.target.clone();
                    if let Some((t, _)) = &im.impl_trait {
                        let trait_name = FmtBuilder::generate_node(&t);
                        trait_impls.push((imname.clone(), trait_name));
                    }
                    for mth in im.methods.iter_mut() {
                        mth.id.name = format!("|{}::{}", imname, mth.id.name);
                        mth.paralist.insert(
                            0,
                            Box::new(TypedIdentifierNode {
                                id: VarNode {
                                    name: "self".to_string(),
                                    range: Default::default(),
                                },
                                typenode: Box::new(TypeNodeEnum::PointerTypeNode(
                                    PointerTypeNode {
                                        elm: Box::new(target.clone()),
                                        range: Default::default(),
                                    },
                                )),
                                doc: None,
                                range: Default::default(),
                            }),
                        );
                        fntypes.push(*mth.clone());
                    }
                    nodes.push(Box::new(im.into()));
                }
                TopLevel::TraitDef(tr) => {
                    traits.push(tr.clone());
                    nodes.push(Box::new(tr.into()));
                }
            }
            input = i;
        } else if let Err(err) = top {
            let e: Result<(Span, Span), nom::Err<nom::error::Error<Span>>> = eof(input);
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
            traits,
            trait_impls,
        }
        .into(),
    );
    Ok((input, node))
}

fn top_level_statement(input: Span) -> IResult<Span, Box<TopLevel>> {
    delspace(alt((
        del_newline_or_space!(function_def),
        del_newline_or_space!(struct_def),
        del_newline_or_space!(impl_def),
        del_newline_or_space!(macro_parser),
        map_res(
            del_newline_or_space!(semi_statement!(global_variable)),
            |node| {
                Ok::<_, ()>(Box::new(if let NodeEnum::Global(g) = *node {
                    TopLevel::GlobalDef(g)
                } else {
                    TopLevel::Common(node)
                }))
            },
        ),
        map_res(del_newline_or_space!(semi_statement!(use_statement)), |c| {
            Ok::<_, ()>(Box::new(TopLevel::Use(c)))
        }),
        map_res(del_newline_or_space!(comment), |c| {
            Ok::<_, ()>(Box::new(TopLevel::Common(c)))
        }),
        map_res(del_newline_or_space!(trait_def), |c| {
            Ok::<_, ()>(Box::new(TopLevel::TraitDef(*c)))
        }),
        map_res(
            del_newline_or_space!(except(
                "\n\r",
                "failed to parse top level statement",
                ErrorCode::SYNTAX_ERROR_TOP_STATEMENT
            )),
            |e| Ok::<_, ()>(Box::new(TopLevel::Common(e))),
        ),
    )))(input)
}
