use nom::{
    branch::alt,
    combinator::{eof, map_res, map},
    IResult,
};

use crate::nomparser::Span;
use crate::{
    ast::node::types::TypedIdentifierNode,
    ast::range::Range,
    ast::{diag::ErrorCode, fmt::FmtBuilder, node::types::PointerTypeNode},
};

use super::{implement::impl_def, macro_parse::macro_parser, union::union_stmt, *};

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
    let mut unions = vec![];
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
                    let generics_from_impl =
                        im.generics.as_mut().map_or(vec![], |g| g.generics.clone());
                    let imname = FmtBuilder::generate_node(&im.target);
                    let target = *im.target.clone();
                    if im.impl_trait.is_some() {
                        trait_impls.push(im.clone());
                    }
                    for mth in im.methods.iter_mut() {
                        if let Some(g) = &mut mth.generics {
                            g.generics.append(&mut generics_from_impl.clone())
                        } else {
                            mth.generics = im.generics.clone();
                            if let Some(g) = &mut mth.generics {
                                g.generics_size = 0;
                            }
                        }
                        mth.id.name = format!("|{}::{}", imname, mth.id.name);
                        mth.is_method = true;
                        mth.paralist.insert(
                            0,
                            Box::new(TypedIdentifierNode {
                                id: VarNode {
                                    name: "self".to_string(),
                                    range: Default::default(),
                                },
                                typenode: Box::new(TypeNodeEnum::Pointer(PointerTypeNode {
                                    elm: Box::new(target.clone()),
                                    range: Default::default(),
                                })),
                                doc: None,
                                range: Default::default(),
                            }),
                        );
                        mth.target_range = im.target.range();
                        mth.impl_trait = im
                            .impl_trait
                            .clone()
                            .map(|(a, b)| (a, b, im.generics.is_some()));
                        fntypes.push(*mth.clone());
                    }
                    nodes.push(Box::new(im.into()));
                }
                TopLevel::TraitDef(tr) => {
                    traits.push(tr.clone());
                    nodes.push(Box::new(tr.into()));
                }
                TopLevel::Union(u) => {
                    if let NodeEnum::UnionDefNode(un) = *u.clone() {
                        unions.push(un.clone());
                    }
                    nodes.push(u);
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
            unions,
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
            del_newline_or_space!(semi_stmt(global_variable, global_variable)),
            |node| {
                Ok::<_, ()>(Box::new(if let NodeEnum::Global(g) = *node {
                    TopLevel::GlobalDef(g)
                } else {
                    TopLevel::Common(node)
                }))
            },
        ),
        map(
            del_newline_or_space!(semi_stmt(global_const, global_const)),
            |node| {
                Box::new( TopLevel::Common(node))
            },
        ),
        map_res(
            del_newline_or_space!(semi_stmt(use_statement, use_statement)),
            |c| Ok::<_, ()>(Box::new(TopLevel::Use(c))),
        ),
        map_res(del_newline_or_space!(comment), |c| {
            Ok::<_, ()>(Box::new(TopLevel::Common(c)))
        }),
        map_res(del_newline_or_space!(trait_def), |c| {
            Ok::<_, ()>(Box::new(TopLevel::TraitDef(*c)))
        }),
        map_res(del_newline_or_space!(union_stmt), |c| {
            Ok::<_, ()>(Box::new(TopLevel::Union(c)))
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
