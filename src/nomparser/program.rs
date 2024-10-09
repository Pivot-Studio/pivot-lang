use nom::{
    branch::alt,
    combinator::{eof, map, map_res},
    IResult,
};
use ret::RetNode;

use crate::{
    ast::node::types::TypedIdentifierNode,
    ast::range::Range,
    ast::{diag::ErrorCode, fmt::FmtBuilder, node::types::PointerTypeNode},
};
use crate::{
    ast::{
        node::{function::FuncCallNode, pkg::ExternIdNode},
        pltype::{PLType, PriType},
    },
    nomparser::Span,
};

use super::{implement::impl_def, macro_parse::macro_parser, union::union_stmt, *};

/// # program
///
/// `program` uses [top_level_statement] to consume the input span continuously until all inputs are consumed,
/// or the process of consuming raises an error. If an error raises on the fly, it stops consuming and return the error
/// wrapped by [Err].
///
/// After finishing consuming, all top level statements are classified based on their catagories.
/// It returns a [ProgramNode] which entails all top level nodes.
pub fn program(input: Span) -> IResult<Span, Box<NodeEnum>> {
    let old = input.clone();
    let mut input = input.clone();
    let mut nodes = vec![];
    let mut structs = vec![];
    let mut fntypes = vec![];
    let mut globaldefs = vec![];
    let mut uses = vec![];
    let mut traits = vec![];
    let mut trait_impls = vec![];
    let mut unions = vec![];
    loop {
        let top = top_level_statement(input.clone());
        if let Ok((i, t)) = top {
            match *t {
                TopLevel::FuncType(f) => {
                    // if it's async main, we need to generate a new main function wrapping the async main
                    if f.id.name == "main" && f.generator_ty.is_async() {
                        let mut new_f = f.clone();
                        new_f.id.name = "__main_async".into();
                        let call_main_async = FuncCallNode {
                            generic_params: None,
                            callee: Box::new(NodeEnum::ExternIdNode(ExternIdNode {
                                namespace: vec![],
                                id: Box::new(VarNode {
                                    name: "__main_async".into(),
                                    range: Default::default(),
                                    id: None,
                                }),
                                complete: true,
                                singlecolon: false,
                                range: Default::default(),
                            })),
                            paralist: vec![],
                            generic_infer: None,
                            comments: vec![vec![]],
                            range: Default::default(),
                        };
                        let call_spawn_async_main = FuncCallNode {
                            generic_params: None,
                            callee: Box::new(NodeEnum::ExternIdNode(ExternIdNode {
                                namespace: vec![],
                                id: Box::new(VarNode {
                                    name: "spawn_async_main".into(),
                                    range: Default::default(),
                                    id: None,
                                }),
                                complete: true,
                                singlecolon: false,
                                range: Default::default(),
                            })),
                            paralist: vec![Box::new(NodeEnum::FuncCall(call_main_async))],
                            generic_infer: None,
                            comments: vec![vec![]],
                            range: Default::default(),
                        };
                        let ret_zero = RetNode {
                            value: Some(Box::new(NodeEnum::Num(NumNode {
                                value: Num::Int(0),
                                range: Default::default(),
                            }))),
                            yield_identifier: None,
                            comments: vec![vec![]],
                            range: Default::default(),
                        };
                        let real_main_def = FuncDefNode {
                            id: Box::new(VarNode {
                                name: "main".into(),
                                range: Default::default(),
                                id: None,
                            }),
                            paralist: vec![],
                            ret: PLType::Primitive(PriType::I64).get_typenode(&"".into()),
                            doc: Default::default(),
                            pre_comments: Default::default(),
                            is_declaration_only: false,
                            generics: Default::default(),
                            body: Some(StatementsNode {
                                statements: vec![
                                    Box::new(NodeEnum::FuncCall(call_spawn_async_main)),
                                    Box::new(NodeEnum::Ret(ret_zero)),
                                ],
                                range: Default::default(),
                            }),
                            modifier: None,
                            generics_size: 0,
                            trait_bounds: None,
                            impl_trait: None,
                            is_method: false,
                            in_trait_def: false,
                            target_range: Default::default(),
                            generator_ty: crate::ast::node::function::GeneratorType::None,
                            range: Default::default(),
                        };
                        fntypes.push(real_main_def.clone());
                        nodes.push(Box::new(real_main_def.into()));
                        fntypes.push(new_f.clone());
                        nodes.push(Box::new(new_f.into()));
                    } else {
                        fntypes.push(f.clone());
                        nodes.push(Box::new(f.into()));
                    }
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
                        mth.id.name = format!("|{}::{}", imname, mth.id.name).into();
                        mth.is_method = true;
                        let r = mth.range.start_point();
                        mth.paralist.insert(
                            0,
                            Box::new(TypedIdentifierNode {
                                id: VarNode {
                                    name: "self".into(),
                                    range: r,
                                    id: None,
                                },
                                typenode: Box::new(TypeNodeEnum::Pointer(PointerTypeNode {
                                    elm: Box::new(target.clone()),
                                    range: r,
                                })),
                                doc: None,
                                range: r,
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
            let e: Result<(Span, Span), nom::Err<nom::error::Error<Span>>> = eof(input.clone());
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
            range: Range::new(&old, &input),
            uses,
            traits,
            trait_impls,
            unions,
        }
        .into(),
    );
    Ok((input, node))
}

/// # top_level_statement
///
/// `top_level_statement` consumes the input span with one top-level statement.
/// It returns the first top level statemnt and the left span for further consuming.
/// When an error is encountered, the error will be wrapped in the `TopLevel` format to inspect in the furture.
///
/// The top level statements are consisted by the following kinds of statement:
/// 1. function definition
/// 2. structure definition
/// 3. implementation definition
/// 4. macro
/// 5. global variable declaration
/// 6. global constant declaration
/// 7. use statement to import library
/// 8. comments
/// 9. trait definition
/// 10. union definition
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
            |node| Box::new(TopLevel::Common(node)),
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
