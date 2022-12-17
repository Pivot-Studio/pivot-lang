use std::path::PathBuf;

use crate::ast::builder::BuilderEnum;
use crate::ast::plmod::get_ns_path_completions;
use crate::{
    ast::{
        ctx::Ctx,
        diag::ErrorCode,
        node::{deal_line, tab},
        pltype::PLType,
    },
    plv,
};
use internal_macro::{fmt, range};
use lsp_types::SemanticTokenType;

use super::PrintTrait;
use super::{primary::VarNode, Node, NodeResult, PLValue, TerminatorEnum};
#[range]
#[fmt]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct UseNode {
    pub ids: Vec<Box<VarNode>>,
    /// 是否完整
    /// use a::b 完整
    /// use a::b:: 不完整
    pub complete: bool,
    pub singlecolon: bool,
}

impl PrintTrait for UseNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("UseNode");
        let mut i = self.ids.len();
        for id in &self.ids {
            i -= 1;
            id.print(tabs + 1, i == 0, line.clone());
        }
    }
}

impl Node for UseNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        _builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        let mut path = PathBuf::from(&ctx.config.root);
        let head = self.ids[0].name.clone();
        if self.ids.len() != 0 {
            // head is project name or deps name
            let dep = ctx.config.deps.as_ref().unwrap().get(&head);
            if head == ctx.config.project || dep.is_some() {
                // change path
                if dep.is_some() {
                    path = path.join(&dep.unwrap().path);
                }
                for i in 1..self.ids.len() {
                    path = path.join(&self.ids[i].name);
                }
            }
        }
        for v in self.ids.iter() {
            ctx.push_semantic_token(v.range, SemanticTokenType::NAMESPACE, 0);
        }
        if !path.with_extension("pi").exists() {
            ctx.add_diag(self.range.new_err(ErrorCode::UNRESOLVED_MODULE));
        }
        ctx.if_completion(self.range, || {
            if self.singlecolon {
                return vec![];
            }
            let mut completions = get_ns_path_completions(path.to_str().unwrap());
            if self.ids.len() < 2 {
                if self.complete {
                    completions.clear();
                    if let Some(deps) = &ctx.config.deps {
                        for (dep, _) in deps {
                            completions.push(lsp_types::CompletionItem {
                                label: dep.clone(),
                                kind: Some(lsp_types::CompletionItemKind::MODULE),
                                ..Default::default()
                            });
                        }
                    }
                    completions.push(lsp_types::CompletionItem {
                        label: ctx.config.project.clone(),
                        kind: Some(lsp_types::CompletionItemKind::MODULE),
                        ..Default::default()
                    });
                }
            }
            completions
        });
        if !self.complete {
            return Err(ctx.add_diag(self.range.new_err(crate::ast::diag::ErrorCode::COMPLETION)));
        }
        Ok((None, None, TerminatorEnum::NONE))
    }
}

/// # ExternIdNode
/// 外部符号节点，可能会退化为内部符号节点（VarNode）
///
/// TODO: 区分该节点与ExternTypeName节点，该节点不生成类型，只生成函数与变量/常量
#[range]
#[fmt]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ExternIdNode {
    pub ns: Vec<Box<VarNode>>,
    pub id: Box<VarNode>,
    pub complete: bool,
    pub singlecolon: bool,
}

impl PrintTrait for ExternIdNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ExternIdNode");
        for id in &self.ns {
            id.print(tabs + 1, false, line.clone());
        }
        self.id.print(tabs + 1, true, line.clone());
    }
}

impl Node for ExternIdNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        if self.ns.is_empty() {
            if self.complete {
                // 如果该节点只有一个id，且完整，那么就是一个普通的包内符号，直接调用idnode
                return self.id.emit(ctx, builder);
            }
            ctx.if_completion(self.range, || {
                // 如果completion请求对应的区域在本节点内
                // 那么将action设成None防止外层节点生成错误的completion
                // a.action = None;

                // 如果是单冒号，不要生成auto complete
                if self.singlecolon {
                    return vec![];
                }
                ctx.get_completions_in_ns(&self.id.name)
                // eprintln!("comp {:?}", completions);
            });
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::COMPLETION)));
        }
        for id in &self.ns {
            ctx.push_semantic_token(id.range, SemanticTokenType::NAMESPACE, 0);
        }
        let mut plmod = &ctx.plmod;
        for ns in self.ns.iter() {
            let re = plmod.submods.get(&ns.name);
            if let Some(re) = re {
                plmod = re;
            } else {
                return Err(ctx.add_diag(ns.range.new_err(ErrorCode::UNRESOLVED_MODULE)));
            }
        }
        if let Some(symbol) = plmod.get_global_symbol(&self.id.name) {
            ctx.push_semantic_token(self.id.range, SemanticTokenType::VARIABLE, 0);
            let pltype = symbol.tp.clone();
            ctx.set_if_refs(symbol.loc.clone(), self.range);
            ctx.send_if_go_to_def(self.range, symbol.range, plmod.path.clone());
            let g = ctx.get_or_add_global(
                &plmod.get_full_name(&self.id.name),
                symbol.tp.clone(),
                builder,
            );
            return Ok((
                Some({
                    let mut res: PLValue = plv!(g);
                    res.set_const(true);
                    res
                }),
                Some(pltype),
                TerminatorEnum::NONE,
            ));
        }
        if let Some(tp) = plmod.get_type(&self.id.name) {
            let range = &tp.clone().borrow().get_range();
            let re = match &*tp.clone().borrow() {
                PLType::FN(_) => {
                    ctx.push_semantic_token(self.id.range, SemanticTokenType::FUNCTION, 0);
                    Ok((None, Some(tp), TerminatorEnum::NONE))
                }
                _ => unreachable!(),
            };
            if let Some(range) = range {
                ctx.send_if_go_to_def(self.range, *range, plmod.path.clone());
            }
            return re;
        }
        Err(ctx.add_diag(self.range.new_err(ErrorCode::SYMBOL_NOT_FOUND)))
    }
}
impl ExternIdNode {
    pub fn get_type<'a, 'ctx>(&'a self, ctx: &Ctx<'a>) -> NodeResult {
        if self.ns.is_empty() {
            if self.complete {
                // 如果该节点只有一个id，且完整，那么就是一个普通的包内符号，直接调用idnode
                return self.id.get_type(ctx);
            }
            ctx.if_completion(self.range, || {
                // 如果completion请求对应的区域在本节点内
                // 那么将action设成None防止外层节点生成错误的completion
                // a.action = None;

                // 如果是单冒号，不要生成auto complete
                if self.singlecolon {
                    return vec![];
                }
                ctx.get_completions_in_ns(&self.id.name)
            });
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::COMPLETION)));
        }
        let mut plmod = &ctx.plmod;
        for ns in self.ns.iter() {
            let re = plmod.submods.get(&ns.name);
            if let Some(re) = re {
                plmod = re;
            } else {
                return Err(ctx.add_diag(ns.range.new_err(ErrorCode::UNRESOLVED_MODULE)));
            }
        }
        if let Some(tp) = plmod.get_type(&self.id.name) {
            let re = match *tp.clone().borrow() {
                PLType::STRUCT(_) => Ok((None, Some(tp), TerminatorEnum::NONE)),
                _ => unreachable!(),
            };
            return re;
        }
        Err(ctx.add_diag(self.range.new_err(ErrorCode::SYMBOL_NOT_FOUND)))
    }
}
