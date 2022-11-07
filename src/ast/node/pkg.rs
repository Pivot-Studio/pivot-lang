use std::path::PathBuf;

use internal_macro::range;
use lsp_types::SemanticTokenType;

use crate::{
    ast::{
        ctx::{get_ns_path_completions, Ctx, PLType},
        diag::ErrorCode,
        node::{deal_line, tab},
    },
    utils::read_config::enter,
};

use super::{primary::VarNode, Node, NodeResult, PLValue, TerminatorEnum};

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct UseNode {
    pub ids: Vec<Box<VarNode>>,
    /// 是否完整
    /// use a::b 完整
    /// use a::b:: 不完整
    pub complete: bool,
    pub singlecolon: bool,
}

impl Node for UseNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        let mut format_res = String::from("use ");
        for (i, id) in self.ids.iter().enumerate() {
            format_res.push_str(&id.format(tabs, prefix));
            if i != self.ids.len() - 1 {
                format_res.push_str("::");
            }
        }
        format_res.push_str(";");
        format_res.push_str(enter());
        format_res
    }
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

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
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
            ctx.add_err(self.range, crate::ast::diag::ErrorCode::UNRESOLVED_MODULE);
        }
        ctx.if_completion(|a, (pos, _)| {
            if pos.is_in(self.range) {
                a.action = None;
                if self.singlecolon {
                    return;
                }
                let mut completions = get_ns_path_completions(path.to_str().unwrap());
                if self.ids.len() < 2 {
                    if self.complete {
                        completions.clear();
                        if let Some(deps) = &a.config.deps {
                            for (dep, _) in deps {
                                completions.push(lsp_types::CompletionItem {
                                    label: dep.clone(),
                                    kind: Some(lsp_types::CompletionItemKind::MODULE),
                                    ..Default::default()
                                });
                            }
                        }
                        completions.push(lsp_types::CompletionItem {
                            label: a.config.project.clone(),
                            kind: Some(lsp_types::CompletionItemKind::MODULE),
                            ..Default::default()
                        });
                    }
                }
                a.completion_items.set(completions);
            }
        });
        if !self.complete {
            return Err(ctx.add_err(self.range, crate::ast::diag::ErrorCode::COMPLETION));
        }
        Ok((None, None, TerminatorEnum::NONE))
    }
}

/// # ExternIDNode
/// 外部符号节点，可能会退化为内部符号节点（VarNode）
///
/// TODO: 区分该节点与ExternTypeName节点，该节点不生成类型，只生成函数与变量/常量
#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ExternIDNode {
    pub ns: Vec<Box<VarNode>>,
    pub id: Box<VarNode>,
    pub complete: bool,
    pub singlecolon: bool,
}

impl Node for ExternIDNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        let mut format_res = String::new();
        for (i, id) in self.ns.iter().enumerate() {
            format_res.push_str(&id.format(tabs, prefix));
            if i != self.ns.len() {
                format_res.push_str("::");
            }
        }
        format_res.push_str(&self.id.format(tabs, prefix));
        format_res
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ExternIDNode");
        for id in &self.ns {
            id.print(tabs + 1, false, line.clone());
        }
        self.id.print(tabs + 1, true, line.clone());
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        self.get_type(ctx)
    }
}
impl ExternIDNode {
    pub fn replace_type<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>, tp: PLType) {
        if self.ns.is_empty() {
            return self.id.replace_type(ctx, tp);
        }
        let mut plmod = &mut ctx.plmod;
        for ns in self.ns.iter() {
            let re = plmod.submods.get_mut(&ns.name);
            if let Some(re) = re {
                plmod = re;
            } else {
                unreachable!()
            }
        }
        plmod.replace_type(&self.id.name, tp)
    }
    pub fn get_type<'a, 'ctx>(&'a self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        if self.ns.is_empty() {
            if self.complete {
                // 如果该节点只有一个id，且完整，那么就是一个普通的包内符号，直接调用idnode
                return self.id.get_type(ctx);
            }
            ctx.if_completion(|a, (pos, _)| {
                if pos.is_in(self.range) {
                    // 如果completion请求对应的区域在本节点内
                    // 那么将action设成None防止外层节点生成错误的completion
                    a.action = None;
                    // 如果是单冒号，不要生成auto complete
                    if self.singlecolon {
                        return;
                    }
                    let completions = a.get_completions_in_ns(&self.id.name);
                    // eprintln!("comp {:?}", completions);
                    a.completion_items.set(completions);
                }
            });
            return Err(ctx.add_err(self.range, ErrorCode::COMPLETION));
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
                return Err(ctx.add_err(ns.range, ErrorCode::UNRESOLVED_MODULE));
            }
        }
        if let Some(symbol) = plmod.get_global_symbol(&self.id.name) {
            ctx.push_semantic_token(self.id.range, SemanticTokenType::VARIABLE, 0);
            let g = ctx.get_or_add_global(&self.id.name, &plmod, symbol.tp.clone());
            let pltype = symbol.tp.clone();
            ctx.set_if_refs(symbol.loc.clone(), self.range);
            ctx.send_if_go_to_def(self.range, symbol.range, plmod.path.clone());
            return Ok((
                Some({
                    let mut res: PLValue = g.into();
                    res.set_const(true);
                    res
                }),
                Some(pltype),
                TerminatorEnum::NONE,
            ));
        }
        if let Some(tp) = plmod.get_type(&self.id.name) {
            ctx.set_if_refs_tp(&tp, self.range);
            let range = &tp.get_range();
            let re = match &tp {
                PLType::FN(f) => {
                    ctx.push_semantic_token(self.id.range, SemanticTokenType::FUNCTION, 0);
                    Ok((
                        Some(f.get_or_insert_fn(ctx).into()),
                        Some(tp),
                        TerminatorEnum::NONE,
                    ))
                }
                PLType::STRUCT(_) => {
                    ctx.push_semantic_token(self.id.range, SemanticTokenType::STRUCT, 0);
                    Ok((None, Some(tp), TerminatorEnum::NONE))
                }
                _ => unreachable!(),
            };
            if let Some(range) = range {
                ctx.send_if_go_to_def(self.range, *range, plmod.path.clone());
            }
            return re;
        }
        Err(ctx.add_err(self.range, ErrorCode::SYMBOL_NOT_FOUND))
    }
}
