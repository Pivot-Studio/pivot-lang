use std::path::PathBuf;
use std::sync::Arc;

use crate::ast::builder::BuilderEnum;
use crate::ast::diag::PLDiag;
use crate::ast::plmod::get_ns_path_completions;
use crate::ast::range::Range;
use crate::ast::tokens::TokenType;
use crate::ast::{
    ctx::Ctx,
    diag::ErrorCode,
    node::{deal_line, tab},
    pltype::PLType,
};
use internal_macro::node;
use lsp_types::SemanticTokenType;

use super::macro_nodes::MacroNode;
use super::node_result::NodeResultBuilder;
use super::PrintTrait;
use super::{primary::VarNode, Node, NodeResult};
#[node]
pub struct UseNode {
    pub ids: Vec<Box<VarNode>>,
    /// 是否完整
    /// use a::b 完整
    /// use a::b:: 不完整
    pub complete: bool,
    pub singlecolon: bool,
    pub modifier: Option<(TokenType, Range)>,
    pub all_import: bool,
}

impl UseNode {
    pub(crate) fn get_last_id(&self) -> Option<String> {
        self.ids.last().map(|x| x.as_ref().name.clone())
    }
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
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        _builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        #[cfg(target_arch = "wasm32")]
        let mut path = PathBuf::from("");
        #[cfg(not(target_arch = "wasm32"))]
        let mut path = PathBuf::from(&ctx.config.root);
        let head = self.ids[0].get_name(ctx);
        if !self.ids.is_empty() {
            // head is project name or deps name
            let dep = ctx.config.deps.as_ref().and_then(|x| x.get(&head));
            if head == ctx.config.project || dep.is_some() {
                // change path
                if let Some(dep) = dep {
                    path = path.join(&dep.path);
                }
                for i in 1..self.ids.len() {
                    path = path.join(self.ids[i].get_name(ctx));
                }
            }
        }
        if self.ids.len() > 1 {
            for (i, v) in self.ids.iter().enumerate() {
                if i == self.ids.len() - 1 {
                    break;
                }
                ctx.push_semantic_token(v.range, SemanticTokenType::NAMESPACE, 0);
            }
        } else {
            for v in self.ids.iter() {
                ctx.push_semantic_token(v.range, SemanticTokenType::NAMESPACE, 0);
            }
        }
        #[cfg(target_arch = "wasm32")]
        if crate::lsp::wasm::PLLIB_DIR
            .get_file(path.with_extension("pi"))
            .is_some()
        {
            return Ok(Default::default());
        }
        if !path.with_extension("pi").exists() {
            let mut path = path.with_extension("");
            if !path.exists() {
                if path.parent().is_none() {
                    return Err(ctx.add_diag(self.range.new_err(ErrorCode::UNRESOLVED_MODULE)));
                }
                path = path.parent().unwrap().to_path_buf();
            }
            if self.ids.len() > 1 {
                ctx.if_completion(self.range, || {
                    if self.singlecolon {
                        return vec![];
                    }
                    let mut comp = get_ns_path_completions(path.to_str().unwrap());
                    let mod_id = path.file_name().unwrap().to_str().unwrap();
                    if let Some(m) = ctx.plmod.submods.get(mod_id) {
                        comp.extend(m.get_pltp_completions_list())
                    }
                    comp
                });
                let mod_id = path.file_name().unwrap().to_str().unwrap();
                if let Some(m) = ctx.plmod.submods.get(mod_id) {
                    let n = self.ids.last().unwrap();
                    if let Ok(tp) = m.get_type(&n.name, n.range, ctx) {
                        let t = match &*tp.borrow() {
                            PLType::Fn(_) => SemanticTokenType::FUNCTION,
                            PLType::Struct(_) => SemanticTokenType::STRUCT,
                            PLType::Trait(_) => SemanticTokenType::INTERFACE,
                            PLType::Union(_) => SemanticTokenType::ENUM,
                            _ => SemanticTokenType::NAMESPACE,
                        };
                        ctx.push_semantic_token(n.range, t, 0);
                        if !self.complete {
                            return Err(ctx.add_diag(
                                self.range.new_err(crate::ast::diag::ErrorCode::COMPLETION),
                            ));
                        }
                        if !tp.borrow().is_pub() {
                            return Err(ctx.add_diag(
                                n.range
                                    .new_err(crate::ast::diag::ErrorCode::EXPECT_PUBLIC_SYMBOL),
                            ));
                        }
                        return Ok(Default::default());
                    } else if let Some(mac) = m.macros.get(&n.name) {
                        ctx.push_semantic_token(n.range, SemanticTokenType::MACRO, 0);
                        ctx.send_if_go_to_def(n.range, mac.range, mac.file.clone());
                        ctx.set_glob_refs(&format!("{}..{}", &mac.file, &mac.id.name), n.range);
                        if !self.complete {
                            return Err(ctx.add_diag(
                                self.range.new_err(crate::ast::diag::ErrorCode::COMPLETION),
                            ));
                        }
                        return Ok(Default::default());
                    } else {
                        ctx.push_semantic_token(n.range, SemanticTokenType::NAMESPACE, 0);
                    }
                }
            }
            ctx.add_diag(self.range.new_err(ErrorCode::UNRESOLVED_MODULE));
        }
        if self.ids.len() > 1 {
            let last = self.ids.last().unwrap();
            ctx.push_semantic_token(last.range, SemanticTokenType::NAMESPACE, 0);
            if let Some(m) = ctx.plmod.submods.get(&last.name) {
                ctx.send_if_go_to_def(last.range, Default::default(), m.path.to_owned());
            }
        }
        ctx.if_completion(self.range, || {
            if self.singlecolon {
                return vec![];
            }
            let mut completions = get_ns_path_completions(path.to_str().unwrap());
            if self.ids.len() < 2 && self.complete {
                completions.clear();
                if let Some(deps) = &ctx.config.deps {
                    for dep in deps.keys() {
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
            completions
        });
        if !self.complete {
            return Err(ctx.add_diag(self.range.new_err(crate::ast::diag::ErrorCode::COMPLETION)));
        }
        Ok(Default::default())
    }
}

/// # ExternIdNode
/// 外部符号节点，可能会退化为内部符号节点（VarNode）
///
/// TODO: 区分该节点与ExternTypeName节点，该节点不生成类型，只生成函数与变量/常量
#[node]
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
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
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
                ctx.get_completions_in_ns(&self.id.get_name(ctx))
                // eprintln!("comp {:?}", completions);
            });
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::COMPLETION)));
        } else {
            ctx.if_completion(self.range, || {
                ctx.get_completions_in_ns(&self.ns[0].get_name(ctx))
            });
        }
        for id in &self.ns {
            ctx.push_semantic_token(id.range, SemanticTokenType::NAMESPACE, 0);
        }
        let mut plmod = &ctx.plmod;
        for ns in self.ns.iter() {
            let re = plmod.submods.get(&ns.get_name(ctx));
            if let Some(re) = re {
                plmod = re;
            } else {
                return Err(ctx.add_diag(ns.range.new_err(ErrorCode::UNRESOLVED_MODULE)));
            }
        }
        if let Some(symbol) = plmod.get_global_symbol(&self.id.get_name(ctx)) {
            ctx.push_semantic_token(self.id.range, SemanticTokenType::VARIABLE, 0);
            let pltype = symbol.tp.clone();
            ctx.set_glob_refs(&plmod.get_full_name(&self.id.get_name(ctx)), self.id.range);
            ctx.send_if_go_to_def(self.range, symbol.range, plmod.path.clone());
            let g = ctx.get_or_add_global(
                &plmod.get_full_name(&self.id.get_name(ctx)),
                symbol.tp.clone(),
                builder,
            );
            return g.new_output(pltype).set_const().to_result();
        }
        if let Ok(tp) = plmod.get_type(&self.id.get_name(ctx), self.range, ctx) {
            let mtp = tp.clone();
            let re = match &*mtp.borrow() {
                PLType::Fn(_) => {
                    // 必须是public的
                    _ = tp.expect_pub(ctx, self.range);
                    ctx.push_semantic_token(self.id.range, SemanticTokenType::FUNCTION, 0);
                    usize::MAX.new_output(tp.tp).to_result()
                }
                _ => return Err(ctx.add_diag(self.range.new_err(ErrorCode::COMPLETION))),
            };
            return re;
        }
        Err(ctx.add_diag(self.range.new_err(ErrorCode::SYMBOL_NOT_FOUND)))
    }
}
impl ExternIdNode {
    pub fn get_type(&self, ctx: &Ctx) -> NodeResult {
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
                ctx.get_completions_in_ns(&self.id.get_name(ctx))
            });
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::COMPLETION)));
        } else {
            ctx.if_completion(self.range, || {
                ctx.get_completions_in_ns(&self.ns[0].get_name(ctx))
            });
        }
        let mut plmod = &ctx.plmod;
        for ns in self.ns.iter() {
            let re = plmod.submods.get(&ns.get_name(ctx));
            if let Some(re) = re {
                plmod = re;
            } else {
                return Err(ctx.add_diag(ns.range.new_err(ErrorCode::UNRESOLVED_MODULE)));
            }
        }

        if let Ok(tp) = plmod.get_type(&self.id.get_name(ctx), self.range, ctx) {
            // 必须是public的
            _ = tp.expect_pub(ctx, self.range);
            let re = match *tp.clone().borrow() {
                PLType::Struct(_) | PLType::Trait(_) => usize::MAX.new_output(tp.tp).to_result(),
                _ => unreachable!(),
            };
            return re;
        }
        Err(ctx.add_diag(self.range.new_err(ErrorCode::SYMBOL_NOT_FOUND)))
    }

    pub fn get_macro(&self, ctx: &Ctx) -> Result<Arc<MacroNode>, PLDiag> {
        if self.ns.is_empty() {
            // 如果该节点只有一个id，且完整，那么就是一个普通的包内符号，直接调用idnode
            if let Some(m) = ctx.get_macro(&self.id.get_name(ctx)) {
                return Ok(m);
            }
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::MACRO_NOT_FOUND)));
        }
        let mut plmod = &ctx.plmod;
        for ns in self.ns.iter() {
            let re = plmod.submods.get(&ns.get_name(ctx));
            if let Some(re) = re {
                plmod = re;
            } else {
                return Err(ctx.add_diag(ns.range.new_err(ErrorCode::UNRESOLVED_MODULE)));
            }
        }
        if let Some(m) = plmod.macros.get(&self.id.get_name(ctx)) {
            return Ok(m.clone());
        }
        Err(ctx.add_diag(self.range.new_err(ErrorCode::MACRO_NOT_FOUND)))
    }
}
