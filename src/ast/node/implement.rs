use super::*;
use crate::ast::{ctx::Ctx, tokens::TokenType};
use internal_macro::{comments, fmt, range};
use lsp_types::{DocumentSymbol, SymbolKind};
use rustc_hash::FxHashSet;

#[range]
#[fmt]
#[comments]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ImplNode {
    pub target: Box<TypeNodeEnum>,
    pub methods: Vec<Box<FuncDefNode>>,
    pub impl_trait: Option<(Box<TypeNodeEnum>, (TokenType, Range))>,
}

impl PrintTrait for ImplNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ImplNode");
        self.target.print(tabs + 1, false, line.clone());
        for method in &self.methods {
            method.print(tabs + 1, false, line.clone());
        }
    }
}

impl Node for ImplNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        let mut traittpandrange = None;
        let mut traitfns = FxHashSet::default();
        if let Some((t, (_, r))) = &self.impl_trait {
            let tp = t.get_type(ctx, builder);
            if let Ok(tp) = tp {
                if let PLType::TRAIT(st) = &*tp.borrow() {
                    traittpandrange = Some((tp.clone(), t.range()));
                    for (name, _) in &st.fields {
                        traitfns.insert(name.clone());
                    }
                }
            }
            t.emit_highlight(ctx);
            ctx.push_semantic_token(*r, SemanticTokenType::KEYWORD, 0)
        }
        self.target.emit_highlight(ctx);
        let mut method_docsymbols = vec![];
        let tp = self.target.get_type(ctx, builder)?;
        match &mut *tp.borrow_mut() {
            PLType::STRUCT(sttp) => {
                if let Some((t, _)) = &self.impl_trait {
                    let trait_tp = t.get_type(ctx, builder)?;
                    let name = trait_tp.borrow().get_kind_name();
                    if let PLType::TRAIT(st) = &*trait_tp.borrow_mut() {
                        ctx.send_if_go_to_def(t.range(), st.range, st.path.clone());
                    } else {
                        t.range()
                            .new_err(ErrorCode::EXPECT_TRAIT_TYPE)
                            .add_label(t.range(), Some(("type {}".to_string(), vec![name])))
                            .add_to_ctx(ctx);
                    };
                    // ctx.plmod.add_impl(&sttp, trait_tp);
                }
                ctx.send_if_go_to_def(self.target.range(), sttp.range, sttp.path.clone());
            }
            _ => {
                ctx.add_diag(self.target.range().new_err(ErrorCode::EXPECT_TYPE));
            }
        };

        for method in &mut self.methods {
            let res = method.emit(ctx, builder);
            if res.is_err() {
                continue;
            }
            let (_, pltype, _) = res.unwrap();
            if pltype.is_none() {
                continue;
            }
            let tmp = pltype.unwrap();
            if self.impl_trait.is_some() {
                // 检查方法是否在trait中
                let trait_tp = traittpandrange.clone().unwrap().0;
                if let PLType::TRAIT(st) = &*trait_tp.borrow() {
                    if st
                        .fields
                        .iter()
                        .find(|(_, f)| {
                            let tp = f.typenode.get_type(ctx, builder);
                            if tp.is_err() {
                                return false;
                            }
                            let tp = tp.unwrap();
                            let re = match (&*tp.borrow(), &*tmp.borrow()) {
                                (PLType::FN(f1), PLType::FN(f2)) => {
                                    if f1.eq_except_receiver(f2, ctx, builder) {
                                        traitfns.remove(&f1.name);
                                        true
                                    } else {
                                        false
                                    }
                                }
                                _ => unreachable!(),
                            };
                            re
                        })
                        .is_none()
                    {
                        method
                            .range()
                            .new_err(ErrorCode::METHOD_NOT_IN_TRAIT)
                            .add_label(
                                method.range(),
                                Some(("method {} not in trait {}".to_string(), vec![
                                    method.id.name.split("::").last().unwrap().to_string(),
                                    st.name.clone()
                                ])),
                            )
                            .add_label(
                                st.range,
                                Some(("trait {} def here".to_string(), vec![st.name.clone()])),
                            ).add_help("move this method to another impl block or remove it from current impl block")
                            .add_to_ctx(ctx);
                    }
                };
            }
            let f = if let PLType::FN(f) = &*tmp.borrow() {
                f.get_doc_symbol()
            } else {
                continue;
            };
            method_docsymbols.push(f);
        }
        for f in traitfns {
            let (tp, r) = traittpandrange.clone().unwrap();
            r.new_err(ErrorCode::METHOD_NOT_IN_IMPL)
                .add_label(
                    r,
                    Some((
                        "method {} not in impl block, whitch is required in trait {}".to_string(),
                        vec![f, tp.borrow().get_name()],
                    )),
                )
                .add_label(
                    tp.borrow().get_range().unwrap(),
                    Some((
                        "trait {} def here".to_string(),
                        vec![tp.borrow().get_name()],
                    )),
                )
                .add_help("add the method to current impl block")
                .add_to_ctx(ctx);
        }
        ctx.emit_comment_highlight(&self.comments[0]);
        #[allow(deprecated)]
        let docsymbol = DocumentSymbol {
            name: format!("impl {}", FmtBuilder::generate_node(&self.target)),
            detail: None,
            kind: SymbolKind::OBJECT,
            tags: None,
            deprecated: None,
            range: self.range.to_diag_range(),
            selection_range: self.range.to_diag_range(),
            children: Some(method_docsymbols),
        };
        ctx.plmod.doc_symbols.borrow_mut().push(docsymbol);
        Ok((None, None, TerminatorEnum::NONE))
    }
}
