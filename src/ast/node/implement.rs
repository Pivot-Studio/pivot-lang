use super::*;
use crate::{
    ast::{ctx::Ctx, tokens::TokenType},
    format_label,
};
use internal_macro::node;
use lsp_types::{DocumentSymbol, SymbolKind};
use rustc_hash::FxHashSet;

#[node(comment)]
pub struct ImplNode {
    pub generics: Option<Box<GenericDefNode>>,
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
        if let Some(generics) = &self.generics {
            generics.emit_highlight(ctx);
        }
        let mut traittpandrange = None;
        let mut traitfns = FxHashSet::default();
        if let Some((t, (_, r))) = &self.impl_trait {
            let tp = t.get_type(ctx, builder);
            if let Ok(tp) = tp {
                if let PLType::Trait(st) = &*tp.borrow() {
                    traittpandrange = Some((tp.clone(), t.range()));
                    for name in st.fields.keys() {
                        traitfns.insert(name.clone());
                    }
                }
            }
            t.emit_highlight(ctx);
            ctx.push_semantic_token(*r, SemanticTokenType::KEYWORD, 0)
        }
        self.target.emit_highlight(ctx);
        let mut method_docsymbols = vec![];
        if let TypeNodeEnum::Pointer(pt) = &*self.target {
            if let TypeNodeEnum::Basic(bt) = &*pt.elm {
                let st_pltype = bt.get_origin_type_with_infer(ctx, builder)?;
                if let PLType::Struct(sttp) = &*st_pltype.borrow() {
                    if let Some((t, _)) = &self.impl_trait {
                        let trait_tp = t.get_type(ctx, builder)?;
                        let name = trait_tp.borrow().get_kind_name();
                        if let PLType::Trait(st) = &*trait_tp.borrow_mut() {
                            ctx.send_if_go_to_def(t.range(), st.range, st.path.clone());
                        } else {
                            t.range()
                                .new_err(ErrorCode::EXPECT_TRAIT_TYPE)
                                .add_label(
                                    t.range(),
                                    ctx.get_file(),
                                    format_label!("type {}", name),
                                ) //Some(("type {}".to_string(), vec![name])))
                                .add_to_ctx(ctx);
                        };
                    }
                    ctx.send_if_go_to_def(self.target.range(), sttp.range, sttp.path.clone());
                };
            }
        }
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
                // 检查是否有modifier
                if let Some((m, r)) = method.modifier {
                    r.new_err(ErrorCode::TRAIT_METHOD_SHALL_NOT_HAVE_MODIFIER)
                        .add_label(
                            r,
                            ctx.get_file(),
                            format_label!("modifier {} shall be removed", m.get_str()),
                        )
                        .add_help(
                            "trait methods share the same modifier with \
                            trait, so you shall not add modifier here",
                        )
                        .add_to_ctx(ctx);
                }

                // 检查方法是否在trait中
                let trait_tp = traittpandrange.clone().unwrap().0;
                if let PLType::Trait(st) = &*trait_tp.borrow() {
                    if !st.fields.iter().any(|(_, f)| {
                        let tp = f.typenode.get_type(ctx, builder);
                        if tp.is_err() {
                            return false;
                        }
                        let tp = tp.unwrap();
                        let re = match (&*tp.borrow(), &*tmp.borrow()) {
                            (PLType::Fn(f1), PLType::Fn(f2)) => {
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
                    }) {
                        method
                            .range()
                            .new_err(ErrorCode::METHOD_NOT_IN_TRAIT)
                            .add_label(
                                method.range(),
                                ctx.get_file(),
                                format_label!("method {} not in trait {}",
                                    method.id.name.split("::").last().unwrap(),
                                    &st.name
                                ),
                            )
                            .add_label(
                                st.range,
                                ctx.get_file(),
                                format_label!("trait {} def here", &st.name),
                            ).add_help("move this method to another impl block or remove it from current impl block")
                            .add_to_ctx(ctx);
                    }
                };
            }
            let f = if let PLType::Fn(f) = &*tmp.borrow() {
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
                    ctx.get_file(),
                    format_label!(
                        "method {} not in impl block, whitch is required in trait {}",
                        f,
                        tp.borrow().get_name()
                    ),
                )
                .add_label(
                    tp.borrow().get_range().unwrap(),
                    ctx.get_file(),
                    format_label!("trait {} def here", tp.borrow().get_name()),
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
        Ok((None, None, TerminatorEnum::None))
    }
}
