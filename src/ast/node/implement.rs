use super::*;
use crate::{
    ast::{ctx::Ctx, tokens::TokenType, traits::CustomType},
    format_label,
};
use indexmap::IndexMap;
use internal_macro::node;
use lsp_types::{DocumentSymbol, SymbolKind};
use rustc_hash::FxHashMap;
use ustr::Ustr;

#[node(comment)]
pub struct ImplNode {
    /// generics stands for the available generics types in the implementation scope,
    /// which could be accessed by all methods in the block, even it might have no generic types.
    /// for example, the generics in the following implementation is the `T` after keyword impl.
    /// ```rust
    /// struct A<T> { t: T  }
    /// trait Demo<T>{
    ///     fn a(self, t:T);
    ///     fn b<F>(self,t:T, f:F);
    /// }
    /// impl<T> Demo<T> for A<T> {
    ///     fn a(self,t:T) {
    ///         todo!()
    ///     }
    ///     fn b<F>(self,t:T, f:F) {
    ///         todo!()
    ///     }
    /// }
    /// ```
    /// All methods and structures inside the block could access the generic type T. Method `b` could access it without declaring T.
    pub generics: Option<Box<GenericDefNode>>,

    /// target is the structure which implements the methods in impl_trait
    pub target: Box<TypeNodeEnum>,

    /// methods holds the definitions of the implemented methods
    pub methods: Vec<Box<FuncDefNode>>,

    /// impl_trait is the trait to implement for the target structure
    pub impl_trait: Option<(Box<TypeNodeEnum>, (TokenType, Range))>,
}

impl ImplNode {
    pub fn add_impl_to_ctx<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> Result<(), PLDiag> {
        if self.generics.is_some() {
            let gm = self.generics.as_ref().unwrap().gen_generic_type(ctx);
            _ = ctx.protect_generic_context(&gm, |ctx| {
                self.generics
                    .as_ref()
                    .unwrap()
                    .set_traits(ctx, &gm)
                    .unwrap();
                let sttp = self.target.get_type(ctx, builder, false)?;
                let trait_tp = self
                    .impl_trait
                    .as_ref()
                    .unwrap()
                    .0
                    .get_type(ctx, builder, false)?;
                if let PLType::Trait(t) = &*trait_tp.borrow() {
                    ctx.plmod.add_impl(
                        &sttp.borrow().get_full_elm_name_without_generic(),
                        &trait_tp.borrow().get_full_elm_name(),
                        t.generic_map.clone(),
                    );
                }
                Ok::<_, PLDiag>(())
            });
        } else {
            let sttp = self.target.get_type(ctx, builder, false)?;
            let trait_tp = self
                .impl_trait
                .as_ref()
                .unwrap()
                .0
                .get_type(ctx, builder, false)?;
            if let PLType::Trait(t) = &*trait_tp.borrow() {
                ctx.plmod.add_impl(
                    &sttp.borrow().get_full_elm_name_without_generic(),
                    &trait_tp.borrow().get_full_elm_name(),
                    t.generic_map.clone(),
                );
            };
        }
        Ok(())
    }
}

impl PrintTrait for ImplNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ImplNode");
        self.target
            .print(tabs + 1, self.methods.is_empty(), line.clone());
        for (i, method) in self.methods.iter().enumerate() {
            method.print(tabs + 1, i == self.methods.len() - 1, line.clone());
        }
    }
}
fn check_fn<'a, 'b>(
    ctx: &'b mut Ctx<'a>,
    builder: &'b BuilderEnum<'a, '_>,
    method: &FuncDefNode,
    trait_tp: Arc<RefCell<PLType>>,
    traitfns: &mut FxHashMap<Ustr, bool>,
    fntype: Arc<RefCell<PLType>>,
) -> Result<(), PLDiag> {
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
    if let PLType::Trait(st) = &*trait_tp.borrow() {
        ctx.run_in_type_mod(st, |ctx,st| {
            if !st.fields.iter().any(|(_, f)| {
                let tp = f.typenode.get_type(ctx, builder, true).unwrap();
                let re = match (&*tp.borrow(), &*fntype.borrow()) {
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
                        format_label!(
                            "method {} not in trait {}",
                            method.id.name.split("::").last().unwrap(),
                            st.name
                        ),
                    )
                    .add_label(
                        st.range,
                        st.get_path(),
                        format_label!("trait {} def here", st.name),
                    )
                    .add_help(
                        "move this method to another impl block or remove it from current impl block",
                    )
                    .add_to_ctx(ctx);
            }
        });
        return Ok(());
    }
    unreachable!()
}
impl Node for ImplNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        if let Some(generics) = &self.generics {
            generics.emit_highlight(ctx);
        }
        let gm = self
            .generics
            .as_ref()
            .map(|e| e.gen_generic_type(ctx))
            .unwrap_or_default();
        ctx.protect_generic_context(&gm, |ctx| {
            if let Some(g) = self.generics.as_ref() {
                g.set_traits(ctx, &gm)?;
            }
            let mut traittpandrange = None;
            let mut traitfns = FxHashMap::default();
            if let Some((typename, _)) = &self.impl_trait {
                typename.emit_highlight(ctx);
                let trait_tp = typename.get_type(ctx, builder, true)?;
                if let PLType::Trait(st) = &*trait_tp.borrow() {
                    ctx.send_if_go_to_def(typename.range(), st.range, st.path);
                    traittpandrange = Some((trait_tp.clone(), typename.range()));
                    for name in st.fields.keys() {
                        traitfns.insert(
                            *name,
                            st.modifier.map(|m| m.0 == TokenType::PUB).unwrap_or(false),
                        );
                    }
                } else {
                    return Err(typename
                        .range()
                        .new_err(ErrorCode::EXPECT_TRAIT_TYPE)
                        .add_label(
                            typename.range(),
                            ctx.get_file(),
                            format_label!("type {}", trait_tp.borrow().get_kind_name()),
                        )
                        .add_to_ctx(ctx));
                };
            };
            self.target.emit_highlight(ctx);
            let mut method_docsymbols = vec![];
            if let TypeNodeEnum::Basic(bt) = &*self.target {
                if bt.id.is_none() {
                    ctx.generate_completion_if(ctx.should_gen(bt.range), || {
                        ctx.get_type_completions()
                    });
                    return Err(ctx.add_diag(bt.range.new_err(ErrorCode::EXPECT_TYPE)));
                }
                let v = bt.id.as_ref().unwrap().get_type(ctx)?.get_value();
                let st_pltype = v.unwrap().get_ty();
                // ctx.set_self_type(st_pltype.clone());
                if let PLType::Struct(sttp) = &*st_pltype.borrow() {
                    if let Some((trait_tp, r)) = &traittpandrange {
                        if let PLType::Trait(trait_tp) = &*trait_tp.borrow() {
                            trait_tp.check_impl_derives(ctx, sttp, *r);
                        } else {
                            unreachable!()
                        }
                    }
                    ctx.send_if_go_to_def(self.target.range(), sttp.range, sttp.path);
                };
            }
            for method in &mut self.methods {
                if let Ok(res) = method.emit(ctx, builder) {
                    if let Some(node_val) = res.get_value() {
                        let fntype = node_val.get_ty();
                        let f = if let PLType::Fn(f) = &*fntype.borrow() {
                            f.get_doc_symbol()
                        } else {
                            unreachable!()
                        };
                        method_docsymbols.push(f);
                        if let Some((trait_tp, _)) = &traittpandrange {
                            if let PLType::Trait(t) = &*trait_tp.clone().borrow() {
                                ctx.protect_generic_context(&t.generic_map, |ctx| {
                                    let generic_map = if let Some(generics) = &method.generics {
                                        generics.gen_generic_type(ctx)
                                    } else {
                                        IndexMap::default()
                                    };
                                    ctx.protect_generic_context(&generic_map, |ctx| {
                                        if let Some(generics) = &method.generics {
                                            generics.set_traits(ctx, &generic_map)?;
                                        }
                                        check_fn(
                                            ctx,
                                            builder,
                                            method,
                                            trait_tp.clone(),
                                            &mut traitfns,
                                            fntype.clone(),
                                        )?;
                                        Ok(())
                                    })
                                })?;
                            }
                        }
                    }
                }
            }
            for (f, _) in traitfns {
                let (tp, r) = traittpandrange.clone().unwrap();
                r.new_err(ErrorCode::METHOD_NOT_IN_IMPL)
                    .add_label(
                        r,
                        ctx.get_file(),
                        format_label!(
                            "method {} not in impl block, which is required in trait {}",
                            f,
                            tp.borrow().get_name()
                        ),
                    )
                    .add_label(
                        tp.borrow().get_range().unwrap(),
                        tp.borrow().get_path().unwrap_or(ctx.get_file()),
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
            Ok(Default::default())
        })
    }
}
