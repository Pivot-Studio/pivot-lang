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


impl ImplNode {
    pub fn add_impl_to_ctx<'a, 'b, 'ctx>(&self,ctx: &'b mut Ctx<'a>,builder: &'b BuilderEnum<'a, 'ctx>)->Result<(),PLDiag> {
        if self.generics.is_some() {
            let gm = self.generics.as_ref().unwrap().gen_generic_type(ctx);
            _ = ctx.protect_generic_context(&gm, |ctx|{
                let sttp = self.target.get_type(ctx, builder,true)?;
                let trait_tp =self.impl_trait.as_ref().unwrap().0.get_type(ctx, builder,false)?;
                ctx.plmod.add_impl(&sttp.borrow().get_full_elm_name_without_generic(), 
                &trait_tp.borrow().get_full_elm_name());
                Ok(())
            });
        }else {
            let sttp = self.target.get_type(ctx, builder,true)?;
            let trait_tp =self.impl_trait.as_ref().unwrap().0.get_type(ctx, builder,false)?;
            let st_name = sttp.borrow().get_full_elm_name();
            let trait_name = trait_tp.borrow().get_full_elm_name();
            ctx.plmod.add_impl(&st_name, &trait_name);
        }
        Ok(())
    }
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
fn check_fn<'a, 'b, 'ctx>(
    ctx: &'b mut Ctx<'a>,
    builder: &'b BuilderEnum<'a, 'ctx>,
    method: &FuncDefNode,
    trait_tp: Arc<RefCell<PLType>>,
    traitfns: &mut FxHashSet<String>,
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
                        &st.name
                    ),
                )
                .add_label(
                    st.range,
                    ctx.get_file(),
                    format_label!("trait {} def here", &st.name),
                )
                .add_help(
                    "move this method to another impl block or remove it from current impl block",
                )
                .add_to_ctx(ctx);
        }
        return Ok(());
    }
    unreachable!()
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
        if let Some((typename, _)) = &self.impl_trait {
            typename.emit_highlight(ctx);
            let trait_tp = typename.get_type(ctx, builder, true)?;
            if let PLType::Trait(st) = &*trait_tp.borrow() {
                ctx.send_if_go_to_def(typename.range(), st.range, st.path.clone());
                traittpandrange = Some((trait_tp.clone(), typename.range()));
                for name in st.fields.keys() {
                    traitfns.insert(name.clone());
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
                ctx.if_completion(bt.range, || ctx.get_type_completions());
                return Err(ctx.add_diag(bt.range.new_err(ErrorCode::EXPECT_TYPE)));
            }
            let v = bt.id.as_ref().unwrap().get_type(ctx)?.get_value();
            let st_pltype = v.unwrap().get_ty();
            if let PLType::Struct(sttp) = &*st_pltype.borrow() {
                if let Some((trait_tp, r)) = &traittpandrange {
                    if let PLType::Trait(trait_tp) = &*trait_tp.borrow() {
                        trait_tp.check_impl_derives(ctx, sttp, *r);
                    } else {
                        unreachable!()
                    }
                }
                ctx.send_if_go_to_def(self.target.range(), sttp.range, sttp.path.clone());
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
                        check_fn(
                            ctx,
                            builder,
                            method,
                            trait_tp.clone(),
                            &mut traitfns,
                            fntype,
                        )?;
                    }
                }
            }
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
        Ok(Default::default())
    }
}
