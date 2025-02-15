use super::primary::VarNode;
use super::*;
use crate::{
    ast::{
        ctx::Ctx,
        pltype::{Field, STType},
        tokens::TokenType,
    },
    format_label,
};
use indexmap::IndexMap;
use internal_macro::node;
use linked_hash_map::LinkedHashMap;
use ustr::Ustr;
#[node]
pub struct MultiTraitNode {
    pub traits: Vec<Box<TypeNodeEnum>>,
}
impl MultiTraitNode {
    pub fn merge(&mut self, other: &MultiTraitNode) {
        self.traits.extend(other.traits.clone());
    }
    pub fn emit_highlight(&self, ctx: &mut Ctx) {
        for t in &self.traits {
            t.emit_highlight(ctx);
        }
    }
    pub fn get_types<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> Result<Vec<Arc<RefCell<PLType>>>, PLDiag> {
        let mut traits = vec![];
        for t in &self.traits {
            let trait_tp = t.get_type(ctx, builder, true)?;
            if !matches!(&*trait_tp.borrow(), PLType::Trait(_)) {
                return Err(t
                    .range()
                    .new_err(ErrorCode::EXPECT_TRAIT_TYPE)
                    .add_to_ctx(ctx));
            }
            traits.push(trait_tp);
        }
        Ok(traits)
    }
}
#[node]
pub struct TraitBoundNode {
    pub identifier: Box<VarNode>,
    /// impl_trait is the trait identifier bound to
    pub impl_trait: Option<Box<MultiTraitNode>>,
}
impl TraitBoundNode {
    pub fn set_traits(
        &self,
        ctx: &mut Ctx<'_>,
        generic_map: &IndexMap<Ustr, Arc<RefCell<PLType>>>,
    ) -> Result<(), PLDiag> {
        if !generic_map.contains_key(&self.identifier.name) {
            return Err(ctx.add_diag(
                self.identifier
                    .range()
                    .new_err(ErrorCode::GENERIC_NOT_FOUND),
            ));
        }
        if let Some(impl_trait) = &self.impl_trait {
            // let trait_pltype = impl_trait.get_types(ctx, builder)?;
            let generic_type = generic_map.get(&self.identifier.name).unwrap();
            if let PLType::Generic(generic_type) = &mut *generic_type.borrow_mut() {
                if generic_type.trait_impl.is_some() {
                    return Err(
                        ctx.add_diag(impl_trait.range().new_err(ErrorCode::DUPLICATE_TRAIT_BOUND))
                    );
                }
                generic_type.trait_impl = Some(*impl_trait.clone());
                return Ok(());
            }
            unreachable!()
        }
        Ok(())
    }
    pub fn emit_highlight(&self, ctx: &mut Ctx) {
        ctx.push_semantic_token(self.identifier.range, SemanticTokenType::TYPE, 0);
        if let Some(impl_trait) = &self.impl_trait {
            impl_trait.emit_highlight(ctx);
        }
    }
}
#[node]
pub struct TraitDefNode {
    /// id is the identifier of the trait
    pub id: Box<VarNode>,
    /// methods is all methods defined by a trait
    pub methods: Vec<FuncDefNode>,
    /// derives is the trait derived by the current trait, multiple traits derivation are allowed
    pub derives: MultiTraitNode,
    /// modifier indicates whether the trait is decorated by a keyword `pub`
    pub modifier: Option<(TokenType, Range)>,
    pub generics: Option<Box<GenericDefNode>>,
}

impl PrintTrait for TraitDefNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("TraitNode");
        self.id.print(tabs + 1, false, line.clone());
        for (i, method) in self.methods.iter().enumerate() {
            method.print(tabs + 1, i == self.methods.len() - 1, line.clone());
        }
    }
}

impl Node for TraitDefNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        _builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        ctx.push_semantic_token(self.id.range, SemanticTokenType::INTERFACE, 0);
        if let Some(g) = &self.generics {
            g.emit_highlight(ctx);
        }
        self.derives.emit_highlight(ctx);
        for method in &self.methods {
            method.emit_highlight(ctx);
        }
        Ok(Default::default())
    }
}

impl TraitDefNode {
    pub fn add_to_symbols<'a, 'b>(&self, ctx: &'b mut Ctx<'a>, builder: &'b BuilderEnum<'a, '_>) {
        let generic_map = if let Some(generics) = &self.generics {
            let mp = generics.gen_generic_type(ctx);
            _ = generics.set_traits(ctx, &mp);
            mp
        } else {
            IndexMap::default()
        };
        let stu = Arc::new(RefCell::new(PLType::Trait(STType {
            generic_map,
            name: self.id.name,
            path: ctx.plmod.path,
            fields: LinkedHashMap::default(),
            range: self.id.range(),
            doc: vec![],
            derives: vec![],
            modifier: self.modifier,
            body_range: self.range(),
            is_trait: true,
            is_tuple: false,
            generic_infer_types: Default::default(),
            // generic_infer: Default::default(),
            methods: Default::default(),
            trait_methods_impl: Default::default(),
            atomic: false,
        })));
        builder.opaque_struct_type(&ctx.plmod.get_full_name(self.id.name));
        _ = ctx.add_type(self.id.name, stu, self.id.range);
    }
    pub fn emit_trait_def<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> Result<(), PLDiag> {
        let pltype = ctx.get_type(&self.id.name, self.id.range)?;
        let generic_map = if let PLType::Trait(st) = &mut *pltype.borrow_mut() {
            st.generic_map.clone()
        } else {
            IndexMap::default()
        };
        ctx.protect_generic_context(&generic_map, |ctx| {
            // ctx.set_self_type(Arc::new(RefCell::new(PLType::Primitive( PriType::I64))));// 随便放个类型，用Self的接口不能实例化
            let mut fields = LinkedHashMap::new();
            // add generic type before field add type
            let derives = self.derives.get_types(ctx, builder)?;
            for (i, field) in self.methods.iter().enumerate() {
                let mut tp = field.clone();
                tp.modifier = self.modifier;
                tp.paralist
                    .insert(0, Box::new(new_selfptr_tf_with_name("self".into())));
                let id = field.id.clone();
                let f = Field {
                    index: i as u32 + 2,
                    typenode: Box::new(tp.into()),
                    name: field.id.name,
                    range: field.range,
                    modifier: Some((TokenType::PUB, field.range)),
                };
                field.emit_highlight(ctx);
                if field.get_type(ctx, builder, true).is_err() {
                    continue;
                }

                if let Some((m, r)) = field.modifier {
                    r.new_err(ErrorCode::TRAIT_METHOD_SHALL_NOT_HAVE_MODIFIER)
                        .add_label(
                            r,
                            ctx.get_file(),
                            format_label!("modifier {} shall be removed", m.get_str()),
                        )
                        .add_help(
                            "trait methods share the same modifier with trait, \
                                so you shall not add modifier here",
                        )
                        .add_to_ctx(ctx);
                }

                // ctx.set_if_refs(f.refs.clone(), field.id.range);
                fields.insert(id.name, f.clone());
            }
            if let PLType::Trait(st) = &mut *pltype.borrow_mut() {
                st.fields = fields.clone();
                st.derives = derives.clone();
                if let Some(stpltype) = ctx.linked_tp_tbl.remove(&pltype.typ.as_ptr()) {
                    for st in stpltype {
                        if let PLType::Trait(st) = &mut *st.borrow_mut() {
                            st.fields = fields.clone();
                            st.derives = derives.clone();
                        }
                    }
                }
                builder.add_body_to_struct_type(&ctx.plmod.get_full_name(self.id.name), st, ctx);
            }
            ctx.add_doc_symbols(pltype.clone().typ);
            // ctx.save_if_comment_doc_hover(self.range, Some(self.doc.clone()));
            Ok(())
        })
    }
}

fn new_selfptr_tf_with_name(n: Ustr) -> TypedIdentifierNode {
    TypedIdentifierNode {
        id: VarNode {
            name: n,
            range: Default::default(),
            id: None,
        },
        typenode: Box::new(TypeNodeEnum::Pointer(PointerTypeNode {
            elm: Box::new(TypeNameNode::new_from_str(&"i64".into()).into()),
            range: Default::default(),
        })),
        doc: None,
        range: Default::default(),
    }
}
