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
#[node]
pub struct MultiTraitNode {
    pub traits: Vec<Box<TypeNodeEnum>>,
}
impl MultiTraitNode {
    fn merge_traits<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> Result<Arc<RefCell<PLType>>, PLDiag> {
        let derives = self.get_types(ctx, builder)?;
        if derives.len() == 1 {
            return Ok(derives[0].clone());
        }
        let name = derives
            .iter()
            .map(|t| t.borrow().get_name())
            .collect::<Vec<_>>()
            .join("+");
        let st = STType {
            generic_map: IndexMap::default(),
            name: name.clone(),
            path: ctx.plmod.path.clone(),
            fields: LinkedHashMap::default(),
            range: Default::default(),
            doc: vec![],
            derives,
            modifier: None,
            body_range: Default::default(),
            is_trait: true,
            is_tuple: false,
            generic_infer_types: Default::default(),
            generic_infer: Default::default(),
            methods: Default::default(),
            trait_methods_impl: Default::default(),
        };
        builder.opaque_struct_type(&ctx.plmod.get_full_name(&name));
        builder.add_body_to_struct_type(&ctx.plmod.get_full_name(&name), &st, ctx);
        let trait_tp = Arc::new(RefCell::new(PLType::Trait(st)));
        _ = ctx.add_type(name, trait_tp.clone(), Default::default());
        Ok(trait_tp)
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
    pub generic: Box<VarNode>,
    pub impl_trait: Option<Box<MultiTraitNode>>,
}
impl TraitBoundNode {
    pub fn set_traits<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
        generic_map: &IndexMap<String, Arc<RefCell<PLType>>>,
    ) -> Result<(), PLDiag> {
        if !generic_map.contains_key(&self.generic.name) {
            return Err(ctx.add_diag(self.generic.range().new_err(ErrorCode::GENERIC_NOT_FOUND)));
        }
        if let Some(impl_trait) = &self.impl_trait {
            let trait_pltype = impl_trait.get_types(ctx, builder)?;
            let trait_place_holder = impl_trait.merge_traits(ctx, builder)?;
            let generic_type = generic_map.get(&self.generic.name).unwrap();
            if let PLType::Generic(generic_type) = &mut *generic_type.borrow_mut() {
                if generic_type.trait_impl.is_some() {
                    return Err(
                        ctx.add_diag(impl_trait.range().new_err(ErrorCode::DUPLICATE_TRAIT_BOUND))
                    );
                }
                generic_type.trait_impl = Some(trait_pltype);
                generic_type.trait_place_holder = Some(trait_place_holder);
                return Ok(());
            }
            unreachable!()
        }
        Ok(())
    }
    pub fn emit_highlight(&self, ctx: &mut Ctx) {
        ctx.push_semantic_token(self.generic.range, SemanticTokenType::TYPE, 0);
        if let Some(impl_trait) = &self.impl_trait {
            ctx.push_semantic_token(impl_trait.range(), SemanticTokenType::TYPE, 0);
        }
    }
}
#[node]
pub struct TraitDefNode {
    pub id: Box<VarNode>,
    pub methods: Vec<FuncDefNode>,
    pub derives: MultiTraitNode,
    pub modifier: Option<(TokenType, Range)>,
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
        self.derives.emit_highlight(ctx);
        for method in &self.methods {
            method.emit_highlight(ctx);
        }
        Ok(Default::default())
    }
}

impl TraitDefNode {
    pub fn add_to_symbols<'a, 'b>(&self, ctx: &'b mut Ctx<'a>, builder: &'b BuilderEnum<'a, '_>) {
        let stu = Arc::new(RefCell::new(PLType::Trait(STType {
            generic_map: IndexMap::default(),
            name: self.id.name.clone(),
            path: ctx.plmod.path.clone(),
            fields: LinkedHashMap::default(),
            range: self.id.range(),
            doc: vec![],
            derives: vec![],
            modifier: self.modifier,
            body_range: self.range(),
            is_trait: true,
            is_tuple: false,
            generic_infer_types: Default::default(),
            generic_infer: Default::default(),
            methods: Default::default(),
            trait_methods_impl: Default::default(),
        })));
        builder.opaque_struct_type(&ctx.plmod.get_full_name(&self.id.name));
        _ = ctx.add_type(self.id.name.clone(), stu, self.id.range);
    }
    pub fn emit_trait_def<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> Result<(), PLDiag> {
        let mut fields = LinkedHashMap::new();
        // add generic type before field add type
        let derives = self.derives.get_types(ctx, builder)?;
        let pltype = ctx.get_type(self.id.name.as_str(), self.id.range)?;
        for (i, field) in self.methods.iter().enumerate() {
            let mut tp = field.clone();
            tp.paralist
                .insert(0, Box::new(new_i64ptr_tf_with_name("self")));
            let id = field.id.clone();
            let f = Field {
                index: i as u32 + 2,
                typenode: Box::new(tp.into()),
                name: field.id.name.clone(),
                range: field.range,
                modifier: Some((TokenType::PUB, field.range)),
            };
            _ = field.get_type(ctx, builder, true);

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
            fields.insert(id.name.to_string(), f.clone());
        }
        if let PLType::Trait(st) = &mut *pltype.borrow_mut() {
            st.fields = fields;
            st.derives = derives;
            builder.add_body_to_struct_type(&ctx.plmod.get_full_name(&self.id.name), st, ctx);
        }
        ctx.add_doc_symbols(pltype.tp);
        // ctx.save_if_comment_doc_hover(self.range, Some(self.doc.clone()));
        Ok(())
    }
}

fn new_i64ptr_tf_with_name(n: &str) -> TypedIdentifierNode {
    TypedIdentifierNode {
        id: VarNode {
            name: n.to_string(),
            range: Default::default(),
        },
        typenode: Box::new(TypeNodeEnum::Pointer(PointerTypeNode {
            elm: Box::new(TypeNameNode::new_from_str("i64").into()),
            range: Default::default(),
        })),
        doc: None,
        range: Default::default(),
    }
}
