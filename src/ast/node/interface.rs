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
use rustc_hash::FxHashMap;
#[node]
pub struct MultiTraitNode {
    pub traits: Box<TypeNodeEnum>,
}
impl MultiTraitNode {
    pub fn merge_traits<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<Arc<RefCell<PLType>>, PLDiag> {
        let trait_pltype = self.traits.get_type(ctx, builder)?;
        if matches!(*trait_pltype.borrow(), PLType::Trait(_)) {
            return Ok(trait_pltype);
        }
        Err(ctx.add_diag(self.range().new_err(ErrorCode::EXPECT_TRAIT_TYPE)))
    }
}
#[node]
pub struct TraitBoundNode {
    pub generic: Box<VarNode>,
    pub impl_trait: Option<Box<MultiTraitNode>>,
}
impl TraitBoundNode {
    pub fn set_traits<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
        generic_map: &IndexMap<String, Arc<RefCell<PLType>>>,
    ) -> Result<(), PLDiag> {
        if !generic_map.contains_key(&self.generic.name) {
            return Err(ctx.add_diag(self.generic.range().new_err(ErrorCode::GENERIC_NOT_FOUND)));
        }
        if let Some(impl_trait) = &self.impl_trait {
            let trait_pltype = impl_trait.merge_traits(ctx, builder)?;
            let generic_type = generic_map.get(&self.generic.name).unwrap();
            if let PLType::Generic(generic_type) = &mut *generic_type.borrow_mut() {
                if generic_type.trait_impl.is_some() {
                    return Err(
                        ctx.add_diag(impl_trait.range().new_err(ErrorCode::DUPLICATE_TRAIT_BOUND))
                    );
                }
                generic_type.trait_impl = Some(trait_pltype);
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
    pub derives: Vec<Box<TypeNodeEnum>>,
    pub modifier: Option<(TokenType, Range)>,
}

impl PrintTrait for TraitDefNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("TraitNode");
        self.id.print(tabs + 1, false, line.clone());
        for method in &self.methods {
            method.print(tabs + 1, false, line.clone());
        }
    }
}

impl Node for TraitDefNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        _builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        ctx.push_semantic_token(self.id.range, SemanticTokenType::INTERFACE, 0);
        for de in &self.derives {
            de.emit_highlight(ctx);
        }
        for method in &self.methods {
            method.emit_highlight(ctx);
        }
        Ok((None, None, TerminatorEnum::None))
    }
}

impl TraitDefNode {
    pub fn add_to_symbols<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) {
        let stu = Arc::new(RefCell::new(PLType::Trait(STType {
            generic_map: IndexMap::default(),
            name: self.id.name.clone(),
            path: ctx.plmod.path.clone(),
            fields: FxHashMap::default(),
            ordered_fields: vec![],
            range: self.id.range(),
            doc: vec![],
            derives: vec![],
            modifier: self.modifier,
            body_range: self.range(),
        })));
        builder.opaque_struct_type(&ctx.plmod.get_full_name(&self.id.name));
        _ = ctx.add_type(self.id.name.clone(), stu, self.id.range);
    }
    pub fn emit_trait_def<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<(), PLDiag> {
        let mut fields = FxHashMap::<String, Field>::default();
        let mut order_fields = Vec::<Field>::new();
        let mut i = 0;
        // add generic type before field add type
        let mut derives = vec![];
        for de in &self.derives {
            derives.push(de.get_type(ctx, builder)?);
        }
        // type hash
        order_fields.push(Field {
            index: i,
            typenode: Box::new(TypeNameNode::new_from_str("u64").into()),
            name: "__type_hash".to_string(),
            range: Default::default(),
            modifier: None,
        });
        i += 1;
        // pointer to real value
        order_fields.push(Field {
            index: i,
            typenode: Box::new(TypeNodeEnum::Pointer(PointerTypeNode {
                elm: Box::new(TypeNameNode::new_from_str("i64").into()),
                range: Default::default(),
            })),
            name: "__ptr".to_string(),
            range: Default::default(),
            modifier: None,
        });
        i += 1;
        let pltype = ctx.get_type(self.id.name.as_str(), self.id.range)?;
        let clone_map = ctx.plmod.types.clone();
        for field in self.methods.iter() {
            let mut tp = field.clone();
            tp.paralist
                .insert(0, Box::new(new_i64ptr_tf_with_name("self")));
            let id = field.id.clone();
            let f = Field {
                index: i,
                typenode: Box::new(tp.into()),
                name: field.id.name.clone(),
                range: field.range,
                modifier: Some((TokenType::PUB, field.range)),
            };
            _ = field.get_type(ctx, builder);

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
            order_fields.push(f);
            i += 1;
        }
        let newf = order_fields.clone();
        builder.add_body_to_struct_type(
            &ctx.plmod.get_full_name(&self.id.name),
            &order_fields,
            ctx,
        );
        ctx.plmod.types = clone_map;
        if let PLType::Trait(st) = &mut *pltype.borrow_mut() {
            st.fields = fields;
            st.ordered_fields = newf;
            st.derives = derives;
            // st.doc = self.doc.clone();
        }
        ctx.add_doc_symbols(pltype);
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
