use super::primary::VarNode;
use super::*;
use crate::ast::{
    ctx::Ctx,
    pltype::{Field, STType},
};
use indexmap::IndexMap;
use internal_macro::{fmt, range};
use rustc_hash::FxHashMap;

#[range]
#[fmt]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TraitDefNode {
    pub id: Box<VarNode>,
    pub generics: Option<Box<GenericDefNode>>,
    pub methods: Vec<FuncDefNode>,
    pub derives: Vec<Box<TypeNodeEnum>>,
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
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        ctx.push_semantic_token(self.id.range, SemanticTokenType::INTERFACE, 0);
        for g in &mut self.generics {
            g.emit(ctx, builder)?;
        }
        for de in &self.derives {
            de.emit_highlight(ctx);
        }
        for method in &self.methods {
            method.emit_highlight(ctx);
        }
        Ok((None, None, TerminatorEnum::NONE))
    }
}

impl TraitDefNode {
    pub fn add_to_symbols<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) {
        let mut generic_map = IndexMap::default();
        if let Some(generics) = &self.generics {
            generic_map = generics.gen_generic_type(ctx);
        }
        let stu = Arc::new(RefCell::new(PLType::TRAIT(STType {
            name: self.id.name.clone(),
            path: ctx.plmod.path.clone(),
            fields: FxHashMap::default(),
            ordered_fields: vec![],
            range: self.range(),
            // refs: Arc::new(RwVec::new()),
            doc: vec![],
            generic_map,
            impls: FxHashMap::default(),
            derives: vec![],
        })));
        builder.opaque_struct_type(&ctx.plmod.get_full_name(&self.id.name));
        _ = ctx.add_type(self.id.name.clone(), stu, self.id.range);
    }
    pub fn emit_trait_def<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<(), PLDiag> {
        let mp = ctx.move_generic_types();
        let mut fields = FxHashMap::<String, Field>::default();
        let mut order_fields = Vec::<Field>::new();
        let mut i = 0;
        // add generic type before field add type
        if let Some(generics) = &mut self.generics {
            let generic_map = generics.gen_generic_type(ctx);
            for (name, pltype) in generic_map.iter() {
                ctx.add_generic_type(
                    name.clone(),
                    pltype.clone(),
                    pltype.clone().borrow().get_range().unwrap(),
                );
            }
        }
        let mut derives = vec![];
        for de in &self.derives {
            derives.push(de.get_type(ctx, builder)?);
        }
        // type hash
        order_fields.push(Field {
            index: i,
            typenode: Box::new(TypeNameNode::new_from_str("u64").into()),
            name: "tmp".to_string(),
            range: Default::default(),
            // refs: Arc::new(RwVec::new()),
        });
        i += 1;
        // pointer to real value
        order_fields.push(Field {
            index: i,
            typenode: Box::new(TypeNodeEnum::PointerTypeNode(PointerTypeNode {
                elm: Box::new(TypeNameNode::new_from_str("i64").into()),
                range: Default::default(),
            })),
            name: "tmp".to_string(),
            range: Default::default(),
            // refs: Arc::new(RwVec::new()),
        });
        i += 1;
        let pltype = ctx.get_type(self.id.name.as_str(), self.range)?;
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
                // refs: Arc::new(RwVec::new()),
            };
            field.get_type(ctx, builder)?;

            // ctx.set_if_refs(f.refs.clone(), field.id.range);
            fields.insert(id.name.to_string(), f.clone());
            order_fields.push(f);
            i += 1;
        }
        let newf = order_fields.clone();
        if self.generics.is_none() {
            builder.add_body_to_struct_type(
                &ctx.plmod.get_full_name(&self.id.name),
                &order_fields,
                ctx,
            );
        }
        ctx.plmod.types = clone_map;
        if let PLType::TRAIT(st) = &mut *pltype.borrow_mut() {
            st.fields = fields;
            st.ordered_fields = newf;
            st.derives = derives;
            // st.doc = self.doc.clone();
        }
        ctx.set_if_refs_tp(pltype.clone(), self.id.range);
        ctx.add_doc_symbols(pltype);
        // ctx.save_if_comment_doc_hover(self.range, Some(self.doc.clone()));
        ctx.reset_generic_types(mp);
        Ok(())
    }
}

fn new_i64ptr_tf_with_name(n: &str) -> TypedIdentifierNode {
    TypedIdentifierNode {
        id: VarNode {
            name: n.to_string(),
            range: Default::default(),
        },
        typenode: Box::new(TypeNodeEnum::PointerTypeNode(PointerTypeNode {
            elm: Box::new(TypeNameNode::new_from_str("i64").into()),
            range: Default::default(),
        })),
        doc: None,
        range: Default::default(),
    }
}
