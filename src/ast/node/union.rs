use crate::ast::node::{deal_line, tab, RangeTrait};
use indexmap::IndexMap;
use internal_macro::node;
use lsp_types::SemanticTokenType;
use std::{cell::RefCell, sync::Arc};

use crate::ast::{
    builder::{BuilderEnum, IRBuilder},
    ctx::Ctx,
    pltype::{PLType, UnionType},
    range::Range,
    tokens::TokenType,
};

use super::{
    primary::VarNode, types::GenericDefNode, Node, NodeResult, PrintTrait, TerminatorEnum,
    TypeNode, TypeNodeEnum,
};

#[node]
pub struct UnionDefNode {
    pub modifier: Option<(TokenType, Range)>,
    pub name: VarNode,
    pub generics: Option<Box<GenericDefNode>>,
    pub sum_types: Vec<Box<TypeNodeEnum>>,
}

impl Node for UnionDefNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        ctx.push_semantic_token(self.name.range, SemanticTokenType::TYPE, 0);
        if let Some(generics) = &mut self.generics {
            generics.emit_highlight(ctx);
        }
        for field in self.sum_types.iter() {
            ctx.push_semantic_token(field.range(), SemanticTokenType::TYPE, 0);
        }
        let generic_map = self
            .generics
            .as_ref()
            .map_or(IndexMap::default(), |generics| {
                generics.gen_generic_type(ctx)
            });

        _ = ctx.protect_generic_context(&generic_map, |ctx| {
            for tp in self.sum_types.iter_mut() {
                _ = tp.get_type(ctx, builder);
            }
            Ok(())
        });
        Ok((None, None, TerminatorEnum::None))
    }
}

impl PrintTrait for UnionDefNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("UnionTypeNode");
        self.name.print(tabs + 1, false, line.clone());
    }
}

impl UnionDefNode {
    pub fn add_to_symbols<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) {
        let generic_map = self
            .generics
            .as_ref()
            .map_or(IndexMap::default(), |generics| {
                generics.gen_generic_type(ctx)
            });
        let stu = Arc::new(RefCell::new(PLType::Union(UnionType {
            name: self.name.name.clone(),
            path: ctx.plmod.path.clone(),
            range: self.name.range,
            generic_map,
            modifier: self.modifier,
            sum_types: self.sum_types.clone(),
        })));
        builder.opaque_struct_type(&ctx.plmod.get_full_name(&self.name.name));
        _ = ctx.add_type(self.name.name.clone(), stu, self.name.range);
    }
}
