use crate::ast::{
    diag::PLDiag,
    node::{RangeTrait, TypeNode},
};
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
    TypeNodeEnum,
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
        _builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        ctx.push_semantic_token(self.name.range, SemanticTokenType::TYPE, 0);
        if let Some(generics) = &mut self.generics {
            generics.emit_highlight(ctx);
        }
        for field in self.sum_types.iter() {
            ctx.push_semantic_token(field.range(), SemanticTokenType::TYPE, 0);
        }
        Ok((None, None, TerminatorEnum::None))
    }
}

impl PrintTrait for UnionDefNode {
    fn print(&self, tabs: usize, end: bool, line: Vec<bool>) {
        todo!()
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
            .map_or(IndexMap::default(), |generics| generics.gen_generic_type());
        let stu = Arc::new(RefCell::new(PLType::Union(UnionType {
            name: self.name.name.clone(),
            path: ctx.plmod.path.clone(),
            range: self.range,
            generic_map,
            modifier: self.modifier,
            sum_types: self.sum_types.clone(),
        })));
        builder.opaque_struct_type(&ctx.plmod.get_full_name(&self.name.name));
        _ = ctx.add_type(self.name.name.clone(), stu, self.name.range);
    }
    pub fn emit_union_def<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<(), PLDiag> {
        // let tp = ctx.get_type(&self.name.name, self.range)?;
        // let mut tp = tp.borrow_mut();
        // if let PLType::Union(u) = &mut *tp {
        //     for sum_type in self.sum_types.iter() {
        //         let sum_type = sum_type.get_type(ctx, builder)?;
        //         u.sum_types.push(sum_type);
        //     }
        // } else {
        //     unreachable!()
        // }
        // Ok(())
        todo!()
    }
}
