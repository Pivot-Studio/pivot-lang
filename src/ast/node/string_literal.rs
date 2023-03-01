use crate::{
    ast::{
        ctx::Ctx,
        node::{deal_line, tab},
        pltype::PriType,
    },
    plv,
};

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use internal_macro::node;
use lsp_types::SemanticTokenType;

use super::{Node, NodeResult, PLValue, PrintTrait, TerminatorEnum};

#[node]
pub struct StringNode {
    pub content: String,
}

impl PrintTrait for StringNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StringNode: \"{}\"", self.content);
    }
}

impl Node for StringNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        ctx.push_semantic_token(self.range, SemanticTokenType::STRING, 0);
        let v = builder.const_string(&self.content);
        let gcmod = ctx.plmod.submods.get("gc").unwrap();
        let tp = gcmod.get_type("string").unwrap();
        let alloca = builder.alloc("string", &tp.borrow(), ctx, None);
        let len = builder.build_struct_gep(alloca, 1, "len").unwrap();
        let byte_len = builder.build_struct_gep(alloca, 2, "byte_len").unwrap();
        let read_arr = builder.build_struct_gep(alloca, 3, "real_arr").unwrap();
        builder.build_store(read_arr, v);

        builder.build_store(
            len,
            builder.int_value(&PriType::I64, self.content.chars().count() as u64, true),
        );
        builder.build_store(
            byte_len,
            builder.int_value(&PriType::I64, self.content.len() as u64, true),
        );
        Ok((
            Some({
                let mut res: PLValue = plv!(alloca);
                res.set_const(true);
                res
            }),
            Some(tp),
            TerminatorEnum::NONE,
        ))
    }
}
