use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;

use super::function::FuncTypeNode;
use super::types::StructDefNode;
use super::*;
use crate::ast::accumulators::*;
use crate::ast::ctx::{self, create_ctx_info, Ctx, Mod};
use crate::lsp::mem_docs::{EmitParams, MemDocsInput};
use crate::lsp::semantic_tokens::SemanticTokensBuilder;
use crate::Db;

use inkwell::context::Context;
use internal_macro::range;
use rustc_hash::FxHashSet;

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ProgramNode {
    pub nodes: Vec<Box<NodeEnum>>,
    pub structs: Vec<StructDefNode>,
    pub fntypes: Vec<FuncTypeNode>,
    pub globaldefs: Vec<GlobalNode>,
    pub uses: Vec<Box<NodeEnum>>,
}
impl Node for ProgramNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        println!("ProgramNode");
        let mut i = self.nodes.len();
        for statement in &self.nodes {
            i -= 1;
            statement.print(tabs, i == 0, line.clone());
        }
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        // top level parser
        let mut prev = 1000000;
        let mut idxs = FxHashSet::default();
        loop {
            let mut i = 0;
            for (idx, def) in self.structs.iter().enumerate() {
                if idxs.contains(&idx) {
                    continue;
                }
                if def.emit_struct_def(ctx).is_err() {
                    i = i + 1;
                } else {
                    idxs.insert(idx);
                }
            }
            if i == 0 {
                break;
            }
            if i == prev {
                self.structs.iter().for_each(|x| {
                    if let Err(e) = x.emit_struct_def(ctx) {
                        ctx.add_diag(e);
                    }
                });
                break;
            }
            prev = i;
        }
        self.fntypes.iter_mut().for_each(|x| {
            _ = x.emit_func_type(ctx);
        });
        ctx.semantic_tokens_builder = Rc::new(RefCell::new(Box::new(SemanticTokensBuilder::new(
            ctx.plmod.path.to_string(),
        ))));
        // init global
        ctx.function = Some(ctx.module.add_function(
            "__init_global",
            ctx.context.void_type().fn_type(&vec![], false),
            None,
        ));
        let entry = ctx
            .context
            .append_basic_block(ctx.function.unwrap(), "entry");
        ctx.position_at_end(entry);
        self.globaldefs.iter_mut().for_each(|x| {
            _ = x.emit_global(ctx);
        });
        ctx.nodebug_builder.build_return(None);
        ctx.init_func = Some(ctx.function.unwrap());
        // node parser
        self.nodes.iter_mut().for_each(|x| {
            if let NodeEnum::Global(_) = **x {
            } else {
                _ = x.emit(ctx);
            }
        });

        Ok((Value::None, None, TerminatorEnum::NONE, false))
    }
}

#[salsa::tracked]
pub struct Program {
    pub node: ProgramNodeWrapper,
    pub params: EmitParams,
    pub docs: MemDocsInput,
}

#[salsa::tracked]
impl Program {
    #[salsa::tracked(lru = 32)]
    pub fn emit(self, db: &dyn Db) -> ModWrapper {
        eprintln!("emit");
        let n = *self.node(db).node(db);
        let _prog = match n {
            NodeEnum::Program(p) => p,
            _ => panic!("not a program"),
        };

        let context = &Context::create();
        let filepath = Path::new(self.params(db).file(db));
        let abs = dunce::canonicalize(filepath).unwrap();
        let dir = abs.parent().unwrap().to_str().unwrap();
        let fname = abs.file_name().unwrap().to_str().unwrap();
        let (a, b, c, d, e, f) = create_ctx_info(context, dir, fname);
        let v = RefCell::new(Vec::new());
        let mut ctx = ctx::Ctx::new(
            context,
            &a,
            &b,
            &c,
            &d,
            &e,
            &f,
            abs.to_str().unwrap(),
            &v,
            Some(self.params(db).action(db)),
            self.params(db).params(db),
        );
        let m = &mut ctx;
        let node = self.node(db);
        let mut nn = node.node(db);
        let _ = nn.emit(m);
        for d in v.borrow().iter() {
            Diagnostics::push(db, d.get_diagnostic());
        }
        if let Some(c) = ctx.refs.take() {
            let c = c.borrow();
            for refe in c.iter() {
                PLReferences::push(db, refe.clone());
            }
        }
        let b = ctx.semantic_tokens_builder.borrow().build();
        PLSemanticTokens::push(db, b);
        let ci = ctx.completion_items.take();
        Completions::push(db, ci);
        if let Some(c) = ctx.goto_def.take() {
            GotoDef::push(db, c);
        }
        if let Some(c) = ctx.hover.take() {
            PLHover::push(db, c);
        }
        ModWrapper::new(db, ctx.plmod)
    }
}

#[salsa::tracked]
pub struct ProgramNodeWrapper {
    pub node: Box<NodeEnum>,
}

#[salsa::tracked]
pub struct ModWrapper {
    pub plmod: Mod,
}
