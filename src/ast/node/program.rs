use std::cell::RefCell;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use super::function::FuncTypeNode;
use super::types::StructDefNode;
use super::*;
use crate::ast::accumulators::*;
use crate::ast::compiler::{compile_dry_file, ActionType};
use crate::ast::ctx::{self, create_ctx_info, Ctx, Mod};
use crate::lsp::mem_docs::{EmitParams, FileCompileInput, MemDocsInput};
use crate::lsp::semantic_tokens::SemanticTokensBuilder;
use crate::utils::read_config::{get_config, Config};
use crate::Db;

use colored::Colorize;
use inkwell::context::Context;
use inkwell::targets::TargetMachine;
use internal_macro::range;
use rustc_hash::{FxHashMap, FxHashSet};

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
                break;
            }
            prev = i;
            ctx.errs.borrow_mut().clear();
        }
        self.fntypes.iter_mut().for_each(|x| {
            _ = x.emit_func_type(ctx);
        });
        // init global
        ctx.function = Some(ctx.module.add_function(
            &ctx.plmod.get_full_name("__init_global"),
            ctx.context.void_type().fn_type(&vec![], false),
            None,
        ));
        ctx.add_global_gc();
        let entry = ctx
            .context
            .append_basic_block(ctx.function.unwrap(), "entry");
        ctx.position_at_end(entry);
        self.globaldefs.iter_mut().for_each(|x| {
            _ = x.emit_global(ctx);
        });
        ctx.nodebug_builder.build_return(None);
        ctx.init_func = Some(ctx.function.unwrap());
        ctx.semantic_tokens_builder = Rc::new(RefCell::new(Box::new(SemanticTokensBuilder::new(
            ctx.plmod.path.to_string(),
        ))));
        // node parser
        self.nodes.iter_mut().for_each(|x| {
            _ = x.emit(ctx);
        });

        Ok((None, None, TerminatorEnum::NONE))
    }
}

#[salsa::tracked]
pub struct Program {
    pub node: ProgramNodeWrapper,
    pub params: EmitParams,
    pub docs: MemDocsInput,
    pub config: Config,
}

#[salsa::tracked]
impl Program {
    #[salsa::tracked(lru = 32)]
    pub fn emit(self, db: &dyn Db) -> ModWrapper {
        let n = *self.node(db).node(db);
        let mut prog = match n {
            NodeEnum::Program(p) => p,
            _ => panic!("not a program"),
        };

        let mut modmap = FxHashMap::<String, Mod>::default();
        if PathBuf::from(self.params(db).file(db))
            .with_extension("")
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            != "gc"
        {
            let core = Box::new(VarNode {
                name: "core".to_string(),
                range: Default::default(),
            });
            let gc = Box::new(VarNode {
                name: "gc".to_string(),
                range: Default::default(),
            });
            prog.uses.push(Box::new(NodeEnum::UseNode(UseNode {
                ids: vec![core, gc],
                range: Default::default(),
                complete: true,
                singlecolon: false,
            })));
        }
        for u in prog.uses {
            let u = if let NodeEnum::UseNode(p) = *u {
                p
            } else {
                continue;
            };
            if u.ids.is_empty() || !u.complete {
                continue;
            }
            let mut path = PathBuf::from(self.config(db).root);
            let mut config = self.config(db);
            let mut start = 0;
            // 加载依赖包的路径
            if let Some(cm) = self.config(db).deps {
                // 如果use的是依赖包
                if let Some(dep) = cm.get(&u.ids[0].name) {
                    path = path.join(&dep.path);
                    let input = FileCompileInput::new(
                        db,
                        path.join("Kagari.toml")
                            .clone()
                            .to_str()
                            .unwrap()
                            .to_string(),
                        "".to_string(),
                        self.docs(db),
                        Default::default(),
                        Default::default(),
                    );
                    let f = input.get_file_content(db);
                    if f.is_none() {
                        continue;
                    }
                    let re = get_config(db, f.unwrap());
                    if re.is_err() {
                        continue;
                    }
                    config = re.unwrap();
                    config.root = path.to_str().unwrap().to_string();

                    start = 1;
                }
            }
            for p in u.ids[start..].iter() {
                path = path.join(p.name.clone());
            }
            path = path.with_extension("pi");
            let f = path.to_str().unwrap().to_string();
            // eprintln!("use {}", f.clone());
            let f = FileCompileInput::new(
                db,
                f,
                self.params(db).modpath(db).clone(),
                self.docs(db),
                Default::default(),
                config,
            );
            let m = compile_dry_file(db, f);
            if m.is_none() {
                continue;
            }
            let m = m.unwrap();
            modmap.insert(u.ids.last().unwrap().name.clone(), m.plmod(db));
        }
        let filepath = Path::new(self.params(db).file(db));
        let abs = dunce::canonicalize(filepath).unwrap();
        let dir = abs.parent().unwrap().to_str().unwrap();
        let fname = abs.file_name().unwrap().to_str().unwrap();

        let p = ProgramEmitParam::new(
            db,
            self.node(db),
            dir.to_string(),
            fname.to_string(),
            abs.to_str().unwrap().to_string(),
            self.params(db),
            modmap,
        );

        emit_file(db, p)
    }
}

#[salsa::tracked]
pub struct ProgramEmitParam {
    pub node: ProgramNodeWrapper,
    #[return_ref]
    pub dir: String,
    #[return_ref]
    pub file: String,
    #[return_ref]
    pub fullpath: String,
    #[return_ref]
    pub params: EmitParams,
    pub submods: FxHashMap<String, Mod>,
}

#[salsa::tracked]
pub fn emit_file(db: &dyn Db, params: ProgramEmitParam) -> ModWrapper {
    let context = &Context::create();
    let action = params.params(db).action(db);
    let (a, b, c, d, e, f) = create_ctx_info(context, params.dir(db), params.file(db));
    let v = RefCell::new(Vec::new());
    let mut ctx = ctx::Ctx::new(
        context,
        &a,
        &b,
        &c,
        &d,
        &e,
        &f,
        params.fullpath(db),
        &v,
        Some(params.params(db).action(db)),
        params.params(db).params(db),
        params.params(db).config(db),
    );
    ctx.plmod.submods = params.submods(db);
    let m = &mut ctx;
    m.module.set_triple(&TargetMachine::get_default_triple());
    let node = params.node(db);
    let mut nn = node.node(db);
    if m.action.is_some() && m.action.unwrap() == ActionType::PrintAst {
        println!("file: {}", params.fullpath(db).green());
        nn.print(0, true, vec![]);
    }
    let _ = nn.emit(m);
    Diagnostics::push(
        db,
        (
            params.fullpath(db).clone(),
            v.borrow().iter().map(|x| x.clone()).collect(),
        ),
    );
    if let Some(c) = ctx.refs.take() {
        PLReferences::push(db, c.clone());
    }
    if ctx.action.is_some() && ctx.action.unwrap() == ActionType::SemanticTokensFull {
        let b = ctx.semantic_tokens_builder.borrow().build();
        PLSemanticTokens::push(db, b);
    }
    if action == ActionType::Completion {
        let ci = ctx.completion_items.take();
        Completions::push(db, ci);
    }
    if let Some(c) = ctx.goto_def.take() {
        GotoDef::push(db, c);
    }
    if let Some(c) = ctx.hover.take() {
        PLHover::push(db, c);
    }
    if ctx.action.is_some() && ctx.action.unwrap() == ActionType::Compile {
        ctx.dibuilder.finalize();
        let mut hasher = DefaultHasher::new();
        params.fullpath(db).hash(&mut hasher);
        let hashed = format!(
            "{}_{:x}",
            Path::new(&params.file(db))
                .with_extension("")
                .to_str()
                .unwrap(),
            hasher.finish()
        );
        let pp = Path::new(&hashed).with_extension("bc");
        let p = pp.as_path();
        ctx.module.write_bitcode_to_path(p);
        // _= std::fs::write(
        //     Path::new(&hashed).with_extension("ll"),
        //     ctx.module.print_to_string().to_string(),
        // );
        ModBuffer::push(db, p.clone().to_path_buf());
    }
    ModWrapper::new(db, ctx.plmod)
}

#[salsa::tracked]
pub struct ProgramNodeWrapper {
    pub node: Box<NodeEnum>,
}

#[salsa::tracked]
pub struct ModWrapper {
    pub plmod: Mod,
}
