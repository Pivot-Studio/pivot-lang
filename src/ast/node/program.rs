#![allow(clippy::too_many_arguments)]
use super::function::FuncDefNode;
use super::types::StructDefNode;
use super::*;
use crate::ast::accumulators::*;
use crate::ast::builder::llvmbuilder::create_llvm_deps;
use crate::ast::builder::llvmbuilder::LLVMBuilder;
use crate::ast::builder::no_op_builder::NoOpBuilder;
use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::ast::compiler::COMPILE_PROGRESS;
use crate::ast::compiler::{compile_dry_file, ActionType};
use crate::ast::ctx::{self, Ctx};
use crate::ast::plmod::LSPDef;
use crate::ast::plmod::Mod;
use crate::flow::display::Dot;
use crate::lsp::mem_docs::{EmitParams, MemDocsInput};
use crate::lsp::semantic_tokens::SemanticTokensBuilder;
use crate::lsp::text;
use crate::utils::read_config::Config;
use crate::Db;
use colored::Colorize;
use inkwell::context::Context;

use internal_macro::node;
use lsp_types::GotoDefinitionResponse;
use rustc_hash::FxHashMap;
use rustc_hash::FxHashSet;
use std::cell::RefCell;
use std::collections::hash_map::DefaultHasher;
use std::fs;
use std::fs::OpenOptions;
use std::hash::{Hash, Hasher};
use std::io::prelude::*;
use std::ops::Bound::*;
use std::path::{Path, PathBuf};

use std::sync::Arc;

#[node]
pub struct ProgramNode {
    pub nodes: Vec<Box<NodeEnum>>,
    pub structs: Vec<StructDefNode>,
    pub fntypes: Vec<FuncDefNode>,
    pub globaldefs: Vec<GlobalNode>,
    pub uses: Vec<Box<NodeEnum>>,
    pub traits: Vec<TraitDefNode>,
    pub trait_impls: Vec<(String, String)>,
    pub unions: Vec<UnionDefNode>,
}

impl PrintTrait for ProgramNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        println!("ProgramNode");
        let mut i = self.nodes.len();
        for statement in &self.nodes {
            i -= 1;
            statement.print(tabs, i == 0, line.clone());
        }
    }
}

impl Node for ProgramNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        // emit structs
        for def in self.traits.iter() {
            // 提前加入占位符号，解决自引用问题
            def.add_to_symbols(ctx, builder);
        }
        for def in self.structs.iter() {
            // 提前加入占位符号，解决自引用问题
            def.add_to_symbols(ctx, builder);
        }
        for def in self.structs.iter_mut() {
            _ = def.emit_struct_def(ctx, builder);
        }
        for def in self.traits.iter_mut() {
            _ = def.emit_trait_def(ctx, builder);
        }
        self.fntypes.iter_mut().for_each(|x| {
            _ = x.emit_func_def(ctx, builder);
        });
        self.trait_impls.iter().for_each(|x| {
            let (struct_name, trait_name) = x;
            ctx.plmod.add_impl(struct_name, trait_name)
        });
        // init global
        ctx.set_init_fn(builder);
        self.globaldefs.iter_mut().for_each(|x| {
            _ = x.emit_global(ctx, builder);
        });
        ctx.clear_init_fn(builder);
        ctx.plmod.semantic_tokens_builder = Arc::new(RefCell::new(Box::new(
            SemanticTokensBuilder::new(ctx.plmod.path.to_string()),
        )));
        // node parser
        self.nodes.iter_mut().for_each(|x| {
            _ = x.emit(ctx, builder);
        });
        ctx.init_fn_ret(builder);
        Ok((None, None, TerminatorEnum::None))
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
        let pb = &COMPILE_PROGRESS;
        let n = *self.node(db).node(db);
        let mut prog = match n {
            NodeEnum::Program(p) => p,
            _ => panic!("not a program"),
        };
        let params = self.params(db);
        let f1 = self.docs(db).file(db);
        let f2 = params.file(db);
        let is_active_file = dunce::canonicalize(f1).unwrap() == dunce::canonicalize(f2).unwrap();

        let mut modmap = FxHashMap::<String, Mod>::default();
        let binding = PathBuf::from(self.params(db).file(db)).with_extension("");
        let pkgname = binding.file_name().unwrap().to_str().unwrap();
        if pkgname != "gc" {
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
        if pb.length().is_none() {
            pb.set_length(1 + prog.uses.len() as u64);
        } else {
            pb.inc_length(1 + prog.uses.len() as u64);
        }
        for (i, u) in prog.uses.iter().enumerate() {
            pb.set_message(format!(
                "正在编译包{}的依赖项{}/{}",
                pkgname,
                i,
                prog.uses.len()
            ));
            pb.inc(1);
            // pb.set_prefix(format!("[{:3}/{:3}]", pb.position(), pb.length().unwrap()));
            let u = if let NodeEnum::UseNode(p) = *u.clone() {
                p
            } else {
                continue;
            };
            if u.ids.is_empty() || !u.complete {
                continue;
            }
            let mut path = PathBuf::from(self.config(db).root);
            // 加载依赖包的路径
            if let Some(cm) = self.config(db).deps {
                // 如果use的是依赖包
                if let Some(dep) = cm.get(&u.ids[0].name) {
                    path = path.join(&dep.path);
                }
            }
            for p in u.ids[1..].iter() {
                path = path.join(p.name.clone());
            }
            path = path.with_extension("pi");
            let f = path.to_str().unwrap().to_string();
            // eprintln!("use {}", f.clone());
            let f = self.docs(db).get_file_params(db, f, false);
            if f.is_none() {
                continue;
            }
            let f = f.unwrap();
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
        // 不要修改这里，除非你知道自己在干什么
        // 除了当前用户正打开的文件外，其他文件的编辑位置都输入None，这样可以保证每次用户修改的时候，
        // 未被修改的文件的`emit_file`参数与之前一致，不会被重新分析
        // 修改这里可能导致所有文件被重复分析，从而导致lsp性能下降
        let pos = if is_active_file {
            self.docs(db).edit_pos(db)
        } else {
            None
        };

        let p = ProgramEmitParam::new(
            db,
            self.node(db),
            dir.to_string(),
            fname.to_string(),
            abs.to_str().unwrap().to_string(),
            LspParams::new(
                db,
                params.modpath(db).to_string(),
                pos,
                params.config(db),
                params.action(db) == ActionType::Compile,
            ),
            modmap,
            self.docs(db)
                .get_current_file_content(db)
                .unwrap()
                .text(db)
                .clone(),
        );

        let nn = p.node(db).node(db);
        match params.action(db) {
            ActionType::PrintAst => {
                println!("file: {}", p.fullpath(db).green());
                nn.print(0, true, vec![]);
            }
            ActionType::Fmt => {
                let mut builder = FmtBuilder::new();
                nn.format(&mut builder);
                let code = builder.generate();
                let mut f = OpenOptions::new()
                    .write(true)
                    .truncate(true)
                    .open(p.fullpath(db))
                    .unwrap();
                f.write_all(code.as_bytes()).unwrap();
            }
            ActionType::LspFmt => {
                let oldcode = p.file_content(db);
                let mut builder = FmtBuilder::new();
                nn.format(&mut builder);
                let newcode = builder.generate();
                let diff = text::diff(oldcode, &newcode);
                let line_index = text::LineIndex::new(oldcode);
                PLFormat::push(db, diff.into_text_edit(&line_index));
            }
            ActionType::Flow => {
                if let NodeEnum::Program(pro) = *nn {
                    // create dot dir
                    // let mut dotpath = PathBuf::from(p.dir(db));
                    let mut dotpath = PathBuf::from("./");
                    dotpath.push("dots");
                    if !dotpath.exists() {
                        std::fs::create_dir(dotpath.clone()).unwrap();
                    }
                    let graphs = pro.create_graphs();
                    for graph in graphs {
                        let dot = Dot::new(true); // true 曲线; false 折线
                        let dot_str = dot.generate_from_graph(&graph.graph, &graph.name);
                        // write to file
                        let path = format!(
                            "{}/{}_{}.dot",
                            dotpath.to_str().unwrap(),
                            p.fullpath(db)
                                .replace(|c: char| { !c.is_ascii_alphanumeric() }, "_")
                                .replace(".pi", ""),
                            graph.name
                        );
                        let mut file = fs::File::create(path.clone()).unwrap();
                        file.write_all(dot_str.as_bytes()).unwrap();
                        println!("{} {}", "Written to file".bright_cyan(), path.green());
                        print!("{}", dot_str);
                        println!("{}","You can view the flow chart on https://dreampuf.github.io/GraphvizOnline\n".bright_cyan());
                    }
                }
            }
            _ => {}
        }
        pb.set_message(format!("正在编译包{}", pkgname));
        pb.inc(1);
        // pb.set_prefix(format!("[{:3}/{:3}]", pb.position(), pb.length().unwrap()));
        let m = emit_file(db, p);
        let plmod = m.plmod(db);
        let params = self.params(db);
        if is_active_file {
            if pos.is_some() {
                Completions::push(
                    db,
                    plmod
                        .completions
                        .borrow()
                        .iter()
                        .map(|x| x.0.clone())
                        .collect(),
                );
            }
            let hints = plmod.hints.borrow().clone();
            Hints::push(db, hints);
            let docs = plmod.doc_symbols.borrow().clone();
            DocSymbols::push(db, docs);
        }
        match params.action(db) {
            ActionType::FindReferences => {
                let (pos, _) = params.params(db).unwrap();
                let range = pos.to(pos);
                let res = plmod.local_refs.borrow();
                let re = res.range((Unbounded, Included(&range))).last();
                let mut pushed = false;
                if let Some((range, res)) = re {
                    if pos.is_in(*range) {
                        PLReferences::push(db, res.borrow().clone());
                        pushed = true;
                        db.set_ref_str(None);
                    }
                }
                if !pushed {
                    let res = plmod.glob_refs.borrow();
                    let re = res.range((Unbounded, Included(&range))).last();
                    if let Some((range, res)) = re {
                        if pos.is_in(*range) {
                            db.set_ref_str(Some(res.clone()));
                        }
                    }
                }
            }
            ActionType::GotoDef => {
                let (pos, _) = params.params(db).unwrap();
                let range = pos.to(pos);
                let res = plmod.defs.borrow();
                let re = res.range((Unbounded, Included(&range))).last();
                if let Some((range, LSPDef::Scalar(def))) = re {
                    if pos.is_in(*range) {
                        GotoDef::push(db, GotoDefinitionResponse::Scalar(def.clone()));
                    }
                }
            }
            ActionType::SignatureHelp => {
                let (pos, _) = params.params(db).unwrap();
                let range = pos.to(pos);
                let res = plmod.sig_helps.borrow();
                let re = res.range((Unbounded, Included(&range))).last();
                if let Some((range, res)) = re {
                    if pos.is_in(*range) {
                        PLSignatureHelp::push(db, res.clone());
                    }
                }
            }
            ActionType::Hover => {
                let (pos, _) = params.params(db).unwrap();
                let range = pos.to(pos);
                let res = plmod.hovers.borrow();
                let re = res.range((Unbounded, Included(&range))).last();
                if let Some((range, res)) = re {
                    if pos.is_in(*range) {
                        PLHover::push(db, res.clone());
                    }
                }
            }
            ActionType::SemanticTokensFull => {
                let b = plmod.semantic_tokens_builder.borrow().build();
                PLSemanticTokens::push(db, b);
            }
            _ => {}
        }
        m
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
    pub params: LspParams,
    pub submods: FxHashMap<String, Mod>,
    #[return_ref]
    pub file_content: String,
}

#[salsa::tracked]
pub struct LspParams {
    #[return_ref]
    pub modpath: String,
    pub params: Option<Pos>,
    pub config: Config,
    pub is_compile: bool,
}

#[salsa::tracked(lru = 32)]
pub fn emit_file(db: &dyn Db, params: ProgramEmitParam) -> ModWrapper {
    log::info!("emit_file: {}", params.fullpath(db),);
    let context = &Context::create();
    let (a, b, c, d, e) = create_llvm_deps(context, params.dir(db), params.file(db));
    let v = RefCell::new(FxHashSet::default());
    let builder = LLVMBuilder::new(context, &a, &b, &c, &d, &e);
    let mut ctx = ctx::Ctx::new(
        params.fullpath(db),
        &v,
        params.params(db).params(db),
        params.params(db).config(db),
        db,
    );
    ctx.plmod.submods = params.submods(db);
    let m = &mut ctx;
    let node = params.node(db);
    let mut nn = node.node(db);
    let mut builder = &builder.into();
    let noop = NoOpBuilder::default();
    let noop = noop.into();
    if !params.params(db).is_compile(db) {
        log::info!("not in compile mode, using no-op builder");
        builder = &noop;
    }
    let _ = nn.emit(m, builder);
    Diagnostics::push(
        db,
        (
            params.fullpath(db).clone(),
            v.borrow().iter().cloned().collect(),
        ),
    );
    if params.params(db).is_compile(db) {
        builder.finalize_debug();
        let mut hasher = DefaultHasher::new();
        params.fullpath(db).hash(&mut hasher);
        let hashed = format!(
            "target/{}_{:x}",
            Path::new(&params.file(db))
                .with_extension("")
                .to_str()
                .unwrap(),
            hasher.finish()
        );
        let pp = Path::new(&hashed).with_extension("bc");
        let ll = Path::new(&hashed).with_extension("ll");
        let p = pp.as_path();
        builder.write_bitcode_to_path(p);
        builder.print_to_file(&ll).unwrap();
        ModBuffer::push(
            db,
            PLModBuffer {
                path: p.clone().to_path_buf(),
                is_main: builder.get_function("main").is_some(),
            },
        );
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

/// 尽管实际上Mod并不是线程安全的，但是它的使用特性导致
/// 他的内容实际上几乎只会在生成的mod里被修改，当他作为依赖项
/// 给别的mod使用的时候几乎是只读的，所以它不需要真的线程安全，
/// 只需要实现接口来骗过rust
unsafe impl Send for Mod {}

unsafe impl Sync for Mod {}
