use super::function::FuncDefNode;
use super::types::StructDefNode;
use super::*;
use crate::ast::accumulators::*;
use crate::ast::compiler::{compile_dry_file, ActionType};
use crate::ast::ctx::{self, create_ctx_info, Ctx, LSPDef, Mod};
use crate::lsp::mem_docs::{EmitParams, FileCompileInput, MemDocsInput};
use crate::lsp::semantic_tokens::SemanticTokensBuilder;
use crate::lsp::text;
use crate::utils::read_config::{get_config, Config};
use crate::Db;
use colored::Colorize;
use inkwell::context::Context;
use inkwell::targets::TargetMachine;
use internal_macro::{fmt, range};
use lsp_types::GotoDefinitionResponse;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::collections::hash_map::DefaultHasher;
use std::fs::OpenOptions;
use std::hash::{Hash, Hasher};
use std::io::prelude::*;
use std::ops::Bound::*;
use std::path::{Path, PathBuf};
use std::rc::Rc;

#[range]
#[fmt]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ProgramNode {
    pub nodes: Vec<Box<NodeEnum>>,
    pub structs: Vec<StructDefNode>,
    pub fntypes: Vec<FuncDefNode>,
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
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        // emit structs
        for def in self.structs.iter() {
            // 提前加入占位符号，解决自引用问题
            def.add_to_symbols(ctx);
        }
        for def in self.structs.iter_mut() {
            _ = def.emit_struct_def(ctx);
        }
        self.fntypes.iter_mut().for_each(|x| {
            _ = x.emit_func_def(ctx);
        });
        // init global
        ctx.set_init_fn();
        self.globaldefs.iter_mut().for_each(|x| {
            _ = x.emit_global(ctx);
        });
        ctx.clear_init_fn();
        ctx.plmod.semantic_tokens_builder = Rc::new(RefCell::new(Box::new(
            SemanticTokensBuilder::new(ctx.plmod.path.to_string()),
        )));
        // node parser
        self.nodes.iter_mut().for_each(|x| {
            _ = x.emit(ctx);
        });
        ctx.init_fn_ret();
        Ok((None, None, TerminatorEnum::NONE))
    }
}

#[salsa::interned]
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
                }
            }
            for p in u.ids[1..].iter() {
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
        let params = self.params(db);
        // 不要修改这里，除非你知道自己在干什么
        // 除了当前用户正打开的文件外，其他文件的编辑位置都输入None，这样可以保证每次用户修改的时候，
        // 未被修改的文件的`emit_file`参数与之前一致，不会被重新分析
        // 修改这里可能导致所有文件被重复分析，从而导致lsp性能下降
        let pos = if self.docs(db).file(db) == params.file(db) {
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
            self.docs(db).get_file_content(db).unwrap().text(db).clone(),
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
                f.write(code.as_bytes()).unwrap();
            }
            ActionType::LspFmt => {
                let oldcode = p.file_content(db);
                let mut builder = FmtBuilder::new();
                nn.format(&mut builder);
                let newcode = builder.generate();
                let diff = text::diff(&oldcode, &newcode);
                let line_index = text::LineIndex::new(&oldcode);
                PLFormat::push(db, diff.into_text_edit(&line_index));
            }
            _ => {}
        }
        let m = emit_file(db, p);
        let plmod = m.plmod(db);
        let params = self.params(db);
        if self.docs(db).file(db) == params.file(db) {
            if pos.is_some() {
                Completions::push(
                    db,
                    plmod
                        .completions
                        .borrow()
                        .iter()
                        .map(|x| x.into_completions())
                        .collect(),
                );
            }
            let hints = plmod.hints.borrow().clone();
            Hints::push(db, *hints);
            let docs = plmod.doc_symbols.borrow().clone();
            DocSymbols::push(db, *docs);
        }
        match params.action(db) {
            ActionType::FindReferences => {
                let (pos, _) = params.params(db).unwrap();
                let range = pos.to(pos);
                let res = plmod.refs.borrow();
                let re = res.range((Unbounded, Included(&range))).last();
                if let Some((range, res)) = re {
                    if pos.is_in(*range) {
                        PLReferences::push(db, res.clone());
                    }
                }
            }
            ActionType::GotoDef => {
                let (pos, _) = params.params(db).unwrap();
                let range = pos.to(pos);
                let res = plmod.defs.borrow();
                let re = res.range((Unbounded, Included(&range))).last();
                if let Some((range, res)) = re {
                    if let LSPDef::Scalar(def) = res {
                        if pos.is_in(*range) {
                            GotoDef::push(db, GotoDefinitionResponse::Scalar(def.clone()));
                        }
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
        params.params(db).params(db),
        params.params(db).config(db),
        db,
    );
    if PathBuf::from(params.file(db))
        .with_extension("")
        .file_name()
        .unwrap()
        .to_str()
        .unwrap()
        == "gc"
    {
        ctx.usegc = false;
    }
    ctx.plmod.submods = params.submods(db);
    let m = &mut ctx;
    m.module.set_triple(&TargetMachine::get_default_triple());
    let node = params.node(db);
    let mut nn = node.node(db);
    let _ = nn.emit(m);
    Diagnostics::push(
        db,
        (
            params.fullpath(db).clone(),
            v.borrow().iter().map(|x| x.clone()).collect(),
        ),
    );
    if params.params(db).is_compile(db) {
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
        let ll = Path::new(&hashed).with_extension("ll");
        let p = pp.as_path();
        ctx.module.print_to_file(ll).unwrap();
        ctx.module.write_bitcode_to_path(p);
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
