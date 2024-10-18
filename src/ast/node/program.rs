#![allow(clippy::too_many_arguments)]
use super::function::FuncDefNode;
use super::types::StructDefNode;
use super::*;
use crate::ast::accumulators::*;
#[cfg(feature = "llvm")]
use crate::ast::builder::llvmbuilder::create_llvm_deps;
#[cfg(feature = "llvm")]
use crate::ast::builder::llvmbuilder::LLVMBuilder;
use crate::ast::builder::no_op_builder::NoOpBuilder;
use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::ast::compiler::CHECK_PROGRESS;
use crate::ast::compiler::COMPILE_PROGRESS;
use crate::ast::compiler::{compile_dry_file, ActionType};
use crate::ast::ctx::{self, Ctx};
use crate::ast::plmod::GlobalType;
use crate::ast::plmod::LSPDef;
use crate::ast::plmod::Mod;
use crate::ast::pltype::add_primitive_types;
use crate::ast::pltype::FNValue;
use crate::ast::tokens::TokenType;
use crate::flow::display::Dot;
use crate::format_label;
use crate::lsp::semantic_tokens::SemanticTokensBuilder;
use crate::lsp::text;
#[cfg(feature = "repl")]
use crate::repl::LOADED_SET;
#[cfg(feature = "repl")]
use crate::repl::REPL_VIRTUAL_ENTRY;
use crate::utils::read_config::ConfigWrapper;
use crate::Db;
use colored::Colorize;
use indicatif::ProgressBar;
#[cfg(feature = "llvm")]
use inkwell::context::Context;

use internal_macro::node;
use lazy_static::lazy_static;
use lsp_types::GotoDefinitionResponse;
use rustc_hash::FxHashMap;
use rustc_hash::FxHashSet;
use salsa::Accumulator;
use std::cell::RefCell;
use std::collections::hash_map::DefaultHasher;
use std::fs;
use std::fs::OpenOptions;
use std::hash::{Hash, Hasher};
use std::io::prelude::*;
use std::ops::Bound::*;
use std::path::{Path, PathBuf};
use ustr::ustr;
use ustr::Ustr;

use std::sync::Arc;
use std::sync::Mutex;

/// ProgramNode is an AST representation of a file
#[node]
pub struct ProgramNode {
    /// nodes stores all NodeEnum nodes in the file
    pub nodes: Vec<Box<NodeEnum>>,

    pub structs: Vec<StructDefNode>,
    pub fntypes: Vec<FuncDefNode>,
    pub globaldefs: Vec<GlobalNode>,
    /// uses stores all dependencies used by the program node
    pub uses: Vec<Box<NodeEnum>>,
    pub traits: Vec<TraitDefNode>,
    pub trait_impls: Vec<ImplNode>,
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

lazy_static! {
    pub static ref ASSET_PATH: Mutex<String> = Mutex::new("target".to_string());
}

impl Node for ProgramNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
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
        for def in self.unions.iter() {
            // 提前加入占位符号，解决自引用问题
            def.add_to_symbols(ctx, builder);
        }
        for def in self.structs.iter_mut() {
            _ = def.emit_struct_def(ctx, builder);
        }
        for def in self.traits.iter_mut() {
            _ = def.emit_trait_def(ctx, builder);
        }
        self.trait_impls.iter().for_each(|x| {
            _ = x.add_impl_to_ctx(ctx, builder);
        });
        self.fntypes.iter_mut().for_each(|x| {
            _ = x.emit_func_def(ctx, builder);
        });

        // init global
        ctx.set_init_fn(builder);
        self.globaldefs.iter_mut().for_each(|x| {
            _ = x.emit_global(ctx, builder);
        });
        ctx.plmod.semantic_tokens_builder = Arc::new(RefCell::new(Box::new(
            SemanticTokensBuilder::new(ctx.plmod.path.to_string()),
        )));

        // emit information for all nodes
        self.nodes.iter_mut().for_each(|x| {
            if x.range() == Default::default() {
                *ctx.need_highlight.borrow_mut() = 1;
                ctx.disable_diag = true;
            }
            _ = x.emit(ctx, builder);
            ctx.disable_diag = false;
            *ctx.need_highlight.borrow_mut() = 0;
        });
        ctx.init_fn_ret(builder);
        Ok(Default::default())
    }
}

fn new_var(name: &str) -> Box<VarNode> {
    Box::new(VarNode {
        name: ustr(name),
        range: Default::default(),
        id: None,
    })
}
fn new_use(ns: &[&str], all: bool) -> Box<NodeEnum> {
    Box::new(NodeEnum::UseNode(UseNode {
        namespace: ns.iter().map(|a| new_var(a)).collect(),
        range: Default::default(),
        complete: true,
        singlecolon: false,
        modifier: None,
        all_import: all,
    }))
}

lazy_static::lazy_static! {
    static ref DEFAULT_USE_NODES: Vec<Box<NodeEnum>> = {
        vec![
            new_use(&["core", "builtin"], true),
            new_use(&["core", "hash"], false),
            new_use(&["core", "eq"], false),
            new_use(&["std", "stdbuiltin"], true)
        ]
    };

    static ref GC_USE_NODES: Vec<Box<NodeEnum>> = {
        vec![new_use(&["core", "gc"], false)]
    };

    static ref STD_USE_NODES: Vec<Box<NodeEnum>> = {
        vec![new_use(&["core", "builtin"], true)]
    };
}

fn import_symbol(
    s: Ustr,
    x: &GlobalType,
    re_export: bool,
    global_tp_map: &mut FxHashMap<Ustr, GlobalType>,
    global_mthd_map: &mut FxHashMap<Ustr, FxHashMap<Ustr, Arc<RefCell<FNValue>>>>,
) {
    if x.visibal_outside() {
        global_tp_map.insert(
            s,
            GlobalType {
                is_extern: true,
                re_export,
                typ: x.typ.to_owned(),
            },
        );
    }
    if let PLType::Trait(t) = &*x.borrow() {
        for (k, v) in t.trait_methods_impl.borrow().clone() {
            for (k2, v) in v {
                global_mthd_map.entry(k).or_default().insert(k2, v);
            }
        }
    }
}

#[salsa::tracked]
impl<'db> Program<'db> {
    /// # is_active_file
    ///
    /// it checks whether the current file is focused in the editor as an active file
    #[salsa::tracked]
    pub(crate) fn is_active_file(self, db: &'db dyn Db) -> bool {
        let entry_file = self.docs(db).file(db);
        let focused_file = self.params(db).file(db);
        crate::utils::canonicalize(entry_file).unwrap()
            == crate::utils::canonicalize(focused_file).unwrap()
    }

    /// # get_pkgname
    ///
    /// get the package name according to the building file path
    fn get_pkgname(self, db: &'db dyn Db) -> String {
        let binding = PathBuf::from(self.params(db).file(db)).with_extension("");
        let pkgname = binding.file_name().unwrap().to_str().unwrap();
        pkgname.to_string()
    }

    /// # guard_and_load_buitin_modules
    ///
    /// guard the entry node of the [Program] is a [NodeEnum::Program] node,
    /// and then loads built-in module and gc module.
    pub fn guard_and_load_buitin_modules(self, db: &'db dyn Db) -> ProgramNode {
        let mut entry_node = match *self.entry_node(db).node(db) {
            NodeEnum::Program(p) => p,
            _ => panic!("not a program"),
        };

        // add biltin module by default
        if self.config(db).project == "std" {
            entry_node.uses.extend_from_slice(&STD_USE_NODES);
        }
        if self.config(db).project != "core" && self.config(db).project != "std" {
            entry_node.uses.extend_from_slice(&DEFAULT_USE_NODES);
        }

        let pkgname = self.get_pkgname(db);
        // add gc by default
        if pkgname != "gc" {
            entry_node.uses.extend_from_slice(&GC_USE_NODES);
        }

        entry_node
    }

    /// # load_used_modules
    ///
    /// load_used_modules loads each dependent module used by the entry_node, parses them into modules by [compile_dry_file],
    /// loads all the symbols into the entry_node and returns ProgramEmitParam for the further processing.
    pub fn load_used_modules(self, db: &'db dyn Db, entry_node: ProgramNode) -> ProgramEmitParam {
        let mut modmap = FxHashMap::<Ustr, Arc<Mod>>::default();
        let mut global_mthd_map: FxHashMap<Ustr, FxHashMap<Ustr, Arc<RefCell<FNValue>>>> =
            FxHashMap::default();
        let mut global_tp_map = FxHashMap::default();
        let mut global_macro_map = FxHashMap::default();

        let pkgname = self.get_pkgname(db);
        let (job, pb) = if self.params(db).action(db) == ActionType::Compile {
            ("编译", &COMPILE_PROGRESS as &ProgressBar)
        } else {
            ("检查", &CHECK_PROGRESS as &ProgressBar)
        };
        let parents = self.parent_mods(db);
        // parse all dependencies into modules and process symbols into the main module symbol table
        for (i, u) in entry_node.uses.iter().enumerate() {
            let mut parents = parents.clone();
            parents
                .0
                .insert(self.params(db).file(db).to_string(), u.range());
            #[cfg(not(target_arch = "wasm32"))]
            pb.set_message(format!(
                "正在{}包{}的依赖项{}/{}",
                job,
                pkgname,
                i,
                entry_node.uses.len()
            ));

            #[cfg(not(target_arch = "wasm32"))]
            pb.inc(1);

            let use_statemet_node = match *u.clone() {
                NodeEnum::UseNode(p) => p,
                // skip the loop if it's not a use statement node
                _ => continue,
            };

            let re_export = use_statemet_node
                .modifier
                .map(|(t, _)| t == TokenType::PUB)
                .unwrap_or_default();

            let wrapper = ConfigWrapper::new(db, self.config(db), use_statemet_node);
            let dep_path = wrapper.resolve_dep_path(db);
            let p = self.config(db).project;
            log::trace!(
                "load dep {:?} for {:?} (project {:?})",
                dep_path,
                pkgname,
                p
            );

            let mut mod_id = wrapper.use_node(db).get_last_id();

            let dep_path_str = dep_path.to_str().unwrap().to_string();
            if parents.0.contains_key(&dep_path_str) {
                let mut diag = u.range().new_err(ErrorCode::CYCLE_DEPENDENCY);
                diag.set_source(&dep_path_str);
                for (f, r) in parents.0.iter() {
                    let msg = "import in cycle here";
                    diag.add_label(*r, ustr(f), format_label!(msg));
                }
                Diagnostics((dep_path_str.clone(), vec![diag])).accumulate(db);
                continue;
            }
            parents.0.insert(dep_path_str.clone(), u.range());
            let mut dep_parser_entry = self.docs(db).finalize_parser_input(
                db,
                dep_path_str.clone(),
                false,
                parents.clone(),
            );

            #[cfg(target_arch = "wasm32")]
            if dep_path_str.starts_with("core") || dep_path_str.starts_with("std") {
                let p = crate::lsp::wasm::PLLIB_DIR.get_entry(&dep_path_str);
                if p.is_none() {
                    dep_parser_entry = None;
                }
            }

            let mut symbol_opt = None;
            if dep_parser_entry.is_none() {
                if let Some(p) = dep_path.parent() {
                    mod_id = Some(p.file_name().unwrap().to_str().unwrap().to_string().into());
                    let file = p.with_extension("pi").to_str().unwrap().to_string();
                    dep_parser_entry = self
                        .docs(db)
                        .finalize_parser_input(db, file, false, parents);
                    symbol_opt = Some(
                        dep_path
                            .with_extension("")
                            .file_name()
                            .unwrap()
                            .to_str()
                            .unwrap()
                            .to_string()
                            .into(),
                    );
                    if dep_parser_entry.is_none() {
                        continue;
                    }
                } else {
                    continue;
                }
            }

            let dep_parser_entry = dep_parser_entry.unwrap();
            let dep_module: Option<ModWrapper> = compile_dry_file(db, dep_parser_entry);
            if dep_module.is_none() {
                continue;
            }
            let dep_module = dep_module.unwrap().plmod(db);

            // loads sub_module symbols into the current module
            if let Some(s) = symbol_opt {
                let symbol = dep_module.types.get(&s);
                if let Some(x) = symbol {
                    import_symbol(s, x, re_export, &mut global_tp_map, &mut global_mthd_map);
                }
                let mac = dep_module.macros.get(&s);
                if let Some(x) = mac {
                    global_macro_map.insert(s, x.clone());
                }
            }
            if wrapper.use_node(db).all_import {
                for (s, x) in dep_module.types.iter() {
                    import_symbol(*s, x, re_export, &mut global_tp_map, &mut global_mthd_map);
                }
            }

            // insert the dep module into the current module map
            modmap.insert(mod_id.unwrap(), Arc::new(dep_module));
        }

        log::trace!("done deps compile");
        let filepath = Path::new(self.params(db).file(db));
        let abs = crate::utils::canonicalize(filepath).unwrap();
        let dir = abs.parent().unwrap().to_str().unwrap();
        let fname = abs.file_name().unwrap().to_str().unwrap();

        // 不要修改这里，除非你知道自己在干什么
        // 除了当前用户正打开的文件外，其他文件的编辑位置都输入None，这样可以保证每次用户修改的时候，
        // 未被修改的文件的`emit_file`参数与之前一致，不会被重新分析
        // 修改这里可能导致所有文件被重复分析，从而导致lsp性能下降
        let pos = if self.is_active_file(db) {
            self.docs(db).edit_pos(db)
        } else {
            None
        };

        let params = self.params(db);

        ProgramEmitParam::new(
            db,
            self.entry_node(db),
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
            UnsafeWrapper::new(global_tp_map),
            UnsafeWrapper::new(global_mthd_map),
            UnsafeWrapper::new(global_macro_map),
            self.is_active_file(db),
            self.opt(db),
            self.docs(db).op(db).debug,
            self.docs(db).op(db).print_escape,
        )
    }

    /// # emit
    ///
    /// `emit` function analyzes all submodules used by the current program,
    /// resolves all symbols from submodules into the current one,
    /// and compiles the current module with all sub-modules into LLVM IR or does some LSP works
    #[salsa::tracked]
    pub fn emit(self, db: &'db dyn Db) -> ModWrapper<'db> {
        #[cfg(not(target_arch = "wasm32"))]
        let (job, pb) = if self.params(db).action(db) == ActionType::Compile {
            ("编译", &COMPILE_PROGRESS as &ProgressBar)
        } else {
            ("检查", &CHECK_PROGRESS as &ProgressBar)
        };

        let entry_node = self.guard_and_load_buitin_modules(db);

        #[cfg(not(target_arch = "wasm32"))]
        match pb.length() {
            None => pb.set_length(1 + entry_node.uses.len() as u64),
            _ => pb.inc_length(1 + entry_node.uses.len() as u64),
        }

        let pkgname: String = self.get_pkgname(db);

        let emit_params = self.load_used_modules(db, entry_node);
        let raw_node = emit_params.program_node(db).node(db);

        #[cfg(not(target_arch = "wasm32"))]
        pb.set_message(format!("正在{}包{}", job, pkgname));
        #[cfg(not(target_arch = "wasm32"))]
        pb.inc(1);

        // the actual compilation happens here
        let m = emit_file(db, emit_params);
        let plmod = m.plmod(db);

        let pos = emit_params.lsp_params(db).editing_postion(db);
        #[cfg(feature = "repl")]
        let or_cond = plmod.path == REPL_VIRTUAL_ENTRY;
        #[cfg(not(feature = "repl"))]
        let or_cond = false;
        if self.is_active_file(db) || or_cond {
            if pos.is_some() {
                Completions(
                    plmod
                        .completions
                        .borrow()
                        .iter()
                        .map(|x| x.0.clone())
                        .collect(),
                )
                .accumulate(db);
            }
            let hints = plmod.hints.borrow().clone();
            Hints(hints).accumulate(db);
            let docs = plmod.doc_symbols.borrow().clone();
            DocSymbols(docs).accumulate(db);
            let b = plmod.semantic_tokens_builder.borrow().build();
            PLSemanticTokens(b).accumulate(db);
        }

        self.handle_actions(db, emit_params, raw_node, m);
        m
    }

    /// # handle_actions
    ///
    /// it handles one of actions for compiler or LSP features
    fn handle_actions(
        self,
        db: &'db dyn Db,
        p: ProgramEmitParam,
        nn: Box<NodeEnum>,
        m: ModWrapper,
    ) {
        let params = self.params(db);
        let plmod = m.plmod(db);
        match params.action(db) {
            ActionType::PrintAst => {
                println!("file: {}", p.fullpath(db).green());
                nn.print(0, true, vec![]);
            }
            ActionType::Fmt => {
                let mut builder = FmtBuilder::default();
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
                let mut builder = FmtBuilder::default();
                nn.format(&mut builder);
                let newcode = builder.generate();
                let diff = text::diff(oldcode, &newcode);
                let line_index = text::LineIndex::new(oldcode);
                PLFormat(diff.into_text_edit(&line_index)).accumulate(db);
            }
            ActionType::Flow => {
                if let NodeEnum::Program(pro) = *nn {
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
            ActionType::FindReferences => {
                let (pos, _) = params.params(db).unwrap();
                let range = pos.to(pos);
                let res = plmod.local_refs.borrow();
                let re = res.range((Unbounded, Included(&range))).last();
                let mut pushed = false;
                if let Some((range, res)) = re {
                    if pos.is_in(*range) {
                        PLReferences(res.borrow().clone()).accumulate(db);
                        pushed = true;
                        db.set_ref_str(None);
                    }
                }
                if !pushed {
                    let res = plmod.glob_refs.borrow();
                    let re = res.range((Unbounded, Included(&range))).last();
                    if let Some((range, res)) = re {
                        if pos.is_in(*range) {
                            db.set_ref_str(Some(*res));
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
                        GotoDef(GotoDefinitionResponse::Scalar(def.clone())).accumulate(db);
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
                        PLSignatureHelp(res.clone()).accumulate(db);
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
                        PLHover(res.clone()).accumulate(db);
                    }
                }
            }
            _ => {}
        }
    }
}

pub use salsa_structs::*;
mod salsa_structs;

/// # prepare_module_ctx
///
/// prepare_module_ctx prepares the [Ctx] for the program based on the emitting parameters
pub fn prepare_module_ctx<'a>(
    db: &'a dyn Db,
    program_emit_params: &'a ProgramEmitParam,
    v: &'a RefCell<FxHashSet<PLDiag>>,
) -> Ctx<'a> {
    let mut ctx = ctx::Ctx::new(
        program_emit_params.fullpath(db),
        v,
        program_emit_params.lsp_params(db).editing_postion(db),
        program_emit_params.lsp_params(db).config(db),
        db,
        program_emit_params.is_active_file(db),
    );
    ctx.plmod.trait_mthd_table = Arc::new(RefCell::new(
        program_emit_params.mth_table(db).get().clone(),
    ));
    ctx.plmod.types = program_emit_params.types(db).get().clone();
    ctx.plmod.macros = program_emit_params.macro_table(db).get().clone();
    add_primitive_types(&mut ctx);
    ctx.plmod.submods = program_emit_params.submods(db);

    // imports all builtin symbols
    if let Some(builtin_mod) = ctx.plmod.submods.get(&"builtin".into()).cloned() {
        ctx.plmod.import_all_symbols_from(&builtin_mod);
    }
    if let Some(builtin_mod) = ctx.plmod.submods.get(&"stdbuiltin".into()).cloned() {
        ctx.plmod.import_all_symbols_from(&builtin_mod);
    }
    ctx.import_all_infer_maps_from_sub_mods();
    ctx
}

/// # emit_file
///
/// emit_file generates the LLVM IR for a pi file based on the [program_emit_params],
/// or it does some LSP operations.
#[salsa::tracked]
pub fn emit_file<'db>(
    db: &'db dyn Db,
    program_emit_params: ProgramEmitParam<'db>,
) -> ModWrapper<'db> {
    log::info!("Compiling: {}", program_emit_params.fullpath(db),);

    let v = RefCell::new(FxHashSet::default());
    let mut ctx = prepare_module_ctx(db, &program_emit_params, &v);
    ctx.origin_mod = &ctx.plmod as _;

    let mctx = &mut ctx;

    #[cfg(feature = "llvm")]
    let context = &Context::create();
    #[cfg(feature = "llvm")]
    let (a, b, c, d, e) = create_llvm_deps(
        context,
        program_emit_params.dir(db),
        program_emit_params.file(db),
        program_emit_params.opt(db).to_llvm(),
    );
    let builder = {
        if !program_emit_params.lsp_params(db).is_compile(db) {
            let noop = NoOpBuilder::default();
            noop.into()
        } else {
            #[cfg(not(feature = "llvm"))]
            unreachable!("llvm feature is not enabled");
            #[cfg(feature = "llvm")]
            {
                let builder = LLVMBuilder::new(
                    context,
                    &a,
                    &b,
                    &c,
                    &d,
                    &e,
                    program_emit_params.opt(db).to_llvm(),
                    program_emit_params.debug(db),
                    program_emit_params.print_escaped(db),
                );
                builder.into()
            }
        }
    };

    // program is a [program::ProgramNode] enum
    let mut program_node = program_emit_params.program_node(db).node(db);
    let _ = program_node.emit(mctx, &builder);

    Diagnostics((
        program_emit_params.fullpath(db).clone(),
        ctx.diagnose.borrow().iter().cloned().collect(),
    ))
    .accumulate(db);
    if program_emit_params.lsp_params(db).is_compile(db) {
        builder.finalize_debug();
        let mut hasher = DefaultHasher::new();
        program_emit_params.fullpath(db).hash(&mut hasher);
        let hashed = format!(
            "{}/{}_{:x}",
            &ASSET_PATH.lock().unwrap(),
            Path::new(&program_emit_params.file(db))
                .with_extension("")
                .to_str()
                .unwrap(),
            hasher.finish()
        );
        let pp = Path::new(&hashed).with_extension("bc");
        // let ll = Path::new(&hashed).with_extension("ll");
        let p = pp.as_path();
        // builder.optimize();
        // builder.print_to_file(&ll).unwrap();
        // builder.write_bitcode_to_path(p);
        #[cfg(feature = "llvm")]
        let buf = a.write_bitcode_to_memory().as_slice().to_vec();
        #[cfg(not(feature = "llvm"))]
        let buf = vec![];
        #[cfg(feature = "repl")]
        LOADED_SET.lock().unwrap().remove(&p.to_path_buf());
        ModBuffer(PLModBuffer {
            path: p.to_path_buf(),
            buf,
            name: program_emit_params.file(db).to_string(),
        })
        .accumulate(db);
    }

    db.add_module(ctx.get_file(), ctx.plmod.clone());
    ModWrapper::new(db, ctx.plmod)
}

/// 尽管实际上Mod并不是线程安全的，但是它的使用特性导致
/// 他的内容实际上几乎只会在生成的mod里被修改，当他作为依赖项
/// 给别的mod使用的时候几乎是只读的，所以它不需要真的线程安全，
/// 只需要实现接口来骗过rust
unsafe impl Send for Mod {}

unsafe impl Sync for Mod {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnsafeWrapper<T>(T);

unsafe impl<T> Send for UnsafeWrapper<T> {}
unsafe impl<T> Sync for UnsafeWrapper<T> {}
impl<T> UnsafeWrapper<T> {
    pub fn new(t: T) -> Self {
        Self(t)
    }
    pub fn get(&self) -> &T {
        &self.0
    }
}
