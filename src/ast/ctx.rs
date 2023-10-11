use super::builder::no_op_builder::NoOpBuilder;
use super::builder::BlockHandle;
use super::builder::ValueHandle;
use super::diag::ErrorCode;
use super::diag::PLDiag;

use super::node::function::generator::ClosureCtxData;
use super::node::function::generator::CtxFlag;
use super::node::macro_nodes::MacroNode;
use super::node::node_result::NodeResult;
use super::node::NodeEnum;
use super::plmod::GlobType;
use super::plmod::GlobalVar;
use super::plmod::Mod;
use super::plmod::MutVec;
use super::pltype::add_primitive_types;
use super::pltype::get_type_deep;
use super::pltype::FNValue;
use super::pltype::GenericType;
use super::pltype::ImplAble;
use super::pltype::PLType;
use super::pltype::TraitImplAble;
use super::range::Pos;
use super::range::Range;

use super::traits::CustomType;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::format_label;

use crate::utils::read_config::Config;

use crate::Db;

use crate::ast::node::function::generator::GeneratorCtxData;
use indexmap::IndexMap;

use lsp_types::CompletionItem;

use lsp_types::Location;

use lsp_types::Url;
use rustc_hash::FxHashMap;
use rustc_hash::FxHashSet;

use std::cell::RefCell;

use std::sync::Arc;
mod builtins;
mod references;

pub use builtins::*;
#[derive(Clone, Debug)]
pub struct PLSymbolData {
    pub value: ValueHandle,
    pub pltype: Arc<RefCell<PLType>>,
    pub range: Range,
    pub refs: Option<Arc<MutVec<Location>>>,
}

#[derive(Clone)]
pub enum PLSymbol {
    Local(PLSymbolData),
    Global(PLSymbolData),
    Captured(PLSymbolData),
}

impl PLSymbol {
    pub fn is_global(&self) -> bool {
        matches!(self, PLSymbol::Global(_))
    }
    pub fn is_captured(&self) -> bool {
        matches!(self, PLSymbol::Captured(_))
    }
    pub fn get_data_ref(&self) -> &PLSymbolData {
        match self {
            PLSymbol::Local(d) => d,
            PLSymbol::Global(d) => d,
            PLSymbol::Captured(d) => d,
        }
    }
    pub fn get_data(self) -> PLSymbolData {
        match self {
            PLSymbol::Local(d) => d,
            PLSymbol::Global(d) => d,
            PLSymbol::Captured(d) => d,
        }
    }
}

type GenericCacheMap = IndexMap<String, Arc<RefCell<IndexMap<String, Arc<RefCell<PLType>>>>>>;

pub type GenericCache = Arc<RefCell<GenericCacheMap>>;

/// # Ctx
/// Context for code generation
pub struct Ctx<'a> {
    pub generic_types: FxHashMap<String, Arc<RefCell<PLType>>>,
    pub need_highlight: Arc<RefCell<usize>>,
    pub plmod: Mod,
    pub father: Option<&'a Ctx<'a>>, // father context, for symbol lookup
    pub root: Option<&'a Ctx<'a>>,   // root context, for symbol lookup
    pub function: Option<ValueHandle>, // current function
    pub init_func: Option<ValueHandle>, //init function,call first in main
    pub block: Option<BlockHandle>,  // current block
    pub continue_block: Option<BlockHandle>, // the block to jump when continue
    pub break_block: Option<BlockHandle>, // the block to jump to when break
    pub return_block: Option<(BlockHandle, Option<ValueHandle>)>, // the block to jump to when return and value
    pub errs: &'a RefCell<FxHashSet<PLDiag>>,                     // diagnostic list
    pub edit_pos: Option<Pos>,                                    // lsp params
    pub table: FxHashMap<String, PLSymbolData>,                   // variable table
    pub config: Config,                                           // config
    pub db: &'a dyn Db,
    pub rettp: Option<Arc<RefCell<PLType>>>,
    pub macro_vars: FxHashMap<String, MacroReplaceNode>,
    pub macro_skip_level: usize,
    pub macro_loop: bool,
    pub macro_loop_idx: usize,
    pub macro_loop_len: usize,
    pub temp_source: Option<String>,
    pub in_macro: bool,
    pub closure_data: Option<RefCell<ClosureCtxData>>,
    pub expect_ty: Option<Arc<RefCell<PLType>>>,
    pub self_ref_map: FxHashMap<String, FxHashSet<(String, Range)>>, // used to recognize self reference
    pub ctx_flag: CtxFlag,
    pub generator_data: Option<Arc<RefCell<GeneratorCtxData>>>,
    pub generic_cache: GenericCache,
    pub origin_mod: *const Mod,
    pub linked_tp_tbl: FxHashMap<*mut PLType, Vec<Arc<RefCell<PLType>>>>,
    is_active_file: bool,
    as_root: bool,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum MacroReplaceNode {
    NodeEnum(NodeEnum),
    LoopNodeEnum(Vec<NodeEnum>),
}

mod generic;

impl<'a, 'ctx> Ctx<'a> {
    pub fn run_as_root_ctx<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let old_as_root = self.as_root;
        self.as_root = true;
        let result = f(self);
        self.as_root = old_as_root;
        result
    }
    /// lsp fn
    pub fn is_active_file(&self) -> bool {
        self.is_active_file
    }
    pub fn add_term_to_previous_yield(
        &self,
        builder: &'a BuilderEnum<'a, 'ctx>,
        curbb: usize,
    ) -> Arc<RefCell<crate::ast::ctx::GeneratorCtxData>> {
        let ctx = self;
        let data = ctx.generator_data.as_ref().unwrap();
        if let Some(prev_bb) = data.borrow().prev_yield_bb {
            builder.position_at_end_block(prev_bb);
            let ctx_handle = builder.get_nth_param(ctx.function.unwrap(), 0);
            let ptr = builder
                .build_struct_gep(ctx_handle, 1, "block_ptr")
                .unwrap();

            let addr = builder.get_block_address(curbb);
            builder.build_store(ptr, addr);

            builder.build_unconditional_branch(ctx.return_block.unwrap().0);
        }
        data.clone()
    }

    pub fn check_self_ref(&self, name: &str, range: Range) -> Result<(), PLDiag> {
        if let Some(root) = self.root {
            root.check_self_ref_inner(name, name, range)
                .map_err(|e| e.add_to_ctx(self))
        } else {
            self.check_self_ref_inner(name, name, range)
                .map_err(|e| e.add_to_ctx(self))
        }
    }

    fn check_self_ref_inner(
        &self,
        name: &str,
        root_name: &str,
        range: Range,
    ) -> Result<(), PLDiag> {
        // self ref map's key is field type name, value is a set of (host type name, host type range)
        if let Some(host_tp) = self.self_ref_map.get(name) {
            if let Some((host_name, r)) = host_tp.iter().next() {
                if host_name == root_name {
                    return Err(range
                        .new_err(ErrorCode::ILLEGAL_SELF_RECURSION)
                        .add_label(*r, self.get_file(), format_label!("in type {}", name))
                        .clone());
                } else {
                    return self
                        .check_self_ref_inner(host_name, root_name, range)
                        .map_err(|mut e| {
                            e.add_label(*r, self.get_file(), format_label!("in type {}", name))
                                .clone()
                        });
                }
            }
        }
        Ok(())
    }
    pub fn find_macro_symbol(&self, name: &str) -> Option<&MacroReplaceNode> {
        let mut ctx = Some(self);
        let mut i = 0;
        while let Some(p) = ctx {
            i += 1;
            if i <= self.macro_skip_level {
                ctx = p.father;
                continue;
            }
            if let Some(v) = p.macro_vars.get(name) {
                return Some(v);
            }
            ctx = p.father;
        }
        None
    }
    pub fn new(
        src_file_path: &'a str,
        errs: &'a RefCell<FxHashSet<PLDiag>>,
        edit_pos: Option<Pos>,
        config: Config,
        db: &'a dyn Db,
        is_active_file: bool,
    ) -> Ctx<'a> {
        let generic_infer: GenericCache = Default::default();
        Ctx {
            need_highlight: Default::default(),
            generic_types: FxHashMap::default(),
            plmod: Mod::new(src_file_path.to_string(), generic_infer.clone()),
            father: None,
            init_func: None,
            function: None,
            errs,
            edit_pos,
            table: FxHashMap::default(),
            config,
            db,
            block: None,
            continue_block: None,
            break_block: None,
            return_block: None,
            rettp: None,
            macro_vars: FxHashMap::default(),
            macro_loop: false,
            macro_loop_idx: 0,
            macro_loop_len: 0,
            temp_source: None,
            in_macro: false,
            closure_data: None,
            expect_ty: None,
            root: None,
            macro_skip_level: 0,
            self_ref_map: FxHashMap::default(),
            ctx_flag: CtxFlag::Normal,
            generator_data: None,
            generic_cache: generic_infer,
            origin_mod: std::ptr::null(),
            linked_tp_tbl: FxHashMap::default(),
            is_active_file,
            as_root: false,
        }
    }
    pub fn new_child(&'a self, start: Pos, builder: &'a BuilderEnum<'a, 'ctx>) -> Ctx<'a> {
        let mut root = self.root;
        if self.father.is_none() {
            root = Some(self);
        }
        let mut ctx = Ctx {
            need_highlight: self.need_highlight.clone(),
            generic_types: FxHashMap::default(),
            plmod: self.plmod.new_child(),
            father: Some(self),
            errs: self.errs,
            edit_pos: self.edit_pos,
            table: FxHashMap::default(),
            config: self.config.clone(),
            db: self.db.clone(),
            block: self.block,
            continue_block: self.continue_block,
            break_block: self.break_block,
            return_block: self.return_block,
            rettp: self.rettp.clone(),
            init_func: self.init_func,
            function: self.function,
            macro_vars: FxHashMap::default(),
            macro_loop: self.macro_loop,
            macro_loop_idx: self.macro_loop_idx,
            macro_loop_len: self.macro_loop_len,
            temp_source: self.temp_source.clone(),
            in_macro: self.in_macro,
            closure_data: None,
            expect_ty: None,
            root,
            macro_skip_level: self.macro_skip_level,
            self_ref_map: FxHashMap::default(),
            ctx_flag: self.ctx_flag,
            generator_data: self.generator_data.clone(),
            generic_cache: self.generic_cache.clone(),
            origin_mod: self.origin_mod,
            linked_tp_tbl: FxHashMap::default(),
            is_active_file: self.is_active_file,
            as_root: false,
        };
        add_primitive_types(&mut ctx);
        if start != Default::default() {
            builder.new_subscope(start);
        }
        ctx
    }

    pub fn with_macro_loop(
        &mut self,
        mut f: impl FnMut(&mut Self) -> NodeResult,
        r: Range,
    ) -> NodeResult {
        let old_macro_loop = self.macro_loop;
        let old_macro_loop_idx = self.macro_loop_idx;
        self.macro_loop_idx = 0;
        self.macro_loop = true;
        let mut result;
        loop {
            result = f(self);
            self.macro_loop_idx += 1;
            if self.macro_loop_len == 0 {
                // no loop variable used, return error
                return Err(r.new_err(ErrorCode::NO_MACRO_LOOP_VAR));
            }
            if self.macro_loop_idx == self.macro_loop_len {
                // loop end
                break;
            }
        }
        self.macro_loop = old_macro_loop;
        self.macro_loop_idx = old_macro_loop_idx;
        result
    }
    pub fn with_macro_emit(&mut self, f: impl FnOnce(&mut Self) -> NodeResult) -> NodeResult {
        let old_in_macro = self.in_macro;
        self.in_macro = true;
        let result = f(self);
        self.in_macro = old_in_macro;
        result
    }
    pub fn with_macro_loop_parse<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let old_macro_loop = self.macro_loop;
        let old_macro_loop_idx = self.macro_loop_idx;
        self.macro_loop_idx = 0;
        self.macro_loop = true;
        let result = f(self);
        self.macro_loop = old_macro_loop;
        self.macro_loop_idx = old_macro_loop_idx;
        result
    }
    pub fn set_init_fn<'b>(&'b mut self, builder: &'b BuilderEnum<'a, 'ctx>) {
        self.function = Some(builder.add_function(
            &self.plmod.get_full_name("__init_global"),
            &[],
            PLType::Void,
            self,
        ));
        self.init_func = self.function;
        builder.append_basic_block(self.init_func.unwrap(), "alloc");
        let entry = builder.append_basic_block(self.init_func.unwrap(), "entry");
        self.position_at_end(entry, builder);
    }
    pub fn init_fn_ret<'b>(&'b mut self, builder: &'b BuilderEnum<'a, 'ctx>) {
        let alloc = builder.get_first_basic_block(self.init_func.unwrap());
        let entry = builder.get_last_basic_block(self.init_func.unwrap());
        self.position_at_end(alloc, builder);
        builder.rm_curr_debug_location();
        builder.build_unconditional_branch(entry);
        self.position_at_end(entry, builder);
        builder.build_return(None);
    }
    fn add_to_global_mthd_table(
        &self,
        st_name: &str,
        mthd_name: &str,
        fntp: Arc<RefCell<FNValue>>,
    ) {
        self.plmod
            .add_to_global_mthd_table(st_name, mthd_name, fntp);
    }
    pub fn get_root_ctx(&self) -> &Ctx {
        let mut ctx = self;
        while let Some(p) = ctx.root {
            ctx = p;
        }
        ctx
    }

    pub fn find_global_method(&self, name: &str, mthd: &str) -> Option<Arc<RefCell<FNValue>>> {
        self.plmod.find_global_method(name, mthd)
    }

    pub fn get_global_mthd_completions(
        &self,
        name: &str,
        set: &mut FxHashMap<String, CompletionItem>,
    ) {
        self.plmod.get_global_mthd_completions(name, set);
    }

    fn add_trait_impl_method<T: TraitImplAble>(
        &mut self,
        t: &T,
        mthd: &str,
        fntp: Arc<RefCell<FNValue>>,
        impl_trait: Option<(Arc<RefCell<PLType>>, Range)>,
        generic: bool,
        target: Range,
    ) -> Result<(), PLDiag> {
        if let Some((tt, trait_range)) = impl_trait {
            if let PLType::Trait(st) = &*tt.borrow() {
                if st.get_path() != self.get_file() && target != Default::default() {
                    return Err(trait_range
                        .new_err(ErrorCode::CANNOT_IMPL_TYPE_OUT_OF_DEFINE_MOD)
                        .add_to_ctx(self));
                }
                let mut m = st.trait_methods_impl.borrow_mut();
                let st_name = if generic {
                    t.get_full_name_except_generic()
                } else {
                    t.get_full_name()
                };
                let table = m.get_mut(&st_name);
                if let Some(table) = table {
                    if table.insert(mthd.to_string(), fntp.clone()).is_some()
                        && target != Default::default()
                    {
                        return Err(fntp
                            .borrow()
                            .range
                            .new_err(ErrorCode::DUPLICATE_METHOD)
                            .add_to_ctx(self));
                    }
                    self.add_to_global_mthd_table(&st_name, mthd, fntp);
                    Ok(())
                } else {
                    #[allow(clippy::drop_non_drop)]
                    drop(table);
                    m.insert(st_name.clone(), FxHashMap::default());
                    let table = m.get_mut(&st_name).unwrap();
                    table.insert(mthd.to_string(), fntp.clone());
                    self.add_to_global_mthd_table(&st_name, mthd, fntp);
                    Ok(())
                }
            } else {
                Err(trait_range
                    .new_err(ErrorCode::ONLY_TRAIT_CAN_BE_IMPL)
                    .add_to_ctx(self))
            }
        } else {
            Err(target
                .new_err(ErrorCode::EXPECT_TO_BE_A_TRAIT_IMPL)
                .add_to_ctx(self))
        }
    }

    fn add_method_to_tp<T: ImplAble>(
        &mut self,
        t: &T,
        mthd: &str,
        fntp: Arc<RefCell<FNValue>>,
        impl_trait: Option<(Arc<RefCell<PLType>>, Range)>,
        generic: bool,
        target: Range,
    ) -> Result<(), PLDiag> {
        if t.get_path() != self.get_file() {
            self.add_trait_impl_method(t, mthd, fntp, impl_trait, generic, target)
        } else {
            t.add_method(mthd, fntp).map_err(|e| e.add_to_ctx(self))
        }
    }

    pub fn add_method(
        &mut self,
        tp: &PLType,
        mthd: &str,
        fntp: FNValue,
        impl_trait: Option<(Arc<RefCell<PLType>>, Range)>,
        generic: bool,
        target: Range,
    ) -> Result<(), PLDiag> {
        let fntp = Arc::new(RefCell::new(fntp));
        match tp {
            PLType::Struct(s) | PLType::Trait(s) => {
                self.add_method_to_tp(s, mthd, fntp, impl_trait, generic, target)
            }
            PLType::PlaceHolder(s) => {
                self.add_method_to_tp(s, mthd, fntp, impl_trait, generic, target)
            }
            PLType::Union(u) => self.add_method_to_tp(u, mthd, fntp, impl_trait, generic, target),
            PLType::Closure(p) => {
                self.add_trait_impl_method(p, mthd, fntp, impl_trait, generic, target)
            }
            PLType::Primitive(p) => {
                self.add_trait_impl_method(p, mthd, fntp, impl_trait, generic, target)
            }
            PLType::Arr(p) => {
                self.add_trait_impl_method(p, mthd, fntp, impl_trait, generic, target)
            }
            _ => Err(target
                .new_err(ErrorCode::TARGET_TYPE_NOT_IMPL_ABLE)
                .add_to_ctx(self)),
        }
    }
    /// # get_symbol
    /// search in current and all father symbol tables
    pub fn get_symbol<'b>(
        &'b self,
        name: &str,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Option<PLSymbol> {
        let v = self.table.get(name);
        if let Some(symbol) = v {
            return Some(PLSymbol::Local(symbol.clone()));
        }
        if let Some(data) = &self.closure_data {
            let mut data = data.borrow_mut();
            if let Some((symbol, _)) = data.table.get(name) {
                return Some(PLSymbol::Captured(symbol.clone()));
            } else if let Some(father) = self.father {
                let re = father.get_symbol(name, builder);
                if let Some(s) = &re {
                    let symbol = s.get_data_ref();
                    let is_glob = s.is_global();
                    if !is_glob {
                        let cur = builder.get_cur_basic_block();
                        // just make sure we are in the alloca bb
                        // so that the captured value is not used before it is initialized
                        if let Some(bb) = data.alloca_bb {
                            builder.position_at_end_block(bb);
                        } else {
                            builder.position_at_end_block(
                                builder.get_first_basic_block(self.function.unwrap()),
                            );
                        }
                        builder.rm_curr_debug_location();
                        // captured by closure
                        let new_symbol = symbol.clone();
                        let len = data.table.len();
                        builder.add_closure_st_field(data.data_handle, new_symbol.value);
                        let new_symbol = PLSymbolData {
                            value: builder.build_load(
                                builder
                                    .build_struct_gep(
                                        data.data_handle,
                                        len as u32 + 1,
                                        "closure_tmp",
                                    )
                                    .unwrap(),
                                "closure_loaded",
                            ),
                            ..new_symbol
                        };
                        data.table
                            .insert(name.to_string(), (new_symbol.clone(), symbol.value));
                        builder.position_at_end_block(cur);
                        return Some(PLSymbol::Captured(new_symbol));
                    }
                }
                return re;
            }
        }
        if !self.as_root {
            if let Some(father) = self.father {
                let re = father.get_symbol(name, builder);
                return re;
            }
        }
        if let Some(GlobalVar {
            tp: pltype, range, ..
        }) = self.plmod.get_global_symbol(name)
        {
            return Some(PLSymbol::Global(PLSymbolData {
                value: builder
                    .get_global_var_handle(&self.plmod.get_full_name(name))
                    .unwrap_or(builder.get_global_var_handle(name).unwrap()),
                pltype: pltype.clone(),
                range: *range,
                refs: None,
            }));
        }
        None
    }

    pub fn add_symbol(
        &mut self,
        name: String,
        pv: ValueHandle,
        pltype: Arc<RefCell<PLType>>,
        range: Range,
        is_const: bool,
        is_extern: bool,
    ) -> Result<(), PLDiag> {
        if self.table.contains_key(&name) {
            return Err(self.add_diag(range.new_err(ErrorCode::REDECLARATION)));
        }
        self.add_symbol_without_check(name, pv, pltype, range, is_const, is_extern)
    }
    pub fn add_symbol_without_check(
        &mut self,
        name: String,
        pv: ValueHandle,
        pltype: Arc<RefCell<PLType>>,
        range: Range,
        is_const: bool,
        is_extern: bool,
    ) -> Result<(), PLDiag> {
        if is_const {
            self.set_glob_refs(&self.plmod.get_full_name(&name), range);
            self.plmod
                .add_global_symbol(name, pltype, range, is_extern, is_const)?;
        } else {
            let refs = Arc::new(RefCell::new(vec![]));
            self.table.insert(
                name,
                PLSymbolData {
                    value: pv,
                    pltype,
                    range,
                    refs: Some(refs.clone()),
                },
            );
            self.set_local_refs(refs, range);
        }
        self.send_if_go_to_def(range, range, self.plmod.path.clone());
        Ok(())
    }

    /// # add_symbol_raw
    ///
    /// add symbol without checking if it is already declared
    ///
    /// doesn't handle any lsp stuffs like go to def
    ///
    /// mostly used in builtin functions
    pub fn add_symbol_raw(
        &mut self,
        name: String,
        pv: ValueHandle,
        pltype: Arc<RefCell<PLType>>,
        range: Range,
    ) {
        let refs = Arc::new(RefCell::new(vec![]));
        self.table.insert(
            name,
            PLSymbolData {
                value: pv,
                pltype,
                range,
                refs: Some(refs),
            },
        );
    }
    pub fn get_type(&self, name: &str, range: Range) -> Result<GlobType, PLDiag> {
        if let Some(pv) = self.generic_types.get(name) {
            self.set_if_refs_tp(pv.clone(), range);
            if let Ok(pv) = pv.try_borrow() {
                self.send_if_go_to_def(
                    range,
                    pv.get_range().unwrap_or(range),
                    self.plmod.path.clone(),
                );
            }
            return Ok(pv.clone().into());
        }
        if let Ok(pv) = self.plmod.get_type(name, range, self) {
            return Ok(pv);
        }
        if let Some(father) = self.father {
            let re = father.get_type(name, range);
            return re;
        }
        Err(range.new_err(ErrorCode::UNDEFINED_TYPE))
    }

    pub fn get_type_in_mod(&self, m: &Mod, name: &str, range: Range) -> Result<GlobType, PLDiag> {
        if let Some(pv) = self.generic_types.get(name) {
            self.set_if_refs_tp(pv.clone(), range);
            self.send_if_go_to_def(
                range,
                pv.borrow().get_range().unwrap_or(range),
                self.plmod.path.clone(),
            );
            return Ok(pv.clone().into());
        }
        if m.path == self.plmod.path {
            if let Ok(pv) = self.plmod.get_type(name, range, self) {
                return Ok(pv);
            }
        } else if let Ok(pv) = m.get_type(name, range, self) {
            return Ok(pv);
        }
        if let Some(father) = self.father {
            let re = father.get_type_in_mod(m, name, range);
            return re;
        }
        Err(range.new_err(ErrorCode::UNDEFINED_TYPE))
    }
    /// 用来获取外部模块的全局变量
    /// 如果没在当前module的全局变量表中找到，将会生成一个
    /// 该全局变量的声明
    pub fn get_or_add_global<'b>(
        &'b mut self,
        name: &str,
        pltype: Arc<RefCell<PLType>>,
        builder: &'b BuilderEnum<'a, 'ctx>,
        constant: bool,
    ) -> ValueHandle {
        builder.get_or_add_global(name, pltype, self, constant)
    }
    pub fn init_global<'b>(&'b mut self, builder: &'b BuilderEnum<'a, 'ctx>) {
        let mut set: FxHashSet<String> = FxHashSet::default();
        for sub in self.plmod.clone().submods.values() {
            self.init_global_walk(sub, &mut set, builder);
        }

        builder.rm_curr_debug_location();
        builder.build_call(
            builder
                .get_function(&self.plmod.get_full_name("__init_global"))
                .unwrap(),
            &[],
            &PLType::Void,
            self,
        );
    }
    fn init_global_walk<'b>(
        &'b mut self,
        m: &Mod,
        set: &mut FxHashSet<String>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) {
        let name = m.get_full_name("__init_global");
        if set.contains(&name) {
            return;
        }
        for sub in m.submods.values() {
            self.init_global_walk(sub, set, builder);
        }
        let f = builder.add_function(&name, &[], PLType::Void, self);
        builder.rm_curr_debug_location();
        builder.build_call(f, &[], &PLType::Void, self);
        set.insert(name);
    }

    pub fn add_type(
        &mut self,
        name: String,
        pltype: Arc<RefCell<PLType>>,
        range: Range,
    ) -> Result<(), PLDiag> {
        if self.plmod.types.contains_key(&name) {
            return Err(self.add_diag(range.new_err(ErrorCode::REDEFINE_TYPE)));
        }
        self.set_if_refs_tp(pltype.clone(), range);
        self.send_if_go_to_def(range, range, self.plmod.path.clone());
        self.db
            .add_tp_to_mod(&self.plmod.path, &name, pltype.clone());
        self.plmod.types.insert(name, pltype.into());
        Ok(())
    }
    pub fn add_type_without_check(&mut self, pltype: Arc<RefCell<PLType>>) {
        if let PLType::Generic(_) = &*pltype.borrow() {
            unreachable!()
        }
        let name = pltype.borrow().get_name();
        self.db
            .add_tp_to_mod(&self.plmod.path, &name, pltype.clone());
        self.plmod.types.insert(name, pltype.into());
    }
    #[inline]
    fn add_generic_type(&mut self, name: String, pltype: Arc<RefCell<PLType>>) {
        self.generic_types.insert(name, pltype);
    }
    pub fn add_doc_symbols(&mut self, pltype: Arc<RefCell<PLType>>) {
        match &*RefCell::borrow(&pltype) {
            PLType::Fn(fnvalue) => {
                if self.get_file() != fnvalue.get_path() {
                    return;
                }
                if !fnvalue.fntype.method && !fnvalue.in_trait {
                    self.plmod
                        .doc_symbols
                        .borrow_mut()
                        .push(fnvalue.get_doc_symbol())
                }
            }
            PLType::Struct(st) => {
                if self.get_file() != st.get_path() {
                    return;
                }
                self.plmod
                    .doc_symbols
                    .borrow_mut()
                    .push(st.get_doc_symbol())
            }
            _ => {}
        }
    }

    pub fn with_diag_src<T>(&mut self, src: &str, f: impl FnOnce(&mut Self) -> T) -> T {
        let old = self.temp_source.clone();
        self.temp_source = Some(src.to_string());
        let re = f(self);
        self.temp_source = old;
        re
    }

    pub fn add_diag(&self, mut dia: PLDiag) -> PLDiag {
        if let Some(src) = &self.temp_source {
            dia.set_source(src);
        }
        let dia2 = dia.clone();
        self.errs.borrow_mut().insert(dia);
        dia2
    }
    // load type* to type
    pub fn try_load2var<'b>(
        &'b mut self,
        range: Range,
        v: ValueHandle,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<ValueHandle, PLDiag> {
        builder.try_load2var(range, v, self)
    }
    fn set_mod(&mut self, plmod: Mod) -> Mod {
        let m = self.plmod.clone();
        self.plmod = plmod;
        m
    }
    pub fn protect_generic_context<'b, T, F: FnMut(&mut Ctx<'a>) -> Result<T, PLDiag>>(
        &mut self,
        generic_map: &IndexMap<String, Arc<RefCell<PLType>>>,
        mut f: F,
    ) -> Result<T, PLDiag> {
        let mp = self.generic_types.clone();
        for (name, pltype) in generic_map.iter() {
            self.add_generic_type(name.clone(), pltype.clone());
        }
        let res = f(self);
        self.generic_types = mp;
        res
    }

    pub fn run_in_type_mod<'b, TP: CustomType, R, F: FnMut(&mut Ctx<'a>, &TP) -> R>(
        &'b mut self,
        u: &TP,
        mut f: F,
    ) -> R {
        if u.get_path() != self.plmod.path {
            let ori_mod = unsafe { &*self.origin_mod as &Mod };
            let m = if u.get_path() == ori_mod.path {
                ori_mod.clone()
            } else {
                self.db.get_module(&u.get_path()).unwrap()
            };
            let oldm = self.set_mod(m);
            let origin = self.origin_mod as isize == &self.plmod as *const Mod as isize;
            if origin {
                self.origin_mod = &oldm as *const Mod;
            }
            let res = f(self, u);
            self.set_mod(oldm);
            if origin {
                self.origin_mod = &self.plmod as *const Mod;
            }
            res
        } else {
            f(self, u)
        }
    }

    pub fn run_in_origin_mod<'b, R, F: FnMut(&mut Ctx<'a>) -> R>(&'b mut self, mut f: F) -> R {
        let oldm = self.plmod.clone();
        let ori_mod = unsafe { &*self.origin_mod as &Mod };
        self.set_mod(ori_mod.clone());
        let res = f(self);
        self.set_mod(oldm);
        res
    }
    pub fn run_in_type_mod_mut<'b, TP: CustomType, R, F: FnMut(&mut Ctx<'a>, &mut TP) -> R>(
        &'b mut self,
        u: &mut TP,
        mut f: F,
    ) -> R {
        if u.get_path() != self.plmod.path {
            let ori_mod = unsafe { &*self.origin_mod as &Mod };
            let m = if u.get_path() == ori_mod.path {
                ori_mod.clone()
            } else {
                self.db.get_module(&u.get_path()).unwrap()
            };
            let oldm = self.set_mod(m);
            let origin = self.origin_mod as isize == &self.plmod as *const Mod as isize;
            if origin {
                self.origin_mod = &oldm as *const Mod;
            }
            let res = f(self, u);
            self.set_mod(oldm);
            if origin {
                self.origin_mod = &self.plmod as *const Mod;
            }
            res
        } else {
            f(self, u)
        }
    }

    pub fn get_mod(&self, path: &str) -> Mod {
        let ori_mod = unsafe { &*self.origin_mod as &Mod };

        if path == self.plmod.path {
            self.plmod.clone()
        } else if path == ori_mod.path {
            ori_mod.clone()
        } else {
            self.db.get_module(path).unwrap()
        }
    }

    pub fn get_file_url(&self) -> Url {
        #[cfg(any(unix, windows, target_os = "redox", target_os = "wasi"))]
        return Url::from_file_path(self.plmod.path.clone()).unwrap();
        #[cfg(not(any(unix, windows, target_os = "redox", target_os = "wasi")))]
        {
            if self.plmod.path.starts_with("http") {
                return Url::parse(&self.plmod.path).unwrap();
            }
            return Url::parse("httss://example.com").unwrap();
        }
    }

    pub fn get_file(&self) -> String {
        self.plmod.path.clone()
    }

    pub fn get_location(&self, range: Range) -> Location {
        Location::new(self.get_file_url(), range.to_diag_range())
    }
    pub fn position_at_end<'b>(
        &'b mut self,
        block: BlockHandle,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) {
        self.block = Some(block);
        builder.position_at_end_block(block);
    }

    /// # auto_deref
    /// 自动解引用，有几层解几层
    pub fn auto_deref<'b>(
        &'b self,
        tp: Arc<RefCell<PLType>>,
        value: ValueHandle,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> (Arc<RefCell<PLType>>, ValueHandle) {
        let mut tp = tp;
        let mut value = value;
        while let PLType::Pointer(p) = &*get_type_deep(tp.clone()).borrow() {
            tp = p.clone();
            value = builder.build_load(value, "load");
        }
        (tp, value)
    }

    /// # auto_deref_tp
    /// 自动解pltype引用，有几层解几层
    pub fn auto_deref_tp(&self, tp: Arc<RefCell<PLType>>) -> Arc<RefCell<PLType>> {
        let mut tp = tp;
        while let PLType::Pointer(p) = &*tp.clone().borrow() {
            tp = p.clone()
        }
        tp
    }

    pub fn get_macro(&self, name: &str) -> Option<Arc<MacroNode>> {
        if let Some(m) = self.plmod.macros.get(name) {
            return Some(m.clone());
        }
        if let Some(father) = &self.father {
            return father.get_macro(name);
        }
        None
    }

    fn diff_trait_impl(&mut self, l: &GenericType, r: &GenericType) -> Option<String> {
        let noop = BuilderEnum::NoOp(NoOpBuilder::default());
        // get it's pointer
        let noop_ptr = &noop as *const BuilderEnum<'a, '_>;
        let builder = unsafe { &*(noop_ptr as *const BuilderEnum<'a, '_>) };
        let binding = l
            .trait_impl
            .clone()
            .map(|e| e.get_types(self, builder).unwrap())
            .unwrap_or(vec![]);
        let miss = binding
            .iter()
            .filter(|lf| {
                !r.trait_impl
                    .clone()
                    .map(|e| e.get_types(self, builder).unwrap())
                    .unwrap_or(vec![])
                    .iter()
                    .any(|rf| self.eq(lf.clone().to_owned(), rf.clone()).eq)
            })
            .collect::<Vec<_>>();
        if miss.is_empty() {
            return None;
        }
        let mut s = String::new();
        s.push_str("missing impl for trait(s):");
        for m in miss {
            s.push_str(&format!(" `{}`", m.borrow().get_name()));
        }
        Some(s)
    }

    pub fn try_set_closure_alloca_bb(&self, bb: BlockHandle) {
        if let Some(c) = &self.closure_data {
            c.borrow_mut().alloca_bb = Some(bb);
        } else if let Some(father) = &self.father {
            father.try_set_closure_alloca_bb(bb);
        }
    }
}

mod lsp;

mod completion;

mod cast;

pub use generic::EqRes;
