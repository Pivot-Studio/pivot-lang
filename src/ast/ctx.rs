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
use super::plmod::GlobalType;
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

use crate::inference::TyVariable;
use crate::utils::read_config::Config;

use crate::Db;

use crate::ast::node::function::generator::GeneratorCtxData;
use ena::unify::UnificationTable;
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
    /// Local refers the symbol exists in a local scope
    Local(PLSymbolData),
    /// Global refers the symbol exists in the global scope of current module
    Global(PLSymbolData),
    /// Captured refers a symbol exists in another module
    Captured(PLSymbolData),
}

impl PLSymbol {
    pub fn is_global(&self) -> bool {
        matches!(self, PLSymbol::Global(_))
    }
    pub fn is_captured(&self) -> bool {
        matches!(self, PLSymbol::Captured(_))
    }
    /// # get_data_ref
    ///
    /// returns the reference of the symbol data regardless the scope of data
    pub fn get_data_ref(&self) -> &PLSymbolData {
        match self {
            PLSymbol::Local(d) => d,
            PLSymbol::Global(d) => d,
            PLSymbol::Captured(d) => d,
        }
    }

    /// # get_data
    ///
    /// returns the the symbol data regardless the scope of data
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
/// Context for code generation and LSP features
pub struct Ctx<'a> {
    pub generic_types: FxHashMap<String, Arc<RefCell<PLType>>>,
    pub plmod: Mod,

    pub parent: Option<&'a Ctx<'a>>, // parent context, for symbol lookup
    pub root: Option<&'a Ctx<'a>>,   // root context, for symbol lookup

    /// LSP argument, because the high light among multiple tokens require an order,
    /// hence, we stores a value inside it to refer whether we need to generate the code or not.
    /// highlight is generated only if the number is 0
    pub need_highlight: Arc<RefCell<usize>>,

    /// the index to mark a lower level code generation of function element from the builder
    pub function: Option<ValueHandle>,
    /// the init function called first in main
    pub init_func: Option<ValueHandle>,
    /// current block
    pub block: Option<BlockHandle>,
    /// the block to jump when continue if it's a loop statement
    pub continue_block: Option<BlockHandle>,
    /// the block to jump to when break if it's a loop statement
    pub break_block: Option<BlockHandle>,
    /// the block to jump to when return and value
    pub return_block: Option<(BlockHandle, Option<ValueHandle>)>,

    /// diagnose tries to hold all warning and as many as possible errors
    pub diagnose: &'a RefCell<FxHashSet<PLDiag>>,

    /// LSP argument: the editing position of this context
    pub edit_pos: Option<Pos>,

    /// available varaibles in the current block,
    /// the outside variables could be accessed through the parent ctx.                             
    pub table: FxHashMap<String, PLSymbolData>,
    pub config: Config, // config
    pub db: &'a dyn Db,

    /// the return type of a function, all contexts for functions or the child contexts of functions
    /// hold the return type.
    pub rettp: Option<Arc<RefCell<PLType>>>,

    /// the expect_ty is used when implicitly cast type during assignment
    /// for example, the `let i:i8 = 1.2`, the expect_ty will be i8
    pub expect_ty: Option<Arc<RefCell<PLType>>>,

    /// generics requires a code generation at the place where the identifiers are defined
    /// hence, we need to perserve the definition position as well for a better error report.
    /// the temp_source stores the position where the code is generated.
    pub temp_source: Option<String>,

    /// stores the the additional date of a closure
    pub closure_data: Option<Arc<RefCell<ClosureCtxData>>>,

    /// used to recognize self reference to avoid endless loop
    pub self_ref_map: FxHashMap<String, FxHashSet<(String, Range)>>,

    pub ctx_flag: CtxFlag, // can ignore first
    pub generator_data: Option<Arc<RefCell<GeneratorCtxData>>>,

    pub generic_cache: GenericCache, // too old to remember

    /// back up plmod because sometimes we need to find the parent mod after switching
    pub origin_mod: *const Mod,

    pub linked_tp_tbl: FxHashMap<*mut PLType, Vec<Arc<RefCell<PLType>>>>,

    /// not all lsp request has an edition position
    is_active_file: bool,

    // for lsp, don't give hints for variables, etc...
    as_root: bool,

    macro_expand_depth: Arc<RefCell<u64>>,
    pub unify_table: Arc<RefCell<UnificationTable<TyVariable>>>,
    pub disable_diag: bool,

    pub macro_vars: FxHashMap<String, MacroReplaceNode>,
    pub macro_skip_level: usize,
    pub macro_loop: bool,
    pub macro_loop_idx: usize,
    pub macro_loop_len: usize,
    pub in_macro: bool,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum MacroReplaceNode {
    NodeEnum(NodeEnum),
    LoopNodeEnum(Vec<NodeEnum>),
}

mod generic;

impl<'a, 'ctx> Ctx<'a> {
    pub fn add_macro_depth(&self) {
        *self.macro_expand_depth.borrow_mut() += 1;
    }
    pub fn sub_macro_depth(&self) {
        *self.macro_expand_depth.borrow_mut() -= 1;
    }
    pub fn get_macro_depth(&self) -> u64 {
        *self.macro_expand_depth.borrow()
    }
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
    pub fn add_term_to_previous_yield<'b>(
        &'b mut self,
        builder: &'b BuilderEnum<'a, 'ctx>,
        curbb: usize,
    ) -> Arc<RefCell<crate::ast::ctx::GeneratorCtxData>> {
        let ctx = self;
        let data = ctx.generator_data.as_ref().unwrap().clone();
        if let Some(prev_bb) = data.borrow().prev_yield_bb {
            builder.position_at_end_block(prev_bb);
            let ctx_handle = builder.get_nth_param(ctx.function.unwrap(), 0);
            let ptr = builder
                .build_struct_gep(
                    ctx_handle,
                    1,
                    "block_ptr",
                    &data.borrow().ctx_tp.as_ref().unwrap().borrow(),
                    ctx,
                )
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
                ctx = p.parent;
                continue;
            }
            if let Some(v) = p.macro_vars.get(name) {
                return Some(v);
            }
            ctx = p.parent;
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
            parent: None,
            init_func: None,
            function: None,
            diagnose: errs,
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
            macro_expand_depth: Default::default(),
            unify_table: Arc::new(RefCell::new(UnificationTable::new())),
            disable_diag: false,
        }
    }
    pub fn new_child(&'a self, start: Pos, builder: &'a BuilderEnum<'a, 'ctx>) -> Ctx<'a> {
        let mut root = self.root;
        if self.parent.is_none() {
            root = Some(self);
        }
        let mut ctx = Ctx {
            need_highlight: self.need_highlight.clone(),
            generic_types: FxHashMap::default(),
            plmod: self.plmod.new_child(),
            parent: Some(self),
            diagnose: self.diagnose,
            edit_pos: self.edit_pos,
            table: FxHashMap::default(),
            config: self.config.clone(),
            db: self.db,
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
            macro_expand_depth: self.macro_expand_depth.clone(),
            unify_table: self.unify_table.clone(),
            disable_diag: self.disable_diag,
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
        self.add_macro_depth();
        if self.get_macro_depth() > 30 {
            self.sub_macro_depth();
            return Err(Range::default().new_err(ErrorCode::MACRO_EXPAND_DEPTH_TOO_DEEP));
        }
        let old_in_macro = self.in_macro;
        self.in_macro = true;
        let result = f(self);
        self.in_macro = old_in_macro;
        self.sub_macro_depth();
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
    ///
    /// search in current and all parent symbol tables
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
            }
            if let Some(parent) = self.parent {
                let re = parent.get_symbol(name, builder);
                if let Some(s) = &re {
                    let symbol = s.get_data_ref();
                    if !s.is_global() {
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
                        let st_r = data.data_tp.as_ref().unwrap().borrow();
                        let st: &super::pltype::STType = match &*st_r {
                            PLType::Struct(s) => s,
                            _ => unreachable!(),
                        };
                        let ptr = parent as *const _;
                        let ptr = ptr as usize;
                        let ptr = ptr as *mut Ctx<'_>;
                        builder.add_closure_st_field(st, new_symbol.value, unsafe { &mut *ptr });
                        drop(st_r);
                        let new_symbol = PLSymbolData {
                            value: builder.build_load(
                                builder
                                    .build_struct_gep(
                                        data.data_handle,
                                        len as u32 + 1,
                                        "closure_tmp",
                                        &data.data_tp.as_ref().unwrap().borrow(),
                                        unsafe { &mut *ptr },
                                    )
                                    .unwrap(),
                                "closure_loaded",
                                &PLType::new_i8_ptr(),
                                unsafe { &mut *ptr },
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
            if let Some(father) = self.parent {
                let re = father.get_symbol(name, builder);
                return re;
            }
        }
        if let Some(GlobalVar {
            tp: pltype, range, ..
        }) = self.plmod.get_global_symbol(name)
        {
            return builder
                .get_global_var_handle(&self.plmod.get_full_name(name))
                .or(builder.get_global_var_handle(name))
                .map(|value| {
                    PLSymbol::Global(PLSymbolData {
                        value,
                        pltype: pltype.clone(),
                        range: *range,
                        refs: None,
                    })
                });
        }
        None
    }

    pub fn add_symbol(
        &mut self,
        name: String,
        pv: ValueHandle,
        pltype: Arc<RefCell<PLType>>,
        range: Range,
        is_glob: bool,
        is_extern: bool,
    ) -> Result<(), PLDiag> {
        if self.table.contains_key(&name) {
            return Err(self.add_diag(range.new_err(ErrorCode::REDECLARATION)));
        }

        self.add_symbol_without_check(name, pv, real_tp(pltype), range, is_glob, is_extern)
    }
    pub fn add_symbol_without_check(
        &mut self,
        name: String,
        pv: ValueHandle,
        pltype: Arc<RefCell<PLType>>,
        range: Range,
        is_glob: bool,
        is_extern: bool,
    ) -> Result<(), PLDiag> {
        if is_glob {
            self.set_glob_refs(&self.plmod.get_full_name(&name), range);
            self.plmod
                .add_global_symbol(name, pltype, range, is_extern)?;
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

    /// # get_type
    ///
    /// Get type based on name from the generic types,
    /// from the current context to its ancestor context until one element is found, otherwise it return an error.
    pub fn get_type(&self, name: &str, range: Range) -> Result<GlobalType, PLDiag> {
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
        if let Some(father) = self.parent {
            let re = father.get_type(name, range);
            return re;
        }
        Err(range.new_err(ErrorCode::UNDEFINED_TYPE))
    }

    pub fn get_type_in_mod(&self, m: &Mod, name: &str, range: Range) -> Result<GlobalType, PLDiag> {
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
        if let Some(father) = self.parent {
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
            None,
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
        builder.build_call(f, &[], &PLType::Void, self, None);
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
                if !fnvalue.fntype.st_method && !fnvalue.in_trait {
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
        if self.disable_diag {
            return dia;
        }
        if let Some(src) = &self.temp_source {
            dia.set_source(src);
        }
        let dia2 = dia.clone();
        self.diagnose.borrow_mut().insert(dia);
        dia2
    }
    /// # try_load2var
    ///
    /// load the element of a pointer from the builder,
    /// or get the ordinary types for the others
    pub fn try_load2var<'b>(
        &'b mut self,
        range: Range,
        v: ValueHandle,
        builder: &'b BuilderEnum<'a, 'ctx>,
        tp: &PLType,
    ) -> Result<ValueHandle, PLDiag> {
        builder.try_load2var(range, v, tp, self)
    }
    fn set_mod(&mut self, plmod: Mod) -> Mod {
        let m = self.plmod.clone();
        self.plmod = plmod;
        m
    }

    pub fn protect_generic_context<'b, T, F: FnMut(&mut Ctx<'a>) -> T>(
        &mut self,
        generic_map: &IndexMap<String, Arc<RefCell<PLType>>>,
        mut f: F,
    ) -> T {
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
        if u.get_path() != self.plmod.path && !u.get_path().is_empty() {
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

    /// # position_at_end
    ///
    /// it replace its current block field with the input block handle,
    /// and inserts it at the end of current builder
    pub fn position_at_end<'b>(
        &'b mut self,
        block: BlockHandle,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) {
        self.block = Some(block);
        builder.position_at_end_block(block);
    }

    /// # deref_greedily
    ///
    /// it de-references greedily until the result cannot be de-referenced anymore.
    pub fn deref_greedily<'b>(
        &'b mut self,
        tp: Arc<RefCell<PLType>>,
        value: ValueHandle,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> (Arc<RefCell<PLType>>, ValueHandle) {
        let mut tp = tp;
        let mut value = value;
        while let PLType::Pointer(p) = &*get_type_deep(tp.clone()).borrow() {
            let old_tp = tp.clone();
            tp = p.clone();
            value = builder.build_load(value, "load", &old_tp.borrow(), self);
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
        if let Some(father) = &self.parent {
            return father.get_macro(name);
        }
        None
    }

    fn diff_trait_impl(&mut self, l: &GenericType, r: &GenericType) -> Option<String> {
        let noop = BuilderEnum::NoOp(NoOpBuilder::default());
        // get it's pointer
        let noop_ptr = &noop as *const BuilderEnum<'a, '_>;
        let builder = unsafe { noop_ptr.as_ref().unwrap() };
        let binding = l
            .trait_impl
            .clone()
            .map(|e| e.get_types(self, builder).unwrap())
            .unwrap_or_default();
        let miss = binding
            .iter()
            .filter(|lf| {
                !r.trait_impl
                    .clone()
                    .map(|e| e.get_types(self, builder).unwrap())
                    .unwrap_or_default()
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
        } else if let Some(father) = &self.parent {
            father.try_set_closure_alloca_bb(bb);
        }
    }
}

/// # real_tp
///
/// PartialInferred is just a wrapper for real type
///
/// This function will unwrap it recursively
fn real_tp(pltype: Arc<RefCell<PLType>>) -> Arc<RefCell<PLType>> {
    match &*pltype.clone().borrow() {
        PLType::PartialInferred(p) => real_tp(p.clone()),
        _ => pltype,
    }
}

mod lsp;

mod completion;

mod cast;

pub use generic::EqRes;
