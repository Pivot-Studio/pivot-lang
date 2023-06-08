use super::builder::BlockHandle;
use super::builder::ValueHandle;
use super::diag::ErrorCode;
use super::diag::PLDiag;

use super::node::macro_nodes::MacroNode;
use super::node::node_result::NodeResult;
use super::node::NodeEnum;
use super::node::TypeNode;
use super::plmod::CompletionItemWrapper;
use super::plmod::GlobalVar;
use super::plmod::LSPDef;
use super::plmod::Mod;
use super::plmod::MutVec;
use super::pltype::add_primitive_types;
use super::pltype::get_type_deep;
use super::pltype::FNValue;
use super::pltype::ImplAble;
use super::pltype::PLType;
use super::pltype::PriType;
use super::pltype::TraitImplAble;
use super::range::Pos;
use super::range::Range;
use super::tokens::TokenType;
use super::traits::CustomType;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::format_label;
use crate::lsp::semantic_tokens::type_index;

use crate::mismatch_err;
use crate::skip_if_not_modified_by;
use crate::utils::read_config::Config;
use crate::utils::url_from_path;
use crate::Db;

use indexmap::IndexMap;
use linked_hash_map::LinkedHashMap;
use lsp_types::Command;
use lsp_types::CompletionItem;
use lsp_types::CompletionItemKind;

use lsp_types::Hover;
use lsp_types::HoverContents;
use lsp_types::InlayHint;
use lsp_types::InlayHintKind;
use lsp_types::InsertTextFormat;
use lsp_types::Location;
use lsp_types::MarkedString;
use lsp_types::SemanticTokenType;
use lsp_types::Url;
use rustc_hash::FxHashMap;
use rustc_hash::FxHashSet;
use std::cell::RefCell;

use std::path::Path;

use std::path::PathBuf;
use std::sync::Arc;
mod builtins;
mod references;

pub use builtins::*;
#[derive(Clone)]
pub struct PLSymbol {
    pub value: ValueHandle,
    pub pltype: Arc<RefCell<PLType>>,
    pub range: Range,
    pub refs: Option<Arc<MutVec<Location>>>,
}
/// # Ctx
/// Context for code generation
pub struct Ctx<'a> {
    pub generic_types: FxHashMap<String, Arc<RefCell<PLType>>>,
    pub need_highlight: usize,
    pub plmod: Mod,
    pub father: Option<&'a Ctx<'a>>, // father context, for symbol lookup
    pub root: Option<&'a Ctx<'a>>,   // root context, for symbol lookup
    pub function: Option<ValueHandle>, // current function
    pub init_func: Option<ValueHandle>, //init function,call first in main
    pub roots: RefCell<Vec<ValueHandle>>,
    pub block: Option<BlockHandle>,          // current block
    pub continue_block: Option<BlockHandle>, // the block to jump when continue
    pub break_block: Option<BlockHandle>,    // the block to jump to when break
    pub return_block: Option<(BlockHandle, Option<ValueHandle>)>, // the block to jump to when return and value
    pub errs: &'a RefCell<FxHashSet<PLDiag>>,                     // diagnostic list
    pub edit_pos: Option<Pos>,                                    // lsp params
    pub table: FxHashMap<String, PLSymbol>,                       // variable table
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
    // pub trait_mthd_table: TraitMthdImpl,
}

#[derive(Clone, Default)]
pub struct ClosureCtxData {
    pub table: LinkedHashMap<String, (PLSymbol, ValueHandle)>,
    pub data_handle: ValueHandle,
    pub alloca_bb: Option<BlockHandle>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum MacroReplaceNode {
    NodeEnum(NodeEnum),
    LoopNodeEnum(Vec<NodeEnum>),
}

impl<'a, 'ctx> Ctx<'a> {
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
    ) -> Ctx<'a> {
        let f = Path::new(Path::new(src_file_path).file_stem().unwrap())
            .file_name()
            .take()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();
        Ctx {
            need_highlight: 0,
            generic_types: FxHashMap::default(),
            plmod: Mod::new(f, src_file_path.to_string()),
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
            roots: RefCell::new(Vec::new()),
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
        }
    }
    pub fn new_child(&'a self, start: Pos, builder: &'a BuilderEnum<'a, 'ctx>) -> Ctx<'a> {
        let mut root = self.root;
        if self.father.is_none() {
            root = Some(self);
        }
        let mut ctx = Ctx {
            need_highlight: self.need_highlight,
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
            roots: RefCell::new(Vec::new()),
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
    pub fn up_cast<'b>(
        &mut self,
        target_pltype: Arc<RefCell<PLType>>,
        ori_pltype: Arc<RefCell<PLType>>,
        target_range: Range,
        ori_range: Range,
        ori_value: usize,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<usize, PLDiag> {
        if let (PLType::Closure(c), PLType::Fn(f)) =
            (&*target_pltype.borrow(), &*ori_pltype.borrow())
        {
            if f.to_closure_ty(self, builder) != *c {
                return Err(ori_range
                    .new_err(ErrorCode::FUNCTION_TYPE_NOT_MATCH)
                    .add_label(
                        target_range,
                        self.get_file(),
                        format_label!("expected type `{}`", c.get_name()),
                    )
                    .add_label(
                        ori_range,
                        self.get_file(),
                        format_label!("found type `{}`", f.to_closure_ty(self, builder).get_name()),
                    )
                    .add_to_ctx(self));
            }
            if ori_value == usize::MAX {
                return Err(ori_range
                    .new_err(ErrorCode::CANNOT_ASSIGN_INCOMPLETE_GENERICS)
                    .add_help("try add generic type explicitly to fix this error.")
                    .add_to_ctx(self));
            }
            let closure_v = builder.alloc("tmp", &target_pltype.borrow(), self, None);
            let closure_f = builder.build_struct_gep(closure_v, 0, "closure_f").unwrap();
            let ori_value = builder.try_load2var(ori_range, ori_value, self)?;
            builder.build_store(closure_f, builder.get_closure_trampoline(ori_value));
            return Ok(closure_v);
        }
        if let PLType::Union(u) = &*target_pltype.borrow() {
            let union_members = self.run_in_type_mod(u, |ctx, u| {
                let mut union_members = vec![];
                for tp in &u.sum_types {
                    let tp = tp.get_type(ctx, builder, true)?;
                    union_members.push(tp);
                }
                Ok(union_members)
            })?;
            for (i, tp) in union_members.iter().enumerate() {
                if *tp.borrow() == *ori_pltype.borrow() {
                    let union_handle =
                        builder.alloc("tmp_unionv", &target_pltype.borrow(), self, None);
                    let union_value = builder
                        .build_struct_gep(union_handle, 1, "union_value")
                        .unwrap();
                    let union_type_field = builder
                        .build_struct_gep(union_handle, 0, "union_type")
                        .unwrap();
                    let union_type = builder.int_value(&PriType::U64, i as u64, false);
                    builder.build_store(union_type_field, union_type);
                    let mut ptr = ori_value;
                    if !builder.is_ptr(ori_value) {
                        // mv to heap
                        ptr = builder.alloc("tmp", &ori_pltype.borrow(), self, None);
                        builder.build_store(ptr, ori_value);
                    }
                    let st_value = builder.bitcast(
                        self,
                        ptr,
                        &PLType::Pointer(Arc::new(RefCell::new(PLType::Primitive(PriType::I8)))),
                        "traitcast_tmp",
                    );
                    builder.build_store(union_value, st_value);

                    return Ok(union_handle);
                }
            }
        }
        let (st_pltype, st_value) = self.auto_deref(ori_pltype, ori_value, builder);
        if let (PLType::Trait(t), PLType::Struct(st)) =
            (&*target_pltype.borrow(), &*st_pltype.borrow())
        {
            if !st.implements_trait(t, &self.get_root_ctx().plmod) {
                return Err(mismatch_err!(
                    self,
                    ori_range,
                    target_range,
                    target_pltype.borrow(),
                    st_pltype.borrow()
                ));
            }
            let trait_handle = builder.alloc("tmp_traitv", &target_pltype.borrow(), self, None);
            for f in t.list_trait_fields().iter() {
                let mthd = find_mthd(st, f, t).unwrap_or_else(|| {
                    for t in &t.derives {
                        match &*t.borrow() {
                            PLType::Trait(t) => {
                                if let Some(mthd) = find_mthd(st, f, t) {
                                    return mthd;
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                    unreachable!()
                });

                // TODO: let a:trait = B<i64>{} panic
                let fnhandle = builder.get_or_insert_fn_handle(&mthd.borrow(), self);
                let targetftp = f.typenode.get_type(self, builder, true).unwrap();
                let casted = builder.bitcast(self, fnhandle, &targetftp.borrow(), "fncast_tmp");
                let f_ptr = builder
                    .build_struct_gep(trait_handle, f.index, "field_tmp")
                    .unwrap();
                builder.build_store(f_ptr, casted);
            }
            let st_value = builder.bitcast(
                self,
                st_value,
                &PLType::Pointer(Arc::new(RefCell::new(PLType::Primitive(PriType::I64)))),
                "traitcast_tmp",
            );
            let v_ptr = builder.build_struct_gep(trait_handle, 1, "v_tmp").unwrap();
            builder.build_store(v_ptr, st_value);
            let type_hash = builder
                .build_struct_gep(trait_handle, 0, "tp_hash")
                .unwrap();
            let hash = st.get_type_code();
            let hash = builder.int_value(&PriType::U64, hash, false);
            builder.build_store(type_hash, hash);
            return Ok(trait_handle);
        }
        #[allow(clippy::needless_return)]
        return Err(mismatch_err!(
            self,
            ori_range,
            target_range,
            target_pltype.borrow(),
            st_pltype.borrow()
        ));
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
    pub fn clear_init_fn<'b>(&'b self, builder: &'b BuilderEnum<'a, 'ctx>) {
        let alloc = builder.get_first_basic_block(self.init_func.unwrap());
        let entry = builder.get_last_basic_block(self.init_func.unwrap());
        builder.delete_block(alloc);
        builder.delete_block(entry);
        builder.append_basic_block(self.init_func.unwrap(), "alloc");
        builder.append_basic_block(self.init_func.unwrap(), "entry");
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
                if st.get_path() != self.get_file() {
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
                    if table.insert(mthd.to_string(), fntp.clone()).is_some() {
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
            PLType::Union(u) => self.add_method_to_tp(u, mthd, fntp, impl_trait, generic, target),
            PLType::Closure(p) => {
                self.add_trait_impl_method(p, mthd, fntp, impl_trait, generic, target)
            }
            PLType::Primitive(p) => {
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
    ) -> Option<(PLSymbol, bool)> {
        let v = self.table.get(name);
        if let Some(symbol) = v {
            return Some((symbol.clone(), false));
        }
        if let Some(data) = &self.closure_data {
            let mut data = data.borrow_mut();
            if let Some((symbol, _)) = data.table.get(name) {
                let new_symbol = symbol.clone();
                return Some((new_symbol, false));
            } else if let Some(father) = self.father {
                let re = father.get_symbol(name, builder);
                if let Some((symbol, is_glob)) = &re {
                    if !*is_glob {
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
                        let new_symbol = PLSymbol {
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
                        return Some((new_symbol, false));
                    }
                }
                return re;
            }
        }
        if let Some(father) = self.father {
            let re = father.get_symbol(name, builder);
            return re;
        }
        if let Some(GlobalVar { tp: pltype, range }) = self.plmod.get_global_symbol(name) {
            return Some((
                PLSymbol {
                    value: builder
                        .get_global_var_handle(&self.plmod.get_full_name(name))
                        .unwrap(),
                    pltype: pltype.clone(),
                    range: *range,
                    refs: None,
                },
                true,
            ));
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
    ) -> Result<(), PLDiag> {
        if self.table.contains_key(&name) {
            return Err(self.add_diag(range.new_err(ErrorCode::REDECLARATION)));
        }
        self.add_symbol_without_check(name, pv, pltype, range, is_const)
    }
    pub fn add_symbol_without_check(
        &mut self,
        name: String,
        pv: ValueHandle,
        pltype: Arc<RefCell<PLType>>,
        range: Range,
        is_const: bool,
    ) -> Result<(), PLDiag> {
        if is_const {
            self.set_glob_refs(&self.plmod.get_full_name(&name), range);
            self.plmod.add_global_symbol(name, pltype, range)?;
        } else {
            let refs = Arc::new(RefCell::new(vec![]));
            self.table.insert(
                name,
                PLSymbol {
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
            PLSymbol {
                value: pv,
                pltype,
                range,
                refs: Some(refs),
            },
        );
    }
    pub fn get_type(&self, name: &str, range: Range) -> Result<Arc<RefCell<PLType>>, PLDiag> {
        if let Some(pv) = self.generic_types.get(name) {
            self.set_if_refs_tp(pv.clone(), range);
            self.send_if_go_to_def(
                range,
                pv.borrow().get_range().unwrap_or(range),
                self.plmod.path.clone(),
            );
            return Ok(pv.clone());
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
    /// 用来获取外部模块的全局变量
    /// 如果没在当前module的全局变量表中找到，将会生成一个
    /// 该全局变量的声明
    pub fn get_or_add_global<'b>(
        &'b mut self,
        name: &str,
        pltype: Arc<RefCell<PLType>>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> ValueHandle {
        builder.get_or_add_global(name, pltype, self)
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
        self.plmod.types.insert(name, pltype);
        Ok(())
    }
    pub fn add_type_without_check(&mut self, pltype: Arc<RefCell<PLType>>) {
        if let PLType::Generic(_) = &*pltype.borrow() {
            unreachable!()
        }
        let name = pltype.borrow().get_name();
        self.plmod.types.insert(name, pltype);
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
    pub fn if_completion(
        &self,
        range: Range,
        get_completions: impl FnOnce() -> Vec<CompletionItem>,
    ) {
        if let Some(pos) = self.edit_pos {
            if pos.is_in(range) && !self.plmod.completion_gened.borrow().is_true() {
                let comps = get_completions();
                let comps = comps.iter().map(|x| CompletionItemWrapper(x.clone()));
                self.plmod.completions.borrow_mut().truncate(0);
                self.plmod.completions.borrow_mut().extend(comps);
                self.plmod.completion_gened.borrow_mut().set_true();
            }
        }
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
        let p = PathBuf::from(&u.get_path());
        let mut oldm = None;
        if u.get_path() != self.plmod.path {
            let s = p.file_name().unwrap().to_str().unwrap();
            let m = s.split('.').next().unwrap();
            let m = self.plmod.submods.get(m).unwrap();
            oldm = Some(self.set_mod(m.clone()));
        }
        let res = f(self, u);
        if let Some(m) = oldm {
            self.set_mod(m);
        }
        res
    }

    pub fn run_in_origin_mod<'b, R, F: FnMut(&mut Ctx<'a>) -> R>(&'b mut self, mut f: F) -> R {
        let oldm = self.plmod.clone();
        let ctx = self.get_root_ctx();
        self.set_mod(ctx.plmod.clone());
        let res = f(self);
        self.set_mod(oldm);
        res
    }
    pub fn run_in_type_mod_mut<'b, TP: CustomType, R, F: FnMut(&mut Ctx<'a>, &mut TP) -> R>(
        &'b mut self,
        u: &mut TP,
        mut f: F,
    ) -> R {
        let p = PathBuf::from(&u.get_path());
        let mut oldm = None;
        if u.get_path() != self.plmod.path {
            let s = p.file_name().unwrap().to_str().unwrap();
            let m = s.split('.').next().unwrap();
            let m = self.plmod.submods.get(m).unwrap();
            oldm = Some(self.set_mod(m.clone()));
        }
        let res = f(self, u);
        if let Some(m) = oldm {
            self.set_mod(m);
        }
        res
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

    pub fn send_if_go_to_def(&self, range: Range, destrange: Range, file: String) {
        if range == Default::default() {
            return;
        }
        self.plmod.defs.borrow_mut().insert(
            range,
            LSPDef::Scalar(Location {
                uri: url_from_path(&file),
                range: destrange.to_diag_range(),
            }),
        );
    }

    pub fn get_completions(&self) -> Vec<CompletionItem> {
        let mut m = FxHashMap::default();
        self.get_pltp_completions(&mut m, |_| true);
        self.plmod.get_ns_completions_pri(&mut m);
        self.get_var_completions(&mut m);
        self.get_keyword_completions(&mut m);
        self.get_macro_completions(&mut m);

        let cm = m.values().cloned().collect();
        cm
    }

    pub fn get_completions_in_ns(&self, ns: &str) -> Vec<CompletionItem> {
        let mut m = FxHashMap::default();
        self.get_const_completions_in_ns(ns, &mut m);
        self.get_type_completions_in_ns(ns, &mut m);
        self.get_macro_completion_in_ns(ns, &mut m);

        let cm = m.values().cloned().collect();
        cm
    }

    fn with_ns(&self, ns: &str, f: impl FnOnce(&Mod)) {
        let ns = self.plmod.submods.get(ns);
        if let Some(ns) = ns {
            f(ns);
        }
    }

    fn get_const_completions_in_ns(&self, ns: &str, m: &mut FxHashMap<String, CompletionItem>) {
        self.with_ns(ns, |ns| {
            for (k, v) in ns.global_table.iter() {
                let mut item = CompletionItem {
                    label: k.to_string(),
                    kind: Some(CompletionItemKind::CONSTANT),
                    ..Default::default()
                };
                item.detail = Some(v.tp.borrow().get_name());
                m.insert(k.clone(), item);
            }
        });
    }

    fn get_type_completions_in_ns(&self, ns: &str, m: &mut FxHashMap<String, CompletionItem>) {
        self.with_ns(ns, |ns| {
            for (k, v) in ns.types.iter() {
                let mut insert_text = None;
                let mut command = None;
                let tp = match &*v.clone().borrow() {
                    PLType::Struct(s) => {
                        skip_if_not_modified_by!(s.modifier, TokenType::PUB);
                        CompletionItemKind::STRUCT
                    }
                    PLType::Fn(fnvalue) => {
                        skip_if_not_modified_by!(fnvalue.fntype.modifier, TokenType::PUB);
                        insert_text = Some(fnvalue.gen_snippet());
                        command = Some(Command::new(
                            "trigger help".to_string(),
                            "editor.action.triggerParameterHints".to_string(),
                            None,
                        ));
                        CompletionItemKind::FUNCTION
                    }
                    _ => continue, // skip completion for primary types
                };
                let mut item = CompletionItem {
                    label: k.to_string(),
                    kind: Some(tp),
                    insert_text,
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    command,
                    ..Default::default()
                };
                item.detail = Some(k.to_string());
                m.insert(k.clone(), item);
            }
        });
    }

    fn get_macro_completion_in_ns(&self, ns: &str, m: &mut FxHashMap<String, CompletionItem>) {
        self.with_ns(ns, |ns| {
            ns.get_macro_completions(m);
        });
    }

    pub fn get_type_completions(&self) -> Vec<CompletionItem> {
        let mut m = FxHashMap::default();
        self.get_pltp_completions(&mut m, |tp| !matches!(tp, PLType::Fn(_)));
        self.plmod.get_ns_completions_pri(&mut m);
        m.values().cloned().collect()
    }

    fn get_var_completions(&self, vmap: &mut FxHashMap<String, CompletionItem>) {
        for (k, _) in self.table.iter() {
            vmap.insert(
                k.to_string(),
                CompletionItem {
                    label: k.to_string(),
                    kind: Some(CompletionItemKind::VARIABLE),
                    ..Default::default()
                },
            );
        }
        if let Some(father) = self.father {
            father.get_var_completions(vmap);
        }
    }

    fn get_macro_completions(&self, vmap: &mut FxHashMap<String, CompletionItem>) {
        self.plmod.get_macro_completions(vmap);
        if let Some(father) = self.father {
            father.get_macro_completions(vmap);
        }
    }

    fn get_builtin_completions(&self, vmap: &mut FxHashMap<String, CompletionItem>) {
        for (name, handle) in BUILTIN_FN_NAME_MAP.iter() {
            vmap.insert(
                name.to_string(),
                CompletionItem {
                    label: name.to_string(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    insert_text: Some(BUILTIN_FN_SNIPPET_MAP.get(handle).unwrap().to_string()),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                },
            );
        }
    }

    fn get_pltp_completions(
        &self,
        vmap: &mut FxHashMap<String, CompletionItem>,
        filter: impl Fn(&PLType) -> bool,
    ) {
        self.plmod
            .get_pltp_completions(vmap, &filter, &self.generic_types, true);
        self.get_builtin_completions(vmap);
        if let Some(father) = self.father {
            father.get_pltp_completions(vmap, filter);
        }
    }

    pub fn push_semantic_token(&self, range: Range, tp: SemanticTokenType, modifiers: u32) {
        if self.need_highlight != 0 || self.in_macro {
            return;
        }
        self.plmod.semantic_tokens_builder.borrow_mut().push(
            range.to_diag_range(),
            type_index(tp),
            modifiers,
        )
    }
    pub fn push_type_hints(&self, range: Range, pltype: Arc<RefCell<PLType>>) {
        if self.need_highlight != 0 || self.in_macro {
            return;
        }
        let hint = InlayHint {
            position: range.to_diag_range().end,
            label: lsp_types::InlayHintLabel::String(
                ": ".to_string() + &pltype.borrow().get_name(),
            ),
            kind: Some(InlayHintKind::TYPE),
            text_edits: None,
            tooltip: None,
            padding_left: None,
            padding_right: None,
            data: None,
        };
        self.plmod.hints.borrow_mut().push(hint);
    }
    pub fn push_param_hint(&self, range: Range, name: String) {
        if self.need_highlight != 0 || self.in_macro {
            return;
        }
        let hint = InlayHint {
            position: range.to_diag_range().start,
            label: lsp_types::InlayHintLabel::String(name + ": "),
            kind: Some(InlayHintKind::TYPE),
            text_edits: None,
            tooltip: None,
            padding_left: None,
            padding_right: None,
            data: None,
        };
        self.plmod.hints.borrow_mut().push(hint);
    }
    pub fn position_at_end<'b>(
        &'b mut self,
        block: BlockHandle,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) {
        self.block = Some(block);
        builder.position_at_end_block(block);
    }
    fn get_keyword_completions(&self, vmap: &mut FxHashMap<String, CompletionItem>) {
        let keywords = vec![
            "if", "else", "while", "for", "return", "struct", "let", "true", "false", "as", "is",
        ];
        let loopkeys = vec!["break", "continue"];
        let toplevel = vec![
            "fn", "struct", "const", "use", "impl", "trait", "pub", "type",
        ];
        if self.father.is_none() {
            for k in toplevel {
                vmap.insert(
                    k.to_string(),
                    CompletionItem {
                        label: k.to_string(),
                        kind: Some(CompletionItemKind::KEYWORD),
                        ..Default::default()
                    },
                );
            }
        } else {
            for k in keywords {
                vmap.insert(
                    k.to_string(),
                    CompletionItem {
                        label: k.to_string(),
                        kind: Some(CompletionItemKind::KEYWORD),
                        ..Default::default()
                    },
                );
            }
        }
        if self.break_block.is_some() && self.continue_block.is_some() {
            for k in loopkeys {
                vmap.insert(
                    k.to_string(),
                    CompletionItem {
                        label: k.to_string(),
                        kind: Some(CompletionItemKind::KEYWORD),
                        ..Default::default()
                    },
                );
            }
        }
    }

    pub fn save_if_comment_doc_hover(&self, range: Range, docs: Option<Vec<Box<NodeEnum>>>) {
        if self.need_highlight != 0 {
            return;
        }
        let mut content = vec![];
        let mut string = String::new();
        if let Some(docs) = docs {
            for doc in docs {
                if let NodeEnum::Comment(c) = *doc {
                    string.push_str(&c.comment);
                    string.push('\n');
                }
            }
        }
        content.push(MarkedString::String(string));
        self.save_if_hover(range, HoverContents::Array(content))
    }

    pub fn save_if_hover(&self, range: Range, value: HoverContents) {
        if self.need_highlight != 0 {
            return;
        }
        self.plmod.hovers.borrow_mut().insert(
            range,
            Hover {
                range: None,
                contents: value,
            },
        );
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
    // when need eq trait and sttype,the left mut be trait
    pub fn eq(&self, l: Arc<RefCell<PLType>>, r: Arc<RefCell<PLType>>) -> EqRes {
        if let (PLType::Generic(l), PLType::Generic(r)) = (&*l.borrow(), &*r.borrow()) {
            if l == r {
                return EqRes {
                    eq: true,
                    need_up_cast: false,
                };
            }
        }
        if matches!(&*l.borrow(), PLType::Generic(_)) {
            if let PLType::Generic(lg) = &mut *l.borrow_mut() {
                if lg.curpltype.is_some() {
                    return self.eq(lg.curpltype.as_ref().unwrap().clone(), r);
                }
                if lg.trait_impl.is_some() {
                    if let PLType::Generic(r) = &*r.borrow() {
                        if r.trait_impl != lg.trait_impl {
                            return EqRes {
                                eq: false,
                                need_up_cast: false,
                            };
                        }
                    } else if lg
                        .trait_impl
                        .as_ref()
                        .unwrap()
                        .iter()
                        .any(|lt| !self.eq(lt.clone(), r.clone()).eq)
                    {
                        return EqRes {
                            eq: false,
                            need_up_cast: false,
                        };
                    }
                }
                lg.set_type(r);
                return EqRes {
                    eq: true,
                    need_up_cast: false,
                };
            }
            unreachable!()
        }
        if l != r {
            if matches!(&*l.borrow(), PLType::Union(_)) {
                return EqRes {
                    eq: true,
                    need_up_cast: true,
                };
            }
            let trait_pltype = l;
            let st_pltype = self.auto_deref_tp(r);
            if let (PLType::Trait(t), PLType::Struct(st)) =
                (&*trait_pltype.borrow(), &*st_pltype.borrow())
            {
                return EqRes {
                    eq: st.implements_trait(t, &self.get_root_ctx().plmod),
                    need_up_cast: true,
                };
            }
            return EqRes {
                eq: false,
                need_up_cast: false,
            };
        }
        EqRes {
            eq: true,
            need_up_cast: false,
        }
    }
    pub fn try_set_closure_alloca_bb(&self, bb: BlockHandle) {
        if let Some(c) = &self.closure_data {
            c.borrow_mut().alloca_bb = Some(bb);
        } else if let Some(father) = &self.father {
            father.try_set_closure_alloca_bb(bb);
        }
    }
}

fn find_mthd(
    st: &super::pltype::STType,
    f: &super::pltype::Field,
    t: &super::pltype::STType,
) -> Option<Arc<RefCell<FNValue>>> {
    st.find_method(&f.name).or(t
        .trait_methods_impl
        .borrow()
        .get(&st.get_full_name())
        .or(t
            .trait_methods_impl
            .borrow()
            .get(&st.get_full_name_except_generic()))
        .and_then(|v| v.get(&f.name))
        .cloned())
}
pub struct EqRes {
    pub eq: bool,
    pub need_up_cast: bool,
}
