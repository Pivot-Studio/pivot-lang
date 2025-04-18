use super::super::cast::get_option_type;
use super::GeneratorType;
use super::Pos;

use crate::ast::builder::BlockHandle;
use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::ast::builder::ValueHandle;
use crate::ast::ctx::Ctx;
use crate::ast::ctx::PLSymbolData;
use crate::ast::diag::ErrorCode;

use crate::ast::diag::PLDiag;
use crate::ast::pltype::Callable;
use crate::ast::pltype::PriType;
use crate::ast::pltype::GLOB_COUNTER;
use crate::ast::tokens::TokenType;
use crate::ast::traits::CustomType;
use crate::inference::unknown_arc;

use linked_hash_map::LinkedHashMap;
use ustr::Ustr;

use std::cell::RefCell;
use std::sync::atomic::Ordering;
use std::sync::Arc;

use crate::ast::pltype::Field;

use crate::ast::pltype::STType;

use crate::ast::pltype::PLType;

#[derive(Clone, Default)]
pub struct GeneratorCtxData {
    pub table: LinkedHashMap<Ustr, PLSymbolData>,
    pub entry_bb: BlockHandle,
    pub ctx_handle: ValueHandle,       //handle in setup function
    pub ret_handle: ValueHandle,       //handle in setup function
    pub yield_ctx_handle: ValueHandle, //handle in yield function
    pub prev_yield_bb: Option<BlockHandle>,
    pub ctx_size_handle: ValueHandle,
    pub is_para: bool,
    pub para_offset: usize,
    // pub para_tmp: ValueHandle,
    pub ctx_tp: Option<Arc<RefCell<PLType>>>,
    pub ret_type: Option<Arc<RefCell<PLType>>>,
    pub generator_type: GeneratorType,
    pub blocks_may_yield: Vec<BlockHandle>, // blocks that may yield
}

/// # CtxFlag
///
/// flags that might change the behavior of the builder
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CtxFlag {
    Normal,
    InGeneratorYield,
}

// record some special infomration which is hard to retrieve usually
#[derive(Clone, Default)]
pub struct ClosureCtxData {
    // only the capture lists
    pub table: LinkedHashMap<Ustr, (PLSymbolData, ValueHandle, bool)>,

    // the logic of the closure
    pub data_handle: ValueHandle,

    // the trampoline function from llvm builder
    pub alloca_bb: Option<BlockHandle>,

    // closure type, all capture lists
    pub data_tp: Option<Arc<RefCell<PLType>>>,
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn end_generator<'a>(
    child: &mut Ctx<'a>,
    builder: &BuilderEnum<'a, '_>,
    i8ptr: PLType,
    mut sttp_opt: Option<STType>,
    funcvalue: usize,
    generator_alloca_b: usize,
    allocab: usize,
    is_closure: bool,
) -> Result<(), PLDiag> {
    child.ctx_flag = CtxFlag::Normal;
    let done = builder.get_cur_basic_block();
    let mut i = 1;

    // 1. 产生真实的generator_ctx对应的pltype
    let mut tps = vec![Arc::new(RefCell::new(i8ptr))];
    if child
        .generator_data
        .as_ref()
        .unwrap()
        .borrow()
        .generator_type
        .is_async()
    {
        tps.push(Arc::new(RefCell::new(PLType::new_i8_ptr())));
        tps.push(Arc::new(RefCell::new(PLType::new_i8_ptr())));
        if is_closure {
            tps.push(Arc::new(RefCell::new(PLType::new_i8_ptr())));
        }
    }
    let st_tp = sttp_opt.as_mut().unwrap();
    for (k, v) in &child.generator_data.as_ref().unwrap().borrow().table {
        let pltp = PLType::Pointer(v.pltype.to_owned());
        st_tp.fields.insert(
            *k,
            Field {
                index: i,
                typenode: pltp.get_typenode(&child.plmod.path),
                name: *k,
                range: Default::default(),
                modifier: None,
            },
        );
        tps.push(Arc::new(RefCell::new(pltp)));
        i += 1;
    }
    let data = child.generator_data.as_ref().unwrap().clone();
    // 2. 生成对应的gc遍历函数
    builder.gen_st_visit_function(child, st_tp, &tps);
    builder.position_at_end_block(data.borrow().entry_bb);
    // 3. 在setup函数中给返回值（接口）的对应字段赋值，并返回
    let b = data.borrow();
    let ptr = builder
        .build_struct_gep(
            b.ret_handle,
            1,
            "ctx_handle_gep",
            &b.ret_type.as_ref().unwrap().borrow(),
            child,
        )
        .unwrap();
    let ptr = builder.bitcast(
        child,
        ptr,
        &PLType::Pointer(Arc::new(RefCell::new(PLType::Pointer(Arc::new(
            RefCell::new(PLType::Struct(st_tp.clone())),
        ))))),
        "casted_ptr",
    );
    builder.build_store(ptr, data.borrow().ctx_handle);
    let ptr = builder
        .build_struct_gep(
            data.borrow().ret_handle,
            2,
            "ctx_handle_gep",
            &b.ret_type.as_ref().unwrap().borrow(),
            child,
        )
        .unwrap();
    unsafe { builder.store_with_aoto_cast(ptr, funcvalue) };
    builder.build_return(Some(data.borrow().ret_handle));

    // 4. 生成yield函数的done分支代码
    builder.position_at_end_block(generator_alloca_b);
    builder.build_unconditional_branch(data.borrow().entry_bb);
    if child
        .generator_data
        .as_ref()
        .unwrap()
        .borrow()
        .generator_type
        .is_iter()
    {
        builder.position_at_end_block(done);
        let flag = builder
            .build_struct_gep(
                child.return_block.unwrap().1.unwrap(),
                0,
                "flag",
                &b.ret_type.as_ref().unwrap().borrow(),
                child,
            )
            .unwrap();
        builder.build_store(flag, builder.int_value(&PriType::U64, 1, false));
        builder.build_unconditional_branch(child.return_block.unwrap().0);
    }

    // 5. 生成yield函数的跳转代码
    builder.position_at_end_block(allocab);
    let ctx_v = builder.get_nth_param(child.function.unwrap(), 0);
    let address = builder
        .build_struct_gep(
            ctx_v,
            1,
            "block_address",
            &data.borrow().ctx_tp.as_ref().unwrap().borrow(),
            child,
        )
        .unwrap();
    let address = builder.build_load(address, "block_address", &PLType::new_i8_ptr(), child);
    builder.build_indirect_br(
        address,
        &child
            .generator_data
            .as_ref()
            .unwrap()
            .borrow()
            .blocks_may_yield,
    );

    // 6. 用最终的generator_ctx大小修正之前的malloc语句
    builder.correct_generator_ctx_malloc_inst(child, &st_tp.get_full_name());
    Ok(())
}

/// # init_generator
///
/// 这个函数干几件事情：
///
/// 1. 创建generator的setup函数，该函数产生实际的generator结构体
/// 2. 创建generator_ctx结构体
/// 3. 创建generator_yield函数，替代上下文中函数为yield函数
/// 4. 将generator的基础信息写入上下文中，方便以后使用
#[allow(clippy::too_many_arguments)]
pub(crate) fn init_generator<'a, T: Callable + CustomType>(
    child: &mut Ctx<'a>,
    i8ptr: &PLType,
    fnvalue: &T,
    builder: &BuilderEnum<'a, '_>,
    funcvalue: &mut usize,
    generator_alloca_b: &mut usize,
    sttp_opt: &mut Option<STType>,
    generator_type: GeneratorType,
    is_closure: bool,
) -> Result<(), PLDiag> {
    child.generator_data = Some(Default::default());
    child
        .generator_data
        .as_ref()
        .unwrap()
        .borrow_mut()
        .generator_type = generator_type;
    let mut m = LinkedHashMap::default();
    m.insert(
        "address".into(),
        Field {
            index: 1,
            typenode: i8ptr.clone().get_typenode(&child.plmod.path),
            name: "address".into(),
            range: Default::default(),
            modifier: None,
        },
    );
    let para_offset = if generator_type == GeneratorType::Async {
        m.insert(
            "task".into(),
            Field {
                index: 2,
                typenode: i8ptr.clone().get_typenode(&child.plmod.path),
                name: "task".into(),
                range: Default::default(),
                modifier: None,
            },
        );
        m.insert(
            "final_ret".into(),
            Field {
                index: 3,
                typenode: i8ptr.clone().get_typenode(&child.plmod.path),
                name: "final_ret".into(),
                range: Default::default(),
                modifier: None,
            },
        );
        if is_closure {
            m.insert(
                "closure".into(),
                Field {
                    index: 4,
                    typenode: i8ptr.clone().get_typenode(&child.plmod.path),
                    name: "closure".into(),
                    range: Default::default(),
                    modifier: None,
                },
            );
            5
        } else {
            4
        }
    } else {
        2
    };
    child
        .generator_data
        .as_ref()
        .unwrap()
        .borrow_mut()
        .para_offset = para_offset;
    let name =
        "__generator_ctx".to_string() + &GLOB_COUNTER.fetch_add(1, Ordering::SeqCst).to_string();
    let st_tp = STType {
        name: name.into(),
        path: child.plmod.path,
        fields: m,
        range: Default::default(),
        doc: vec![],
        generic_map: Default::default(),
        derives: vec![],
        modifier: Some((TokenType::PUB, Default::default())),
        body_range: Default::default(),
        is_trait: false,
        is_tuple: false,
        generic_infer_types: Default::default(),
        // generic_infer: Default::default(),
        methods: Default::default(),
        trait_methods_impl: Default::default(),
        atomic: false,
    };
    builder.opaque_struct_type(&st_tp.get_full_name());
    builder.add_body_to_struct_type(&st_tp.get_full_name(), &st_tp, child);
    let rettp = child.run_in_type_mod(fnvalue, |child, fnvalue| {
        let tp = fnvalue.get_ret_ty(child, builder, false);
        let r = fnvalue.get_range();
        let f = |ctx| {
            r.new_err(ErrorCode::GENERATOR_FN_MUST_RET_ITER)
                .add_to_ctx(ctx)
        };
        let f2 = |ctx| r.new_err(ErrorCode::ASYNC_FN_MUST_RET_TASK).add_to_ctx(ctx);
        return match &*tp.borrow() {
            PLType::Trait(t) => {
                match generator_type {
                    GeneratorType::Iter => {
                        if t.name != "Iterator" {
                            return Err(f(child));
                        }
                    }
                    GeneratorType::Async => {
                        if t.name != "Task" && !t.name.starts_with("Task<") {
                            return Err(f2(child));
                        }
                    }
                    _ => unreachable!(),
                };
                Ok(t.generic_map.first().map(|(_, t)| t.clone()).unwrap_or(
                    t.generic_infer_types
                        .first()
                        .map(|(_, t)| t.clone())
                        .unwrap_or(unknown_arc()),
                ))
            }
            _ => {
                match generator_type {
                    GeneratorType::Iter => {
                        return Err(f(child));
                    }
                    GeneratorType::Async => {
                        return Err(f2(child));
                    }
                    _ => unreachable!(),
                };
            }
        };
    })?;
    let rettp = get_option_type(child, builder, rettp)?;
    child.rettp = Some(rettp.clone());
    let f = builder.add_generator_yield_fn(child, &st_tp.get_full_name(), &rettp.borrow());
    child.function = Some(f);
    let allocab = builder.append_basic_block(*funcvalue, "alloc");
    let param_tys = fnvalue.iter_param_tys(child, builder, false);
    builder.create_params_roots(*funcvalue, allocab, &param_tys);
    let entry = builder.append_basic_block(*funcvalue, "set_up_entry");
    builder.position_at_end_block(allocab);
    *generator_alloca_b = allocab;
    let old_f = *funcvalue;
    *funcvalue = f;
    builder.position_at_end_block(entry);
    let ctx_handle = builder.alloc("___ctx", &PLType::Struct(st_tp.clone()), child, None);
    child.generator_data.as_ref().unwrap().borrow_mut().entry_bb = entry;
    child
        .generator_data
        .as_ref()
        .unwrap()
        .borrow_mut()
        .ctx_handle = ctx_handle;

    child.generator_data.as_ref().unwrap().borrow_mut().ctx_tp =
        Some(Arc::new(RefCell::new(PLType::Struct(st_tp.clone()))));
    if is_closure {
        let para_ptr = builder
            .build_struct_gep(
                ctx_handle,
                4_u32,
                "closure",
                &child
                    .generator_data
                    .clone()
                    .unwrap()
                    .borrow_mut()
                    .ctx_tp
                    .as_ref()
                    .unwrap()
                    .borrow(),
                child,
            )
            .unwrap();
        let closure_in = builder.get_nth_param(old_f, 0);
        builder.build_store(para_ptr, closure_in);
    }
    *sttp_opt = Some(st_tp);
    Ok(())
}

pub(crate) fn save_generator_init_block<'a>(
    builder: &BuilderEnum<'a, '_>,
    child: &mut Ctx<'a>,
    entry: usize,
) {
    builder.position_at_end_block(child.generator_data.as_ref().unwrap().borrow().entry_bb);
    let address = builder.get_block_address(entry);
    child
        .generator_data
        .as_ref()
        .unwrap()
        .borrow_mut()
        .blocks_may_yield
        .push(entry);
    let data = child.generator_data.as_ref().unwrap().clone();
    let address_ptr = builder
        .build_struct_gep(
            data.borrow().ctx_handle,
            1,
            "block_address",
            &data.borrow().ctx_tp.as_ref().unwrap().borrow(),
            child,
        )
        .unwrap();
    builder.build_store(address_ptr, address);
}

pub(crate) fn build_generator_ret<'a, T: Callable + CustomType>(
    builder: &BuilderEnum<'a, '_>,
    child: &mut Ctx<'a>,
    fnvalue: &T,
    entry: usize,
    _allocab: usize,
) -> Result<Option<usize>, PLDiag> {
    builder.rm_curr_debug_location();
    let data = child.generator_data.as_ref().unwrap().clone();
    child.position_at_end(data.borrow().entry_bb, builder);
    let tp = child.rettp.clone().unwrap();
    let r = fnvalue.get_ret_ty(child, builder, true);
    match &*r.clone().borrow() {
        PLType::Void => unreachable!(),
        other => {
            builder.rm_curr_debug_location();
            data.borrow_mut().ret_handle =
                builder.alloc("retvalue1", other, child, Some(Pos::default()));
            data.borrow_mut().ret_type = Some(r);
        }
    }

    child.position_at_end(entry, builder);
    let retv = builder.alloc(
        "retvalue_generator",
        &tp.borrow(),
        child,
        Some(Pos::default()),
    );
    Ok(Some(retv))
}
