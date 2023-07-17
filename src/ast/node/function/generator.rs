use super::super::cast::get_option_type;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::ast::ctx::Ctx;
use crate::ast::diag::ErrorCode;

use crate::ast::diag::PLDiag;
use crate::ast::node::TypeNode;
use crate::ast::pltype::PriType;
use crate::ast::tokens::TokenType;

use linked_hash_map::LinkedHashMap;

use crate::ast::pltype::FNValue;

use std::cell::RefCell;
use std::sync::Arc;

use crate::ast::pltype::Field;

use crate::ast::ctx::CtxFlag;

use crate::ast::pltype::STType;

use crate::ast::node::RangeTrait;
use crate::ast::pltype::PLType;

pub(crate) fn end_generator<'a>(
    child: &mut Ctx<'a>,
    builder: &BuilderEnum<'a, '_>,
    i8ptr: PLType,
    mut sttp_opt: Option<STType>,
    funcvalue: usize,
    generator_alloca_b: usize,
    allocab: usize,
) -> Result<(), PLDiag> {
    child.ctx_flag = CtxFlag::Normal;
    let done = builder.get_cur_basic_block();
    let mut i = 1;

    // 1. 产生真实的generator_ctx对应的pltype
    let mut tps = vec![Arc::new(RefCell::new(i8ptr))];
    let st_tp = sttp_opt.as_mut().unwrap();
    for (k, v) in &child.generator_data.as_ref().unwrap().borrow().table {
        let pltp = PLType::Pointer(v.pltype.to_owned());
        st_tp.fields.insert(
            k.to_owned(),
            Field {
                index: i,
                typenode: pltp.get_typenode(&child.plmod.path),
                name: k.to_owned(),
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
    let ptr = builder
        .build_struct_gep(data.borrow().ret_handle, 1, "ctx_handle_gep")
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
        .build_struct_gep(data.borrow().ret_handle, 2, "ctx_handle_gep")
        .unwrap();
    unsafe { builder.store_with_aoto_cast(ptr, funcvalue) };
    let ret_load = builder.build_load(data.borrow().ret_handle, "ret_load");
    builder.build_return(Some(ret_load));

    // 4. 生成yield函数的done分支代码
    builder.position_at_end_block(generator_alloca_b);
    builder.build_unconditional_branch(data.borrow().entry_bb);
    builder.position_at_end_block(done);
    let flag = builder
        .build_struct_gep(child.return_block.unwrap().1.unwrap(), 0, "flag")
        .unwrap();
    builder.build_store(flag, builder.int_value(&PriType::U64, 1, false));
    builder.build_unconditional_branch(child.return_block.unwrap().0);

    // 5. 生成yield函数的跳转代码
    builder.position_at_end_block(allocab);
    let ctx_v = builder.get_nth_param(child.function.unwrap(), 0);
    let address = builder.build_struct_gep(ctx_v, 1, "block_address").unwrap();
    let address = builder.build_load(address, "block_address");
    builder.build_indirect_br(address, child);

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
pub(crate) fn init_generator<'a>(
    child: &mut Ctx<'a>,
    i8ptr: &PLType,
    fnvalue: &FNValue,
    builder: &BuilderEnum<'a, '_>,
    funcvalue: &mut usize,
    generator_alloca_b: &mut usize,
    sttp_opt: &mut Option<STType>,
) -> Result<(), PLDiag> {
    child.generator_data = Some(Default::default());
    let mut m = LinkedHashMap::default();
    m.insert(
        "address".to_owned(),
        Field {
            index: 1,
            typenode: i8ptr.clone().get_typenode(&child.plmod.path),
            name: "address".to_owned(),
            range: Default::default(),
            modifier: None,
        },
    );
    let st_tp = STType {
        name: fnvalue.get_generator_ctx_name(),
        path: child.plmod.path.clone(),
        fields: m,
        range: Default::default(),
        doc: vec![],
        generic_map: Default::default(),
        derives: vec![],
        modifier: Some((TokenType::PUB, Default::default())),
        body_range: Default::default(),
        is_trait: false,
        is_tuple: true,
        generic_infer_types: Default::default(),
        // generic_infer: Default::default(),
        methods: Default::default(),
        trait_methods_impl: Default::default(),
    };
    builder.opaque_struct_type(&st_tp.get_full_name());
    builder.add_body_to_struct_type(&st_tp.get_full_name(), &st_tp, child);
    let rettp = child.run_in_type_mod(fnvalue, |child, fnvalue| {
        let tp = fnvalue.fntype.ret_pltype.get_type(child, builder, false)?;
        let r = fnvalue.fntype.ret_pltype.range();
        let f = |ctx| {
            r.new_err(ErrorCode::GENERATOR_FN_MUST_RET_ITER)
                .add_to_ctx(ctx)
        };
        return match &*tp.borrow() {
            PLType::Trait(t) => {
                if t.name != "Iterator" {
                    return Err(f(child));
                }
                Ok(t.generic_map.first().unwrap().1.clone())
            }
            _ => Err(f(child)),
        };
    })?;
    let rettp = get_option_type(child, builder, rettp)?;
    child.rettp = Some(rettp.clone());
    let f = builder.add_generator_yield_fn(child, &st_tp.get_full_name(), &rettp.borrow());
    child.function = Some(f);
    let allocab = builder.append_basic_block(*funcvalue, "alloc");
    let entry = builder.append_basic_block(*funcvalue, "set_up_entry");
    builder.position_at_end_block(allocab);
    *generator_alloca_b = allocab;
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
    *sttp_opt = Some(st_tp);
    Ok(())
}

pub(crate) fn save_generator_init_block<'a>(
    builder: &BuilderEnum<'a, '_>,
    child: &mut Ctx<'a>,
    entry: usize,
) {
    let address = builder.get_block_address(entry);
    let data = child.generator_data.as_ref().unwrap().clone();
    let address_ptr = builder
        .build_struct_gep(data.borrow().ctx_handle, 1, "block_address")
        .unwrap();
    builder.build_store(address_ptr, address);
}

pub(crate) fn build_generator_ret<'a>(
    builder: &BuilderEnum<'a, '_>,
    child: &mut Ctx<'a>,
    fnvalue: &FNValue,
    entry: usize,
) -> Result<Option<usize>, PLDiag> {
    builder.rm_curr_debug_location();
    let data = child.generator_data.as_ref().unwrap().clone();
    child.position_at_end(data.borrow().entry_bb, builder);
    let tp = child.rettp.clone().unwrap();
    match &*fnvalue
        .fntype
        .ret_pltype
        .get_type(child, builder, true)?
        .borrow()
    {
        PLType::Void => unreachable!(),
        other => {
            builder.rm_curr_debug_location();
            data.borrow_mut().ret_handle = builder.alloc("retvalue", other, child, None);
        }
    }
    child.position_at_end(entry, builder);
    let retv = builder.stack_alloc("retvalue", child, &tp.borrow());
    Ok(Some(retv))
}
