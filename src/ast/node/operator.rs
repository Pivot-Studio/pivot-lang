use std::sync::Arc;

use super::node_result::NodeResultBuilder;
use super::primary::VarNode;
use super::*;

use super::super::builder::IntPredicate;
use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::ast::ctx::Ctx;
use crate::ast::diag::ErrorCode;
use crate::ast::pltype::get_type_deep;
use crate::ast::pltype::PLType;
use crate::ast::pltype::PriType;
use crate::ast::tokens::TokenType;
use crate::format_label;
use crate::handle_calc;
use internal_macro::node;
use lsp_types::SemanticTokenType;
#[node]
pub struct UnaryOpNode {
    pub op: (TokenType, Range),
    pub exp: Box<NodeEnum>,
}

impl PrintTrait for UnaryOpNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("UnaryOpNode");
        tab(tabs + 1, line.clone(), end);
        println!("{:?}", self.op);
        self.exp.print(tabs + 1, true, line.clone());
    }
}

// 单目运算符
impl Node for UnaryOpNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        let exp_range = self.exp.range();
        let rv = self.exp.emit(ctx, builder)?.get_value();
        if rv.is_none() {
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::INVALID_UNARY_EXPRESSION)));
        }
        let rv = rv.unwrap();
        let pltype = rv.get_ty();
        let exp = ctx.try_load2var(exp_range, rv.get_value(), builder)?;
        return Ok(match (&*pltype.borrow(), self.op.0) {
            (
                PLType::Primitive(
                    PriType::I128 | PriType::I64 | PriType::I32 | PriType::I16 | PriType::I8,
                ),
                TokenType::MINUS,
            ) => {
                //     (
                //     Some(plv!(builder.build_int_neg(exp, "negtmp"))),
                //     Some(pltype.clone()),
                //     TerminatorEnum::None,
                // )
                builder
                    .build_int_neg(exp, "negtmp")
                    .new_output(pltype.clone())
            }
            (PLType::Primitive(PriType::F64 | PriType::F32), TokenType::MINUS) => builder
                .build_float_neg(exp, "negtmp")
                .new_output(pltype.clone()),
            (PLType::Primitive(PriType::BOOL), TokenType::NOT) => builder
                .build_int_compare(
                    IntPredicate::EQ,
                    exp,
                    builder.int_value(&PriType::BOOL, false as u64, true),
                    "nottmp",
                )
                .new_output(pltype.clone()),
            (_exp, _op) => {
                return Err(ctx.add_diag(self.range.new_err(ErrorCode::INVALID_UNARY_EXPRESSION)));
            }
        });
    }
}

#[node]
pub struct BinOpNode {
    pub left: Box<NodeEnum>,
    pub op: (TokenType, Range),
    pub right: Box<NodeEnum>,
}

impl PrintTrait for BinOpNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("BinOpNode");
        self.left.print(tabs + 1, false, line.clone());
        tab(tabs + 1, line.clone(), false);
        println!("{:?}", self.op);
        self.right.print(tabs + 1, true, line.clone());
    }
}

impl Node for BinOpNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        let (lrange, rrange) = (self.left.range(), self.right.range());
        let lv = self.left.emit(ctx, builder)?.get_value();
        if lv.is_none() {
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::EXPECT_VALUE)));
        }
        let lv = lv.unwrap();
        let lpltype = lv.get_ty();
        let lv = lv.get_value();
        let left = ctx.try_load2var(lrange, lv, builder)?;
        if self.op.0 == TokenType::AND || self.op.0 == TokenType::OR {
            return Ok(match *lpltype.clone().borrow() {
                PLType::Primitive(PriType::BOOL) => {
                    // and: left && right
                    // or : left || right
                    // +---------------------------------------------------+
                    // |                            phi incoming 1         |
                    // |                               /                   |
                    // |  left(cur)---------------------------> merge      |  = left
                    // |     \                                   /         |
                    // |      \------------long[--right--]----->/          |  = right
                    // |                         \                         |
                    // |                      phi incoming 2               |
                    // +---------------------------------------------------+
                    let incoming_bb1 = builder.get_cur_basic_block(); // get incoming block 1
                    let long_bb = builder.append_basic_block(ctx.function.unwrap(), "long");
                    let merge_bb = builder.append_basic_block(ctx.function.unwrap(), "merge");
                    if self.op.0 == TokenType::AND {
                        // AND : goto long_bb if left is true
                        builder.build_conditional_branch(left, long_bb, merge_bb);
                    } else {
                        // OR  : goto long_bb if left is false
                        builder.build_conditional_branch(left, merge_bb, long_bb);
                    }
                    // long bb (emit right & goto merge)
                    builder.position_at_end_block(long_bb);
                    let rv = ctx
                        .emit_with_expectation(&mut self.right, lpltype, lrange, builder)?
                        .get_value();
                    if rv.is_none() {
                        return Err(ctx.add_diag(self.range.new_err(ErrorCode::EXPECT_VALUE)));
                    }
                    let right = ctx.try_load2var(rrange, rv.unwrap().get_value(), builder)?;
                    let incoming_bb2 = builder.get_cur_basic_block(); // get incoming block 2
                    builder.build_unconditional_branch(merge_bb);
                    // merge bb
                    builder.position_at_end_block(merge_bb);

                    builder.build_phi(
                        &PLType::Primitive(PriType::BOOL),
                        ctx,
                        &[(left, incoming_bb1), (right, incoming_bb2)],
                    )
                }
                .new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::BOOL)))),
                _ => {
                    return Err(ctx
                        .add_diag(self.range.new_err(ErrorCode::LOGIC_OP_NOT_BOOL))
                        .add_label(
                            self.left.range(),
                            ctx.get_file(),
                            format_label!("expect bool here"),
                        )
                        .clone())
                }
            });
        }
        let re = ctx
            .emit_with_expectation(&mut self.right, lpltype.clone(), lrange, builder)?
            .get_value();
        if re.is_none() {
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::EXPECT_VALUE)));
        }
        let right = ctx.try_load2var(rrange, re.unwrap().get_value(), builder)?;
        Ok(match self.op.0 {
            TokenType::PLUS => {
                handle_calc!(ctx, add, float_add, lpltype, left, right, self.range, builder)
            }
            TokenType::MINUS => {
                handle_calc!(ctx, sub, float_sub, lpltype, left, right, self.range, builder)
            }
            TokenType::MUL => {
                handle_calc!(ctx, mul, float_mul, lpltype, left, right, self.range, builder)
            }
            TokenType::DIV => {
                // TODO: 无符号触发
                handle_calc!(ctx, signed_div, float_div, lpltype, left, right, self.range, builder)
            }
            TokenType::EQ
            | TokenType::NE
            | TokenType::LEQ
            | TokenType::GEQ
            | TokenType::GREATER
            | TokenType::LESS => match *lpltype.borrow() {
                PLType::Primitive(
                    PriType::I128
                    | PriType::I64
                    | PriType::I32
                    | PriType::I16
                    | PriType::I8
                    | PriType::U128
                    | PriType::U64
                    | PriType::U32
                    | PriType::U16
                    | PriType::U8,
                ) => { builder.build_int_compare(self.op.0.get_op(), left, right, "cmptmp") }
                    .new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::BOOL)))),
                PLType::Primitive(PriType::F64 | PriType::F32) => {
                    { builder.build_float_compare(self.op.0.get_fop(), left, right, "cmptmp") }
                        .new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::BOOL))))
                }
                _ => return Err(ctx.add_diag(self.range.new_err(ErrorCode::VALUE_NOT_COMPARABLE))),
            },
            _ => {
                return Err(ctx.add_diag(
                    self.range
                        .new_err(ErrorCode::UNRECOGNIZED_BIN_OPERATOR)
                        .add_label(self.op.1, ctx.get_file(), None)
                        .clone(),
                ))
            }
        })
    }
}

#[node(comment)]
pub struct TakeOpNode {
    pub head: Box<NodeEnum>,
    pub field: Option<Box<VarNode>>,
}

impl PrintTrait for TakeOpNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("TakeOpNode");
        self.head
            .print(tabs + 1, self.field.is_none(), line.clone());
        if let Some(id) = &self.field {
            id.print(tabs + 1, true, line.clone());
        }
    }
}

impl Node for TakeOpNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        let no = self.head.emit(ctx, builder)?;
        let nv = no.get_value();
        if nv.is_none() {
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::INVALID_GET_FIELD)));
        }
        let nv = nv.unwrap();
        let head_pltype = get_type_deep(nv.get_ty());
        if !matches!(
            &*head_pltype.borrow(),
            PLType::Struct(_) | PLType::Pointer(_) | PLType::Trait(_) | PLType::Union(_)
        ) {
            return Err(ctx.add_diag(
                self.head
                    .range()
                    .new_err(ErrorCode::ILLEGAL_GET_FIELD_OPERATION),
            ));
        }
        ctx.if_completion(self.range, || {
            match &*ctx.auto_deref_tp(head_pltype.clone()).borrow() {
                PLType::Struct(s) => s.get_completions(ctx),
                PLType::Trait(s) => s.get_trait_completions(ctx),
                _ => vec![],
            }
        });
        if self.field.is_none() {
            // end with ".", gen completions
            return Err(ctx.add_diag(self.range.new_err(crate::ast::diag::ErrorCode::COMPLETION)));
        }
        let id = self.field.as_ref().unwrap();
        let id_range = id.range();
        let (head_pltype, headptr) = ctx.auto_deref(head_pltype, nv.get_value(), builder);
        match &*head_pltype.clone().borrow() {
            PLType::Trait(s) => {
                let field = s.fields.get(&id.name);
                if let Some(field) = field {
                    _ = s.expect_field_pub(ctx, field, id_range);
                    ctx.push_semantic_token(id_range, SemanticTokenType::METHOD, 0);
                    ctx.set_field_refs(head_pltype, field, id_range);
                    ctx.send_if_go_to_def(id_range, field.range, s.path.clone());
                    let re = field.typenode.get_type(ctx, builder)?;
                    let fnv = builder
                        .build_struct_gep(headptr, field.index, "mthd_ptr")
                        .unwrap();
                    let fnv = builder.build_load(fnv, "mthd_ptr_load");
                    let headptr = builder.build_struct_gep(headptr, 1, "traitptr").unwrap();
                    let headptr = builder.build_load(headptr, "traitptr_load");
                    ctx.emit_comment_highlight(&self.comments[0]);
                    return Ok(NodeOutput::new_value(NodeValue::new_receiver(
                        fnv, re, headptr, None,
                    )));
                }
                Err(ctx.add_diag(id.range.new_err(ErrorCode::STRUCT_FIELD_NOT_FOUND)))
            }
            PLType::Struct(s) => {
                if let Some(field) = s.fields.get(&id.name) {
                    _ = s.expect_field_pub(ctx, field, id_range);
                    ctx.push_semantic_token(id_range, SemanticTokenType::PROPERTY, 0);
                    ctx.set_field_refs(head_pltype, field, id_range);
                    ctx.send_if_go_to_def(id_range, field.range, s.path.clone());
                    return Ok(NodeOutput::new_value(NodeValue::new(
                        builder
                            .build_struct_gep(headptr, field.index, "structgep")
                            .unwrap(),
                        field.typenode.get_type(ctx, builder)?,
                    )));
                }
                if let Some(mthd) = s.find_method(ctx, &id.name) {
                    _ = mthd.expect_pub(ctx, id_range);
                    ctx.push_semantic_token(id_range, SemanticTokenType::METHOD, 0);
                    ctx.send_if_go_to_def(
                        id_range,
                        mthd.range,
                        mthd.llvmname.split("..").next().unwrap().to_string(),
                    );
                    return usize::MAX
                        .new_output(Arc::new(RefCell::new(PLType::Fn(mthd))))
                        .with_receiver(
                            headptr,
                            Some(Arc::new(RefCell::new(PLType::Pointer(head_pltype)))),
                        )
                        .to_result();
                };
                Err(ctx.add_diag(id.range.new_err(ErrorCode::STRUCT_FIELD_NOT_FOUND)))
            }
            PLType::Union(union) => {
                if let Some(mthd) = union.find_method(ctx, &id.name) {
                    _ = mthd.expect_pub(ctx, id_range);
                    ctx.push_semantic_token(id_range, SemanticTokenType::METHOD, 0);
                    ctx.send_if_go_to_def(
                        id_range,
                        mthd.range,
                        mthd.llvmname.split("..").next().unwrap().to_string(),
                    );
                    return usize::MAX
                        .new_output(Arc::new(RefCell::new(PLType::Fn(mthd))))
                        .with_receiver(
                            headptr,
                            Some(Arc::new(RefCell::new(PLType::Pointer(head_pltype)))),
                        )
                        .to_result();
                };
                Err(ctx.add_diag(id.range.new_err(ErrorCode::METHOD_NOT_FOUND)))
            }
            _ => Err(ctx.add_diag(
                self.head
                    .range()
                    .new_err(ErrorCode::ILLEGAL_GET_FIELD_OPERATION),
            )),
        }
    }
}
