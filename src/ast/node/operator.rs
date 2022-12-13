use super::primary::VarNode;
use super::*;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::ast::ctx::Ctx;
use crate::ast::diag::ErrorCode;
use crate::ast::pltype::PLType;
use crate::ast::pltype::PriType;
use crate::ast::tokens::TokenType;
use crate::handle_calc;
use crate::plv;
use inkwell::IntPredicate;
use internal_macro::comments;

use internal_macro::fmt;
use internal_macro::range;
use lsp_types::SemanticTokenType;
use paste::item;
#[range]
#[fmt]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct UnaryOpNode {
    pub op: TokenType,
    pub exp: Box<NodeEnum>,
}
// 单目运算符
impl Node for UnaryOpNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("UnaryOpNode");
        tab(tabs + 1, line.clone(), end);
        println!("{:?}", self.op);
        self.exp.print(tabs + 1, true, line.clone());
    }
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        let exp_range = self.exp.range();
        let (exp, pltype, _) = self.exp.emit(ctx, builder)?;
        if pltype.is_none() {
            return Err(ctx.add_err(self.range, ErrorCode::INVALID_UNARY_EXPRESSION));
        }
        let pltype = pltype.unwrap();
        let (exp, _) = ctx.try_load2var(exp_range, exp.unwrap(), pltype.clone(), builder)?;
        return Ok(match (&*pltype.borrow(), self.op) {
            (
                PLType::PRIMITIVE(
                    PriType::I128 | PriType::I64 | PriType::I32 | PriType::I16 | PriType::I8,
                ),
                TokenType::MINUS,
            ) => (
                Some(plv!(builder.build_int_neg(exp, "negtmp"))),
                Some(pltype.clone()),
                TerminatorEnum::NONE,
            ),
            (PLType::PRIMITIVE(PriType::F64 | PriType::F32), TokenType::MINUS) => (
                Some(plv!(builder.build_float_neg(exp, "negtmp"))),
                Some(pltype.clone()),
                TerminatorEnum::NONE,
            ),
            (PLType::PRIMITIVE(PriType::BOOL), TokenType::NOT) => (
                {
                    let bool_origin = builder.build_int_compare(
                        IntPredicate::EQ,
                        exp,
                        builder.int_value(&PriType::BOOL, false as u64, true),
                        "nottmp",
                    );
                    // Some(plv!(builder.build_int_z_extend(
                    //     bool_origin,
                    //     &PriType::BOOL,
                    //     "zexttemp"
                    // )))
                    Some(plv!(bool_origin))
                },
                Some(pltype.clone()),
                TerminatorEnum::NONE,
            ),
            (_exp, _op) => {
                return Err(ctx.add_err(self.range, ErrorCode::INVALID_UNARY_EXPRESSION));
            }
        });
    }
}

#[range]
#[fmt]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BinOpNode {
    pub left: Box<NodeEnum>,
    pub op: TokenType,
    pub right: Box<NodeEnum>,
}
impl Node for BinOpNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("BinOpNode");
        self.left.print(tabs + 1, false, line.clone());
        tab(tabs + 1, line.clone(), false);
        println!("{:?}", self.op);
        self.right.print(tabs + 1, true, line.clone());
    }
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        let (lrange, rrange) = (self.left.range(), self.right.range());
        let (lv, lpltype, _) = self.left.emit(ctx, builder)?;
        let (rv, rpltype, _) =
            ctx.emit_with_expectation(&mut self.right, lpltype.clone(), builder)?;
        if lv.is_none() || rv.is_none() {
            return Err(ctx.add_err(self.range, ErrorCode::EXPECT_VALUE));
        }
        let (left, _ltp) =
            ctx.try_load2var(lrange, lv.unwrap(), lpltype.clone().unwrap(), builder)?;
        let (right, _rtp) = ctx.try_load2var(rrange, rv.unwrap(), rpltype.unwrap(), builder)?;
        Ok(match self.op {
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
            | TokenType::LESS => match *lpltype.unwrap().borrow() {
                PLType::PRIMITIVE(
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
                ) => (
                    {
                        let bool_origin =
                            builder.build_int_compare(self.op.get_op(), left, right, "cmptmp");
                        // Some(plv!(builder.build_int_z_extend(
                        //     bool_origin,
                        //     &PriType::BOOL,
                        //     "zexttemp"
                        // )))
                        Some(plv!(bool_origin))
                    },
                    Some(Rc::new(RefCell::new(PLType::PRIMITIVE(PriType::BOOL)))),
                    TerminatorEnum::NONE,
                ),
                PLType::PRIMITIVE(PriType::F64 | PriType::F32) => (
                    {
                        let bool_origin =
                            builder.build_float_compare(self.op.get_fop(), left, right, "cmptmp");
                        // Some(plv!(builder.build_int_z_extend(
                        //     bool_origin,
                        //     &PriType::BOOL,
                        //     "zexttemp"
                        // )))
                        Some(plv!(bool_origin))
                    },
                    Some(Rc::new(RefCell::new(PLType::PRIMITIVE(PriType::BOOL)))),
                    TerminatorEnum::NONE,
                ),
                _ => return Err(ctx.add_err(self.range, ErrorCode::VALUE_NOT_COMPARABLE)),
            },
            TokenType::AND => match *lpltype.unwrap().borrow() {
                PLType::PRIMITIVE(PriType::BOOL) => (
                    {
                        let bool_origin = builder.build_and(left, right, "andtmp");
                        // Some(plv!(builder.build_int_z_extend(
                        //     bool_origin,
                        //     &PriType::BOOL,
                        //     "zext_temp"
                        // )))
                        Some(plv!(bool_origin))
                    },
                    Some(Rc::new(RefCell::new(PLType::PRIMITIVE(PriType::BOOL)))),
                    TerminatorEnum::NONE,
                ),
                _ => return Err(ctx.add_err(self.range, ErrorCode::LOGIC_OP_NOT_BOOL)),
            },
            TokenType::OR => match *lpltype.unwrap().borrow() {
                PLType::PRIMITIVE(PriType::BOOL) => (
                    {
                        let bool_origin = builder.build_or(left, right, "ortmp");
                        // Some(plv!(builder.build_int_z_extend(
                        //     bool_origin,
                        //     &PriType::BOOL,
                        //     "zext_temp"
                        // )))
                        Some(plv!(bool_origin))
                    },
                    Some(Rc::new(RefCell::new(PLType::PRIMITIVE(PriType::BOOL)))),
                    TerminatorEnum::NONE,
                ),
                _ => {
                    return Err(
                        ctx.add_err(self.range, crate::ast::diag::ErrorCode::LOGIC_OP_NOT_BOOL)
                    )
                }
            },
            _ => return Err(ctx.add_err(self.range, ErrorCode::UNRECOGNIZED_BIN_OPERATOR)),
        })
    }
}

#[range]
#[comments]
#[fmt]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TakeOpNode {
    pub head: Box<NodeEnum>,
    pub field: Option<Box<VarNode>>,
}

impl Node for TakeOpNode {
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
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        let head = self.head.emit(ctx, builder)?;
        let (mut res, pltype, _) = head;
        if pltype.is_none() {
            return Err(ctx.add_err(self.range, ErrorCode::INVALID_GET_FIELD));
        }
        let mut pltype = pltype.unwrap();
        if let Some(id) = &self.field {
            res = match &*pltype.clone().borrow() {
                PLType::STRUCT(_) | PLType::POINTER(_) => {
                    let (tp, s) = ctx.auto_deref(pltype, res.unwrap().value, builder);
                    let headptr = s;
                    pltype = tp;
                    let etype = pltype.clone();
                    let index;
                    if let PLType::STRUCT(_s) = &*etype.borrow() {
                        // end with ".", gen completions
                        ctx.if_completion(id.range, || {
                            if let PLType::STRUCT(s) = &*pltype.clone().borrow() {
                                return s.get_completions(ctx);
                            }
                            vec![]
                        });
                        let range = id.range();
                        if let PLType::STRUCT(s) = &*pltype.clone().borrow() {
                            let field = s.fields.get(&id.name);
                            let method = s.find_method(&ctx, &id.name);
                            if let Some(field) = field {
                                ctx.push_semantic_token(range, SemanticTokenType::PROPERTY, 0);
                                index = field.index;
                                ctx.set_if_refs(field.refs.clone(), range);
                                ctx.send_if_go_to_def(range, field.range, s.path.clone());
                                pltype = field.typenode.get_type(ctx, builder)?.clone();
                            } else if let Some(mthd) = method {
                                ctx.push_semantic_token(range, SemanticTokenType::METHOD, 0);
                                let mut mthd = mthd.clone();
                                mthd.param_pltypes
                                    .insert(0, pltype.borrow().get_typenode(ctx));
                                ctx.send_if_go_to_def(
                                    range,
                                    mthd.range,
                                    mthd.llvmname.split("..").nth(0).unwrap().to_string(),
                                );
                                return Ok((
                                    Some(PLValue {
                                        value: builder.get_or_insert_fn_handle(&mthd, ctx),
                                        is_const: false,
                                        receiver: Some(headptr),
                                    }),
                                    Some(Rc::new(RefCell::new(PLType::FN(mthd)))),
                                    TerminatorEnum::NONE,
                                ));
                            } else {
                                return Err(
                                    ctx.add_err(id.range, ErrorCode::STRUCT_FIELD_NOT_FOUND)
                                );
                            }
                        } else {
                            unreachable!()
                        }
                    } else {
                        return Err(ctx.add_err(id.range, ErrorCode::INVALID_GET_FIELD));
                    }
                    Some(plv!(builder
                        .build_struct_gep(s, index, "structgep")
                        .unwrap()))
                }
                _ => return Err(ctx.add_err(id.range, ErrorCode::ILLEGAL_GET_FIELD_OPERATION)),
            }
        }
        if self.field.is_none() {
            // end with ".", gen completions
            let tp = ctx.auto_deref_tp(pltype);
            ctx.if_completion(self.range, || {
                if let PLType::STRUCT(s) = &*tp.borrow() {
                    return s.get_completions(&ctx);
                }
                vec![]
            });
            return Err(ctx.add_err(self.range, crate::ast::diag::ErrorCode::COMPLETION));
        }
        ctx.emit_comment_highlight(&self.comments[0]);
        Ok((res, Some(pltype.clone()), TerminatorEnum::NONE))
    }
}
