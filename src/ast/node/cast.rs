use internal_macro::node;

use crate::{
    ast::{
        builder::{BuilderEnum, IRBuilder, ValueHandle},
        ctx::Ctx,
        node::TypeNode,
        pltype::PLType,
    },
    plv,
};

use super::{Node, NodeEnum, NodeResult, PLValue, PrintTrait, TerminatorEnum, TypeNodeEnum};
use crate::ast::node::RangeTrait;

#[node]
pub struct AsNode {
    pub expr: Box<NodeEnum>,
    pub ty: Box<TypeNodeEnum>,
}

impl Node for AsNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        let re = self.expr.emit(ctx, builder);
        self.ty.emit_highlight(ctx);
        let (val, tp, _) = re?;
        let val = ctx.try_load2var(self.expr.range(), val.unwrap(), builder)?;
        let target_tp = self.ty.get_type(ctx, builder)?;
        let val = ctx.force_cast_safe(val, &tp.unwrap().borrow(), &target_tp.borrow(), builder);
        Ok((Some(plv!(val)), Some(target_tp), TerminatorEnum::None))
    }
}

impl PrintTrait for AsNode {
    fn print(&self, tabs: usize, end: bool, line: Vec<bool>) {
        todo!()
    }
}

impl<'a, 'ctx> Ctx<'a> {
    /// # force_cast_safe
    ///
    /// Doing a safe cast.
    ///
    /// All primary types will be casted directly to the target type.
    ///
    /// All union types will be casted to the `Option` of the target type if the target type is
    /// the member of the union type
    pub fn force_cast_safe<'b>(
        &mut self,
        val: ValueHandle,
        ty: &PLType,
        target_ty: &PLType,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> ValueHandle {
        match (ty, target_ty) {
            (PLType::Primitive(ty), PLType::Primitive(target_ty)) => {
                builder.cast_primitives(val, ty, target_ty)
            }
            _ => todo!(),
        }
    }
}
