use super::function::FuncDefNode;
use super::types::StructDefNode;
use super::*;
use crate::ast::ctx::Ctx;
use crate::utils::tabs;
use string_builder::Builder;

use internal_macro::range;

#[range]
pub struct ProgramNode {
    pub fns: Vec<FuncDefNode>,
    pub structs: Vec<StructDefNode>,
}
impl Node for ProgramNode {
    fn string(&self, tabs: usize) -> String {
        let mut builder = Builder::default();
        tabs::print_tabs(&mut builder, tabs);
        builder.append("(ProgramNode");
        for statement in &self.fns {
            builder.append(statement.string(tabs + 1));
        }
        for statement in &self.structs {
            builder.append(statement.string(tabs + 1));
        }
        tabs::print_tabs(&mut builder, tabs);
        builder.append(")");
        builder.string().unwrap()
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        loop {
            let mut i = 0;
            self.structs.iter().for_each(|x| {
                let re = x.get_type(ctx);
                match re {
                    Value::None => i = i + 1,
                    _ => {}
                }
            });
            if i == 0 {
                break;
            }
        }
        // self.fns.iter().for_each(|x| {
        //     x.get_type(ctx);
        // });
        self.fns.iter_mut().for_each(|x| {
            x.emit(ctx);
        });

        Value::None
    }
}

// impl ProgramNode {
//     fn type_reg<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {

//         reg_structs(&self.structs, ctx);

//         for statement in self.fns {
//             statement.get_type(ctx);
//         }

//         Value::None
//     }

// }

// fn reg_structs<'a, 'ctx>(sts: Vec<StructDefNode>, ctx: &mut Ctx<'a, 'ctx>) {
//     if sts.len() == 0 {
//         return;
//     }
//     let mut failedsts = vec![];
//     let mut i = 0;
//     for st in sts {
//         let re = st.get_type(ctx);
//         match re {
//             Value::None => failedsts.push(st),
//             _ => todo!(),
//         }
//         i = i + 1;
//     }
//     reg_structs(failedsts, ctx)

// }
