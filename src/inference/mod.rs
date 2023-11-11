use ena::unify::{UnifyKey, UnifyValue};

use crate::ast::pltype::PLType;



#[test]
fn test() {
    let mut table = ena::unify::UnificationTable::<ena::cc::Token>::new();

    // table.unify_var_var(a_id, b_id)
    // table.new_key(value)

}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct TyVariable {
    id: u32,
}


impl UnifyKey for TyVariable {
    type Value = TyWrapper;

    fn index(&self) -> u32 {
        self.id
    }

    fn from_index(u: u32) -> Self {
        Self { id: u }
    }

    fn tag() -> &'static str {
        "TyVariable"
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyWrapper(pub PLType);

impl UnifyValue for TyWrapper {
    fn unify_values(value1: &Self, value2: &Self) -> Result<Self, (Self, Self)> {
        if value1 == value2 {
            Ok(value1.clone())
        }else {
            Err((value1.clone(), value2.clone()))
        }
    }
}


