use ustr::Ustr;

use super::{
    node::types::CustomTypeNode,
    pltype::{FNValue, PlaceHolderType, STType, UnionType},
};

pub trait CustomType {
    fn get_path(&self) -> Ustr;
    fn get_name(&self) -> Ustr;
    fn get_range(&self) -> Range;
}

macro_rules! impl_custom_type {
    ($t:ty) => {

        impl CustomType for $t {
            fn get_path(&self) -> Ustr {
                self.path.clone()
            }
            fn get_name(&self) -> Ustr {
                self.name.clone()
            }
            fn get_range(&self) -> Range {
                self.range
            }
        }
    };
    () => {

    };
    ($($t:ty),*) => {
        $(impl_custom_type!($t);)*
    };
    ($($t:ty,)*) => {
        $(impl_custom_type!($t);)*
    };
}
use crate::ast::range::Range;
impl_custom_type!(UnionType, STType, FNValue, PlaceHolderType, CustomTypeNode);
