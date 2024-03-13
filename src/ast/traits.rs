use super::{
    node::types::CustomTypeNode,
    pltype::{ARRType, FNValue, PlaceHolderType, STType, TraitImplAble, UnionType},
};

pub trait CustomType {
    fn get_path(&self) -> String;
    fn get_name(&self) -> String;
    fn get_range(&self) -> Range;
}

macro_rules! impl_custom_type {
    ($t:ty) => {

        impl CustomType for $t {
            fn get_path(&self) -> String {
                self.path.clone()
            }
            fn get_name(&self) -> String {
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

impl CustomType for ARRType {
    fn get_path(&self) -> String {
        "".to_string()
    }
    fn get_name(&self) -> String {
        self.get_full_name()
    }
    fn get_range(&self) -> Range {
        Default::default()
    }
}
