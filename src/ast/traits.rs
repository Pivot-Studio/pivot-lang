use super::pltype::{FNValue, STType, UnionType};

pub trait CustomType {
    fn get_path(&self) -> String;
    fn is_tuple(&self) -> bool {
        false
    }
}

macro_rules! impl_custom_type {
    ($t:ty) => {
        impl CustomType for $t {
            fn get_path(&self) -> String {
                self.path.clone()
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

impl_custom_type!(UnionType, FNValue);


impl CustomType for STType {
    fn is_tuple(&self) -> bool {
        self.is_tuple
    }

    fn get_path(&self) -> String {
        self.path.clone()
    }
}
