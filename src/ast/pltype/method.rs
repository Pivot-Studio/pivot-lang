use std::{cell::RefCell, sync::Arc};

use rustc_hash::FxHashMap;

use crate::{
    ast::{
        diag::{ErrorCode, PLDiag},
        node::RangeTrait,
        traits::CustomType,
    },
    format_label,
};

use super::FNValue;

pub trait ImplAble: RangeTrait + CustomType {
    fn get_method_table(&self) -> Arc<RefCell<FxHashMap<String, Arc<RefCell<FNValue>>>>>;
    fn get_method(&self, name: &str) -> Option<Arc<RefCell<FNValue>>> {
        let binding = self.get_method_table();
        let table = binding.borrow();
        table.get(name).cloned()
    }
    fn add_method(&self, name: &str, value: Arc<RefCell<FNValue>>) -> Result<(), PLDiag> {
        let range = self.range();
        let binding = self.get_method_table();
        let mut table = binding.borrow_mut();
        if let Some(v) = table.get(name) {
            return Err(range
                .new_err(ErrorCode::DUPLICATE_METHOD)
                .add_label(
                    v.borrow().range,
                    v.borrow().path.clone(),
                    format_label!("Previously defined here"),
                )
                .clone());
        }
        table.insert(name.to_owned(), value);
        Ok(())
    }
    fn get_full_name_except_generic(&self) -> String;
    fn get_full_name(&self) -> String;
}
