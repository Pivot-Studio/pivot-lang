use std::{cell::RefCell, sync::Arc};

use ustr::Ustr;

use crate::ast::{plmod::Mod, pltype::PLType};

pub trait PivotDB {
    fn set_ref_str(&self, ref_str: Option<Ustr>);
    fn get_ref_str(&self) -> Option<Ustr>;
    fn add_module(&self, name: Ustr, plmod: Mod);
    fn get_module(&self, name: Ustr) -> Option<Mod>;
    fn add_tp_to_mod(&self, name: Ustr, tpname: Ustr, pltype: Arc<RefCell<PLType>>);
}

#[salsa::db]
pub trait Db: salsa::Database + PivotDB {}
