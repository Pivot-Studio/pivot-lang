use std::{
    cell::{Cell, RefCell},
    sync::{Arc, Mutex},
};

use rustc_hash::FxHashMap;
// use salsa::DebugWithDb;
use ustr::Ustr;

use crate::{
    ast::{plmod::Mod, pltype::PLType},
    Db, PivotDB,
};

// ANCHOR: db_struct
#[derive(Default)]
#[salsa::db]
pub struct Database {
    storage: salsa::Storage<Self>,

    // The logs are only used for testing and demonstrating reuse:
    //
    logs: Option<Arc<Mutex<Vec<String>>>>,
    ref_str: Arc<Mutex<Cell<Option<Ustr>>>>,
    module_map: Arc<Mutex<FxHashMap<Ustr, Mod>>>,
}
// ANCHOR_END: db_struct

// ANCHOR: db_impl
#[salsa::db]
impl salsa::Database for Database {
    fn salsa_event(&self, event: &dyn Fn() -> salsa::Event) {
        // Log interesting events, if logging is enabled
        if let Some(logs) = &self.logs {
            let event = event();
            // don't log boring events
            if let salsa::EventKind::WillExecute { .. } = event.kind {
                logs.lock().unwrap().push(format!("Event: {:?}", event));
            }
        }
    }
}

impl Clone for Database {
    fn clone(&self) -> Self {
        Self {
            storage: self.storage.clone(),
            logs: self.logs.clone(),
            ref_str: self.ref_str.clone(),
            module_map: self.module_map.clone(),
        }
    }
}
// // ANCHOR_END: db_impl

// // ANCHOR: par_db_impl
// impl salsa::ParallelDatabase for Database {
//     fn snapshot(&self) -> salsa::Snapshot<Self> {
//         salsa::Snapshot::new(Database {
//             storage: self.storage.snapshot(),
//             logs: self.logs.clone(),
//             ref_str: self.ref_str.clone(),
//             module_map: self.module_map.clone(),
//         })
//     }
// }

// ANCHOR_END: par_db_impl

impl PivotDB for Database {
    fn set_ref_str(&self, ref_str: Option<Ustr>) {
        self.ref_str.lock().unwrap().set(ref_str);
    }
    fn get_ref_str(&self) -> Option<Ustr> {
        *self.ref_str.lock().unwrap().get_mut()
    }

    fn add_module(&self, name: Ustr, plmod: Mod) {
        self.module_map.lock().unwrap().insert(name, plmod);
    }

    fn get_module(&self, name: Ustr) -> Option<Mod> {
        self.module_map.lock().unwrap().get(&name).cloned()
    }

    fn add_tp_to_mod(&self, name: Ustr, tpname: Ustr, pltype: Arc<RefCell<PLType>>) {
        self.module_map
            .lock()
            .unwrap()
            .get_mut(&name)
            .and_then(|m| m.types.insert(tpname, pltype.into()));
    }
}
#[salsa::db]
impl Db for Database {}

impl Database {
    /// Enable logging of each salsa event.
    #[cfg(test)]
    pub fn enable_logging(self) -> Self {
        assert!(self.logs.is_none());
        Self {
            storage: self.storage,
            logs: Some(Default::default()),
            ref_str: self.ref_str,
            module_map: self.module_map,
        }
    }

    #[cfg(test)]
    pub fn take_logs(&self) -> Vec<String> {
        if let Some(logs) = &self.logs {
            std::mem::take(&mut *logs.lock().unwrap())
        } else {
            panic!("logs not enabled");
        }
    }
}
