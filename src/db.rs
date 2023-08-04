use std::{
    cell::{Cell, RefCell},
    sync::{Arc, Mutex},
};

use rustc_hash::FxHashMap;
use salsa::DebugWithDb;

use crate::{
    ast::{plmod::Mod, pltype::PLType},
    Db,
};

// ANCHOR: db_struct
#[derive(Default)]
#[salsa::db(crate::Jar)]
pub struct Database {
    storage: salsa::Storage<Self>,

    // The logs are only used for testing and demonstrating reuse:
    //
    logs: Option<Arc<Mutex<Vec<String>>>>,
    ref_str: Arc<Mutex<Cell<Option<String>>>>,
    module_map: Arc<Mutex<RefCell<FxHashMap<String, Mod>>>>,
}
// ANCHOR_END: db_struct

// ANCHOR: db_impl
impl salsa::Database for Database {
    fn salsa_event(&self, event: salsa::Event) {
        // Log interesting events, if logging is enabled
        if let Some(logs) = &self.logs {
            // don't log boring events
            if let salsa::EventKind::WillExecute { .. } = event.kind {
                logs.lock()
                    .unwrap()
                    .push(format!("Event: {:?}", event.debug(self)));
            }
        }
    }
}
// ANCHOR_END: db_impl

// ANCHOR: par_db_impl
impl salsa::ParallelDatabase for Database {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(Database {
            storage: self.storage.snapshot(),
            logs: self.logs.clone(),
            ref_str: self.ref_str.clone(),
            module_map: self.module_map.clone(),
        })
    }
}

// ANCHOR_END: par_db_impl

impl Db for Database {
    fn set_ref_str(&self, ref_str: Option<String>) {
        self.ref_str.lock().unwrap().set(ref_str);
    }
    fn get_ref_str(&self) -> Option<String> {
        self.ref_str.lock().unwrap().get_mut().clone()
    }

    fn add_module(&self, name: String, plmod: Mod) {
        self.module_map
            .lock()
            .unwrap()
            .borrow_mut()
            .insert(name, plmod);
    }

    fn get_module(&self, name: &str) -> Option<Mod> {
        self.module_map.lock().unwrap().borrow().get(name).cloned()
    }

    fn add_tp_to_mod(&self, name: &str, tpname: &str, pltype: Arc<RefCell<PLType>>) {
        self.module_map
            .lock()
            .unwrap()
            .borrow_mut()
            .get_mut(name)
            .and_then(|m| m.types.insert(tpname.to_string(), pltype.into()));
    }
}

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
