use std::sync::{Arc, Mutex};

use salsa::DebugWithDb;

// ANCHOR: db_struct
#[derive(Default)]
#[salsa::db(crate::Jar)]
pub struct Database {
    storage: salsa::Storage<Self>,

    // The logs are only used for testing and demonstrating reuse:
    //
    logs: Option<Arc<Mutex<Vec<String>>>>,
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
        })
    }
}
// ANCHOR_END: par_db_impl
