use std::fs::read_to_string;

use rustc_hash::FxHashMap;

pub struct MemDocs {
    docs: FxHashMap<String, String>,
}

impl MemDocs {
    pub fn new() -> Self {
        MemDocs {
            docs: FxHashMap::default(),
        }
    }
    pub fn insert(&mut self, key: String, value: String) {
        self.docs.insert(key, value);
    }
    pub fn get(&self, key: &str) -> Option<&String> {
        self.docs.get(key)
    }
    pub fn get_file_content(&self, key: &str) -> Option<String> {
        let mem = self.get(key);
        if let Some(mem) = mem {
            return Some(mem.clone());
        }
        let re = read_to_string(key);
        if let Ok(re) = re {
            return Some(re);
        }
        None
    }
    pub fn get_mut(&mut self, key: &str) -> Option<&mut String> {
        self.docs.get_mut(key)
    }
    pub fn remove(&mut self, key: &str) -> Option<String> {
        self.docs.remove(key)
    }
    pub fn iter(&self) -> impl Iterator<Item = &String> {
        self.docs.values()
    }
}
