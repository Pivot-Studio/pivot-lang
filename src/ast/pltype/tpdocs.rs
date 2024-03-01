use crate::ast::node::NodeEnum;

use super::{FNValue, PLType, STType};

impl PLType {
    pub fn get_docs(&self) -> Option<String> {
        match self {
            PLType::Fn(f) => f.get_docs_string(),
            PLType::Struct(s) | PLType::Trait(s) => s.get_docs_string(),
            PLType::Unknown => Some("A type cannot be inferred".to_string()),
            _ => Some(self.get_kind_name()),
        }
    }
}

pub trait DocAnnotatable {
    fn get_docs(&self) -> Option<&Vec<Box<NodeEnum>>>;
    fn get_docs_string(&self) -> Option<String> {
        if let Some(docs) = self.get_docs() {
            let mut string = String::new();
            for doc in docs {
                if let NodeEnum::Comment(c) = &**doc {
                    string.push_str(&c.comment);
                    string.push('\n');
                }
            }
            Some(string)
        } else {
            None
        }
    }
}

impl DocAnnotatable for FNValue {
    fn get_docs(&self) -> Option<&Vec<Box<NodeEnum>>> {
        Option::Some(self.doc.as_ref())
    }
}

impl DocAnnotatable for STType {
    fn get_docs(&self) -> Option<&Vec<Box<NodeEnum>>> {
        Option::Some(self.doc.as_ref())
    }
}
