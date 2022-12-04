use crate::utils::read_config::enter;

use super::node::{program::ProgramNode, FmtNode, Node, NodeEnum};

pub struct FmtBuilder {
    buf: String,
}

impl FmtBuilder {
    pub fn new() -> Self {
        FmtBuilder { buf: String::new() }
    }
    pub fn format(&self) -> String {
        self.buf.clone()
    }
    pub fn enter(&mut self) {
        self.buf.push_str(enter());
    }
    pub fn space(&mut self) {
        self.buf.push(' ');
    }
    pub fn colon(&mut self) {
        self.buf.push(':');
    }
    pub fn dbcolon(&mut self) {
        self.buf.push_str("::");
    }
    pub fn semicolon(&mut self) {
        self.buf.push(';');
    }
    pub fn comma(&mut self) {
        self.buf.push(',');
    }
    pub fn token(&mut self, token: &str) {
        self.buf.push_str(token);
    }
    pub fn parseProgramNode(&mut self, node: &ProgramNode) {
        for statement in &node.nodes {
            self.buf.push_str(&statement.format(0, "    "));
        }
    }
    pub fn node(&mut self, node: &dyn FmtNode) {
        // self.buf.push_str(&node.formatBuild(0, ""))
    }
}
