use super::*;
use crate::{ast::ctx::Ctx, utils::read_config::enter};
use internal_macro::{range, comments};
use lsp_types::{DocumentSymbol, SymbolKind};

#[range]
#[comments]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ImplNode {
    pub target: Box<TypeNodeEnum>,
    pub methods: Vec<Box<FuncDefNode>>,
}

impl Node for ImplNode {
    fn format(&self, _tabs: usize, _prefix: &str) -> String {
        let mut format_res = String::from(enter());
        format_res.push_str("impl ");
        format_res.push_str(&self.target.format(0, ""));
        format_res.push_str(" {");
        for method in &self.methods {
            format_res.push_str(&method.format(1, "    "));
        }
        format_res.push_str("}");
        format_res.push_str(enter());
        format_res
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ImplNode");
        self.target.print(tabs + 1, false, line.clone());
        for method in &self.methods {
            method.print(tabs + 1, false, line.clone());
        }
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        _ = self.target.emit_highlight(ctx);
        let mut method_docsymbols = vec![];
        for method in &mut self.methods {
            let res = method.emit(ctx);
            if res.is_err() {
                continue;
            }
            let (_, pltype, _) = res.unwrap();
            if pltype.is_none() {
                continue;
            }
            let tmp = pltype.unwrap();
            let f = if let PLType::FN(f) = &*tmp.borrow() {
                f.get_doc_symbol()
            } else {
                continue;
            };
            method_docsymbols.push(f);
        }
        ctx.emit_comment_highlight(&self.comments[0]);
        #[allow(deprecated)]
        let docsymbol = DocumentSymbol {
            name: format!("impl {}", self.target.format(0, "")),
            detail: None,
            kind: SymbolKind::OBJECT,
            tags: None,
            deprecated: None,
            range: self.range.to_diag_range(),
            selection_range: self.range.to_diag_range(),
            children: Some(method_docsymbols),
        };
        ctx.doc_symbols.borrow_mut().push(docsymbol);
        Ok((None, None, TerminatorEnum::NONE))
    }
}
