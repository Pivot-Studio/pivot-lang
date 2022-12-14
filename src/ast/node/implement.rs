use super::*;
use crate::ast::ctx::Ctx;
use internal_macro::{comments, fmt, range};
use lsp_types::{DocumentSymbol, SymbolKind};

#[range]
#[fmt]
#[comments]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ImplNode {
    pub target: Box<TypeNodeEnum>,
    pub methods: Vec<Box<FuncDefNode>>,
}

impl Node for ImplNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ImplNode");
        self.target.print(tabs + 1, false, line.clone());
        for method in &self.methods {
            method.print(tabs + 1, false, line.clone());
        }
    }
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        _ = self.target.emit_highlight(ctx);
        let mut method_docsymbols = vec![];
        let tp = self.target.get_type(ctx, builder)?;
        match &*tp.borrow() {
            PLType::STRUCT(sttp) => {
                ctx.send_if_go_to_def(self.target.range(), sttp.range, sttp.path.clone());
            }
            _ => {
                ctx.add_diag(self.target.range().new_err(ErrorCode::EXPECT_TYPE));
            }
        };

        for method in &mut self.methods {
            let res = method.emit(ctx, builder);
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
            name: format!("impl {}", FmtBuilder::generate_node(&self.target)),
            detail: None,
            kind: SymbolKind::OBJECT,
            tags: None,
            deprecated: None,
            range: self.range.to_diag_range(),
            selection_range: self.range.to_diag_range(),
            children: Some(method_docsymbols),
        };
        ctx.plmod.doc_symbols.borrow_mut().push(docsymbol);
        Ok((None, None, TerminatorEnum::NONE))
    }
}
