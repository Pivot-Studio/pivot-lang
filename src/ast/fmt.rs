use crate::{ast::node::Num, utils::read_config::enter};

use super::{
    node::{
        comment::CommentNode,
        control::{BreakNode, ContinueNode, ForNode, IfNode, WhileNode},
        error::{ErrorNode, StErrorNode},
        function::{FuncCallNode, FuncDefNode},
        global::GlobalNode,
        implement::ImplNode,
        interface::{TraitBoundNode, TraitDefNode},
        macro_nodes::{MacroCallNode, MacroLoopStatementNode, MacroNode, MacroRuleNode},
        operator::{BinOpNode, TakeOpNode, UnaryOpNode},
        pkg::{ExternIdNode, UseNode},
        pointer::{PointerOpEnum, PointerOpNode},
        primary::{
            ArrayElementNode, BoolConstNode, NumNode, ParanthesesNode, PrimaryNode, VarNode,
        },
        print_params,
        program::ProgramNode,
        ret::RetNode,
        statement::{AssignNode, DefNode, EmptyNode, StatementsNode},
        string_literal::StringNode,
        types::{
            ArrayInitNode, ArrayTypeNameNode, GenericDefNode, GenericParamNode, PointerTypeNode,
            StructDefNode, StructInitFieldNode, StructInitNode, TypeNameNode, TypedIdentifierNode,
        },
        FmtTrait, NodeEnum, TypeNodeEnum,
    },
    tokens::TokenType,
};

#[derive(Clone)]
pub struct FmtBuilder {
    buf: String,
    tabs: usize,
    prefix: &'static str,
}

impl FmtBuilder {
    pub fn new() -> Self {
        FmtBuilder {
            buf: String::new(),
            tabs: 0,
            prefix: "    ",
        }
    }
    pub fn generate_node(node: &TypeNodeEnum) -> String {
        let mut b = FmtBuilder::new();
        node.format(&mut b);
        b.generate()
    }
    pub fn generate(&self) -> String {
        self.buf.clone()
    }
    pub fn add_tab(&mut self) {
        self.tabs += 1;
    }
    pub fn sub_tab(&mut self) {
        self.tabs -= 1;
    }
    pub fn prefix(&mut self) {
        self.buf.push_str(&self.prefix.repeat(self.tabs));
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
    pub fn dot(&mut self) {
        self.buf.push('.');
    }
    pub fn l_paren(&mut self) {
        self.buf.push('(');
    }
    pub fn r_paren(&mut self) {
        self.buf.push(')');
    }
    pub fn l_bracket(&mut self) {
        self.buf.push('[');
    }
    pub fn r_bracket(&mut self) {
        self.buf.push(']');
    }
    pub fn l_brace(&mut self) {
        self.buf.push('{');
    }
    pub fn r_brace(&mut self) {
        self.buf.push('}');
    }
    pub fn l_angle_bracket(&mut self) {
        self.buf.push('<');
    }
    pub fn r_angle_bracket(&mut self) {
        self.buf.push('>');
    }
    pub fn parallel(&mut self) {
        self.buf.push('|');
    }
    pub fn underline(&mut self) {
        self.buf.push('_');
    }
    pub fn and(&mut self) {
        self.buf.push('&');
    }
    pub fn asterisk(&mut self) {
        self.buf.push('*');
    }
    pub fn equal(&mut self) {
        self.buf.push('=');
    }
    pub fn token(&mut self, token: &str) {
        self.buf.push_str(token);
    }
    // parse nodes
    pub fn parse_program_node(&mut self, node: &ProgramNode) {
        for statement in &node.nodes {
            statement.format(self);
        }
    }
    pub fn parse_var_node(&mut self, node: &VarNode) {
        self.token(&node.name);
    }
    pub fn parse_use_node(&mut self, node: &UseNode) {
        self.token("use");
        self.space();
        for (i, id) in node.ids.iter().enumerate() {
            id.format(self);
            if i != node.ids.len() - 1 {
                self.dbcolon();
            }
        }
        self.semicolon();
        self.enter();
    }
    pub fn parse_extern_id_node(&mut self, node: &ExternIdNode) {
        for (i, id) in node.ns.iter().enumerate() {
            id.format(self);
            if i != node.ns.len() {
                self.dbcolon();
            }
        }
        node.id.format(self);
    }
    pub fn parse_array_type_name_node(&mut self, node: &ArrayTypeNameNode) {
        self.l_bracket();
        node.id.format(self);
        self.space();
        self.asterisk();
        self.space();
        node.size.format(self);
        self.r_bracket();
    }
    pub fn parse_type_name_node(&mut self, node: &TypeNameNode) {
        if let Some(id_node) = &node.id {
            id_node.format(self);
        }
        if let Some(generic_params) = &node.generic_params {
            generic_params.format(self);
        }
    }
    pub fn parse_typed_identifier_node(&mut self, node: &TypedIdentifierNode) {
        self.enter();
        self.prefix();
        self.token(node.id.name.as_str());
        self.colon();
        self.space();
        node.typenode.format(self);
        self.semicolon();
        if let Some(doc) = &node.doc {
            doc.format(self);
        }
    }
    pub fn parse_struct_def_node(&mut self, node: &StructDefNode) {
        for c in node.pre_comments.iter() {
            c.format(self);
        }
        self.prefix();
        if let Some((modi, _)) = node.modifier {
            self.token(modi.get_str());
            self.space();
        }
        self.token("struct");
        self.space();
        self.token(node.id.name.as_str());
        if let Some(generics) = &node.generics {
            generics.format(self);
        }
        self.space();
        self.l_brace();
        self.add_tab();
        for field in &node.fields {
            self.enter();
            self.prefix();
            if let Some((modi, _)) = field.modifier {
                self.token(modi.get_str());
                self.space();
            }
            self.token(field.id.id.name.as_str());
            self.colon();
            self.space();
            field.id.typenode.format(self);
            self.semicolon();
            if let Some(doc) = &field.id.doc {
                doc.format(self);
            }
        }
        self.enter();
        self.sub_tab();
        self.prefix();
        self.r_brace();
        self.enter();
        // 顶层节点加空格
        self.enter();
    }
    pub fn parse_pointer_type_node(&mut self, node: &PointerTypeNode) {
        self.asterisk();
        node.elm.format(self);
    }
    pub fn parse_struct_init_field_node(&mut self, node: &StructInitFieldNode) {
        self.prefix();
        self.token(&node.id.name);
        self.colon();
        self.space();
        node.exp.format(self);
    }

    pub fn parse_struct_init_node(&mut self, node: &StructInitNode) {
        node.typename.format(self);
        self.l_brace();
        let mut len = 0;
        if !node.fields.is_empty() {
            self.enter();
            self.add_tab();
            for field in &node.fields {
                len += 1;
                field.format(self);
                if len < node.fields.len() {
                    self.comma();
                }
                self.enter();
            }
            self.sub_tab();
            self.prefix();
        }
        self.r_brace();
    }
    pub fn parse_array_init_node(&mut self, node: &ArrayInitNode) {
        self.l_bracket();
        for (i, exp) in node.exps.iter().enumerate() {
            exp.format(self);
            if i != node.exps.len() - 1 {
                self.comma();
                self.space();
            }
        }
        self.r_bracket();
    }
    pub fn parse_generic_param_node(&mut self, node: &GenericParamNode) {
        self.l_angle_bracket();

        for (i, generic) in node.generics.iter().enumerate() {
            match generic {
                Some(n) => n.format(self),
                None => self.underline(),
            }
            if i != node.generics.len() - 1 {
                self.parallel();
            }
        }
        self.r_angle_bracket();
    }
    pub fn parse_def_node(&mut self, node: &DefNode) {
        self.token("let");
        self.space();
        node.var.format(self);
        if let Some(tp) = &node.tp {
            self.colon();
            self.space();
            tp.format(self);
        }
        if let Some(exp) = &node.exp {
            self.space();
            self.equal();
            self.space();
            exp.format(self);
        }
    }
    pub fn parse_assign_node(&mut self, node: &AssignNode) {
        node.var.format(self);
        self.space();
        self.equal();
        self.space();
        node.exp.format(self);
    }

    pub fn parse_empty_node(&mut self, _node: &EmptyNode) {}
    pub fn parse_statements_node(&mut self, node: &StatementsNode) {
        self.enter();
        for statement in &node.statements {
            if let NodeEnum::Empty(_) = &**statement {
                continue;
            }
            self.prefix();
            statement.format(self);
            match &**statement {
                NodeEnum::For(_) | NodeEnum::While(_) | NodeEnum::If(_) | NodeEnum::Comment(_) => {}
                _ => {
                    self.semicolon();
                }
            }
            match &**statement {
                NodeEnum::Comment(_) => {}
                _ => {
                    self.enter();
                }
            }
        }
    }
    pub fn parse_ret_node(&mut self, node: &RetNode) {
        if let Some(value) = &node.value {
            self.token("return");
            self.space();
            value.format(self);
        } else {
            self.token("return");
        }
    }
    pub fn parse_primary_node(&mut self, node: &PrimaryNode) {
        node.value.format(self);
    }

    pub fn parse_array_element_node(&mut self, node: &ArrayElementNode) {
        node.arr.format(self);
        self.l_bracket();
        node.index.format(self);
        self.r_bracket();
    }
    pub fn parse_parantheses_node(&mut self, node: &ParanthesesNode) {
        self.l_paren();
        node.node.format(self);
        self.r_paren();
    }
    pub fn parse_pointer_op_node(&mut self, node: &PointerOpNode) {
        match node.op {
            PointerOpEnum::Addr => self.and(),
            PointerOpEnum::Deref => self.asterisk(),
        }
        node.value.format(self);
    }
    pub fn parse_unary_op_node(&mut self, node: &UnaryOpNode) {
        self.token(TokenType::get_str(&node.op.0));
        node.exp.format(self);
    }

    pub fn parse_bin_op_node(&mut self, node: &BinOpNode) {
        node.left.format(self);
        self.space();
        self.token(TokenType::get_str(&node.op.0));
        self.space();
        node.right.format(self);
    }
    pub fn parse_take_op_node(&mut self, node: &TakeOpNode) {
        node.head.format(self);
        for id in &node.field {
            self.dot();
            id.format(self);
        }
    }
    pub fn parse_impl_node(&mut self, node: &ImplNode) {
        self.token("impl");
        if let Some(generics) = &node.generics {
            generics.format(self)
        }
        self.space();
        if node.impl_trait.is_some() {
            node.impl_trait.as_ref().unwrap().0.format(self);
            self.space();
            self.token("for");
            self.space();
        }
        node.target.format(self);
        self.space();
        self.l_brace();
        self.enter();
        self.add_tab();
        for method in &node.methods {
            method.format(self);
        }
        self.sub_tab();
        self.r_brace();
        self.enter();
        // 顶层节点加空格
        self.enter();
    }
    pub fn parse_global_node(&mut self, node: &GlobalNode) {
        self.token("const");
        self.space();
        node.var.format(self);
        self.space();
        self.equal();
        self.space();
        node.exp.format(self);
        self.semicolon();
        // 顶层节点加空格
        self.enter();
    }
    pub fn parse_func_call_node(&mut self, node: &FuncCallNode) {
        node.callee.format(self);
        if let Some(generic_params) = &node.generic_params {
            generic_params.format(self);
        }
        self.l_paren();
        if !node.paralist.is_empty() {
            let mut len = 0;
            for param in &node.paralist {
                len += 1;
                param.format(self);
                if len < node.paralist.len() {
                    self.comma();
                    self.space();
                }
            }
        }
        self.r_paren();
    }
    pub fn parse_func_def_node(&mut self, node: &FuncDefNode) {
        let paralist = &node.paralist;
        let params_print = print_params(paralist);
        for c in node.pre_comments.iter() {
            self.prefix();
            c.format(self);
        }
        self.prefix();
        if let Some((modi, _)) = node.modifier {
            self.token(modi.get_str());
            self.space();
        }
        self.token("fn");
        self.space();
        self.token(node.id.name.split("::").last().unwrap());
        if let Some(generics) = &node.generics {
            generics.format(self);
        }
        self.l_paren();
        self.token(&params_print);
        self.r_paren();
        self.space();
        node.ret.format(self);
        if let Some(trait_bounds) = &node.trait_bounds {
            self.enter();
            self.token("where");
            self.enter();
            self.add_tab();
            trait_bounds[0..trait_bounds.len() - 1]
                .iter()
                .for_each(|bound| {
                    self.prefix();
                    bound.format(self);
                    self.comma();
                    self.enter();
                });
            self.prefix();
            trait_bounds.last().unwrap().format(self);
            self.enter();
            self.sub_tab();
        }
        match &node.body {
            Some(body) => {
                if node.trait_bounds.is_none() {
                    self.space();
                }
                self.l_brace();
                self.add_tab();
                body.format(self);
                self.sub_tab();
                self.prefix();
                self.r_brace();
            }
            None => {
                self.semicolon();
            }
        }
        self.enter();
        // 顶层节点加空格
        self.enter();
    }
    pub fn parse_st_error_node(&mut self, node: &StErrorNode) {
        node.st.format(self);
    }
    pub fn parse_error_node(&mut self, node: &ErrorNode) {
        self.enter();
        self.token(&node.src);
        self.enter();
    }
    pub fn parse_if_node(&mut self, node: &IfNode) {
        // format_res.push_str(&prefix.repeat(tabs));
        self.token("if");
        self.space();
        node.cond.format(self);
        self.space();
        self.l_brace();
        if let Some(el) = &node.els {
            self.add_tab();
            node.then.format(self);
            self.sub_tab();
            self.prefix();
            self.r_brace();
            self.space();
            self.token("else");
            self.space();
            self.l_brace();
            self.add_tab();
            el.format(self);
            self.sub_tab();
            // if el_str.bytes().len() > 0 {
            //     self.prefix();
            // }
            self.prefix();
            self.r_brace();
        } else {
            self.add_tab();
            node.then.format(self);
            self.sub_tab();
            self.prefix();
            self.r_brace();
        }
    }
    pub fn parse_while_node(&mut self, node: &WhileNode) {
        self.token("while");
        self.space();
        node.cond.format(self);
        self.space();
        self.l_brace();
        self.add_tab();
        node.body.format(self);
        self.sub_tab();
        self.prefix();
        self.r_brace();
    }
    pub fn parse_for_node(&mut self, node: &ForNode) {
        self.token("for");
        self.space();
        if let Some(pre) = &node.pre {
            pre.format(self);
        }
        self.semicolon();
        self.space();
        node.cond.format(self);
        if let Some(opt) = &node.opt {
            self.semicolon();
            self.space();
            opt.format(self);
        }
        self.space();
        self.l_brace();
        self.add_tab();
        node.body.format(self);
        self.sub_tab();
        self.prefix();
        self.r_brace();
    }
    pub fn parse_comment_node(&mut self, node: &CommentNode) {
        if node.is_doc {
            self.token("///");
        } else {
            self.token("//");
        }
        self.token(&node.comment);
        self.enter();
    }
    pub fn parse_continue_node(&mut self, _node: &ContinueNode) {
        self.token("continue");
    }
    pub fn parse_break_node(&mut self, _node: &BreakNode) {
        self.token("break");
    }
    pub fn parse_bool_const_node(&mut self, node: &BoolConstNode) {
        self.token(node.value.to_string().as_str());
    }
    pub fn parse_num_node(&mut self, node: &NumNode) {
        if let Num::Int(x) = node.value {
            self.token(x.to_string().as_str());
        } else if let Num::Float(x) = node.value {
            self.token(x.to_string().as_str());
        }
    }
    pub fn parse_generic_def_node(&mut self, node: &GenericDefNode) {
        self.l_angle_bracket();
        self.token(
            &node.generics[0..node.generics_size]
                .iter()
                .map(|g| g.name.clone())
                .collect::<Vec<_>>()
                .join("|"),
        );
        self.r_angle_bracket();
    }
    pub fn parse_string_node(&mut self, node: &StringNode) {
        self.token(&format!("{:?}", node.content));
    }
    pub fn parse_trait_def_node(&mut self, node: &TraitDefNode) {
        // for c in node.precom.iter() {
        //     c.format(self);
        // }
        self.prefix();
        self.token("trait");
        self.space();
        self.token(node.id.name.as_str());
        self.space();
        self.l_brace();
        self.enter();
        self.add_tab();
        for m in &node.methods {
            m.format(self);
        }
        self.sub_tab();
        self.prefix();
        self.r_brace();
        self.enter();
        // 顶层节点加空格
        self.enter();
    }
    pub fn parse_macro_node(&mut self, node: &MacroNode) {
        self.prefix();
        self.token("macro");
        self.space();
        self.token(node.id.name.as_str());
        self.space();
        self.l_brace();
        self.enter();
        self.add_tab();
        for m in &node.rules {
            m.format(self);
        }
        self.sub_tab();
        self.prefix();
        self.r_brace();
    }
    pub fn parse_macro_loop_statement_node(&mut self, node: &MacroLoopStatementNode) {
        self.token("$(");
        node.statements.format(self);
        self.token(")*");
    }
    pub fn parse_macro_rule_node(&mut self, node: &MacroRuleNode) {
        self.prefix();
        self.l_paren();
        for _e in &node.match_exp {
            todo!()
        }
        self.r_paren();
        self.space();
        self.token("=>");
        self.space();
        node.body.format(self);
        self.enter();
    }
    pub fn parse_macro_call_node(&mut self, node: &MacroCallNode) {
        node.callee.format(self);
        self.token("!");
        self.l_paren();
        self.token(&node.args);
        self.r_paren();
    }
    pub fn parse_trait_bound_node(&mut self, node: &TraitBoundNode) {
        node.generic.format(self);
        self.token(":");
        self.space();
        node.impl_trait.format(self);
    }
}
