use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput};
#[proc_macro_attribute]
pub fn fmt(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut ast = parse_macro_input!(input as DeriveInput);
    match &mut ast.data {
        syn::Data::Struct(_) => {
            let ident = &ast.ident;
            let fnid = format_ident!("{}", to_snake(format_ident!("parse{}", ident).to_string()));
            return quote! {
                #ast
                impl crate::ast::node::FmtTrait for #ident {
                    fn format(&self, builder: &mut crate::ast::fmt::FmtBuilder) {
                        builder.#fnid(self);
                    }
                }
            }
            .into();
        }
        _ => panic!("`format` has to be used with structs "),
    }
}

fn to_snake(id: String) -> String {
    let mut res = String::new();
    let mut last = '_';
    for c in id.chars() {
        if c.is_uppercase() {
            if last != '_' {
                res.push('_');
            }
            res.push(c.to_ascii_lowercase());
        } else {
            res.push(c);
        }
        last = c;
    }
    res
}
