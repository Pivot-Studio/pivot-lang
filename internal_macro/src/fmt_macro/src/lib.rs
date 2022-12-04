use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput};
#[proc_macro_attribute]
pub fn format(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut ast = parse_macro_input!(input as DeriveInput);
    match &mut ast.data {
        syn::Data::Struct(_) => {
            let ident = &ast.ident;
            let fnid = format_ident!("parse{}", ident);
            return quote! {
                #ast
                impl crate::ast::node::FmtNode for #ident {
                    fn formatBuild(&self, builder: &mut crate::ast::fmt::FmtBuilder) {
                        builder.#fnid(self);
                    }
                }
            }
            .into();
        }
        _ => panic!("`format` has to be used with structs "),
    }
}
