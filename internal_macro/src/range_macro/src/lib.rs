use proc_macro::TokenStream;
use quote::quote;
use syn::{parse::Parser, parse_macro_input, DeriveInput};
#[proc_macro_attribute]
pub fn range(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut ast = parse_macro_input!(input as DeriveInput);
    match &mut ast.data {
        syn::Data::Struct(ref mut struct_data) => {
            if let syn::Fields::Named(fields) = &mut struct_data.fields {
                fields.named.push(
                    syn::Field::parse_named
                        .parse2(quote! { pub range : crate::ast::range::Range })
                        .unwrap(),
                );
            }
            let ident = &ast.ident;
            quote! {
                #ast
                impl crate::ast::node::RangeTrait for #ident {
                    fn range(&self) -> crate::ast::range::Range {
                        self.range
                    }
                }
            }
            .into()
        }
        _ => panic!("`range` has to be used with structs "),
    }
}
