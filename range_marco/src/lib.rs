use proc_macro::TokenStream;
use quote::quote;
use syn::{parse::Parser, parse_macro_input, DeriveInput};
#[proc_macro_attribute]
pub fn range(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut ast = parse_macro_input!(input as DeriveInput);
    match &mut ast.data {
        syn::Data::Struct(ref mut struct_data) => {
            match &mut struct_data.fields {
                syn::Fields::Named(fields) => {
                    fields.named.push(
                        syn::Field::parse_named
                            .parse2(quote! { pub range : crate::ast::Range })
                            .unwrap(),
                    );
                }
                _ => (),
            }
            let ident = &ast.ident;
            return quote! {
                #ast
                impl crate::ast::RangeTrait for #ident {
                    fn range(&self) -> crate::ast::Range {
                        self.range
                    }
                }
            }
            .into();
        }
        _ => panic!("`range` has to be used with structs "),
    }
}
