use proc_macro::TokenStream;
use quote::quote;
use syn::{parse::Parser, parse_macro_input, DeriveInput};
#[proc_macro_attribute]
pub fn comments(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut ast = parse_macro_input!(input as DeriveInput);
    match &mut ast.data {
        syn::Data::Struct(ref mut struct_data) => {
            match &mut struct_data.fields {
                syn::Fields::Named(fields) => {
                    fields.named.push(
                        syn::Field::parse_named
                            .parse2(
                                quote! { pub comments : Vec<Vec<Box<crate::ast::node::NodeEnum>>> },
                            )
                            .unwrap(),
                    );
                }
                _ => (),
            }
            return quote! {
                #ast
            }
            .into();
        }
        _ => panic!("`range` has to be used with structs "),
    }
}
