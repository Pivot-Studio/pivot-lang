use proc_macro::TokenStream;
use quote::{format_ident, quote};
use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

use syn::{parse_macro_input, ItemFn};

#[proc_macro_attribute]
pub fn test_parser(args: TokenStream, input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as ItemFn);
    let args = parse_macro_input!(args as syn::LitStr);
    let mut hasher = DefaultHasher::new();
    args.token().to_string().hash(&mut hasher);
    let test_fn = format_ident!(
        "parser_test_err_{}_{}",
        ast.sig.ident.to_string(),
        hasher.finish()
    );
    let original_fn = format_ident!("{}", ast.sig.ident.to_string());
    quote! {
        #ast
        #[test]
        fn #test_fn() {
            let arg = #args;
            let span = Span::from(arg);
            match #original_fn(span) {
                Err(e) => panic!("{:?}",e),
                Ok(span) => {
                    if (span.0.len()!=0) {
                        panic!("span is not empty get {:?}",span.0);
                    }
                },
            }
        }
    }
    .into()
}
#[proc_macro_attribute]
pub fn test_parser_error(args: TokenStream, input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as ItemFn);
    let args = parse_macro_input!(args as syn::LitStr);
    let mut hasher = DefaultHasher::new();
    args.token().to_string().hash(&mut hasher);
    let test_fn = format_ident!(
        "parser_test_err_{}_{}",
        ast.sig.ident.to_string(),
        hasher.finish()
    );
    let original_fn = format_ident!("{}", ast.sig.ident.to_string());
    quote! {
        #ast
        #[test]
        fn #test_fn() {
            let arg = #args;
            let span = Span::from(arg);
            if let Ok(span) = #original_fn(span) {
                if (span.0.len()==0) {
                    panic!("expected err but get ok , input is {:?}",arg);
                }
            }
        }
    }
    .into()
}


#[proc_macro_attribute]
pub fn test_semi_stmt(args: TokenStream, input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as ItemFn);
    let args = parse_macro_input!(args as syn::LitStr);
    let mut hasher = DefaultHasher::new();
    args.token().to_string().hash(&mut hasher);
    let test_fn = format_ident!(
        "semi_stmt_test_{}_{}",
        ast.sig.ident.to_string(),
        hasher.finish()
    );
    let original_fn = format_ident!("{}", ast.sig.ident.to_string());
    quote! {
        #ast
        #[test]
        fn #test_fn() {
            let arg = #args;
            let span = Span::from(arg);
            match #original_fn(global_variable,global_variable)(span) {
                Err(e) => panic!("{:?}",e),
                Ok(span) => {
                    if (span.0.len()!=0) {
                        panic!("span is not empty get {:?}",span.0);
                    }
                },
            }
        }
    }
    .into()
}