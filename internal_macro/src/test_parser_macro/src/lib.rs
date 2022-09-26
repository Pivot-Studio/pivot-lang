use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, ItemFn};
#[proc_macro_attribute]
pub fn test_parser(args: TokenStream, input: TokenStream) -> TokenStream {
    static mut I: i32 = 0;
    let ast = parse_macro_input!(input as ItemFn);
    unsafe {
        I += 1;
        let test_fn = format!("parser_test_{}_{}", ast.sig.ident.to_string(), I);
        let test_fn = format_ident!("{}", test_fn);
        let original_fn = format_ident!("{}", ast.sig.ident.to_string());
        let args = args.to_string();
        quote! {
            #ast
            #[test]
            fn #test_fn() {
                let arg = #args;
                let span = Span::from(arg);
                if let Err(e) = #original_fn(span) {
                    panic!("{:?}",e);
                }
            }
        }
        .into()
    }
}
#[proc_macro_attribute]
pub fn test_parser_error(args: TokenStream, input: TokenStream) -> TokenStream {
    static mut I: i32 = 0;
    let ast = parse_macro_input!(input as ItemFn);
    unsafe {
        I += 1;
        let test_fn = format!("parser_test_error_{}_{}", ast.sig.ident.to_string(), I);
        let test_fn = format_ident!("{}", test_fn);
        let original_fn = format_ident!("{}", ast.sig.ident.to_string());
        let args = args.to_string();
        quote! {
            #ast
            #[test]
            fn #test_fn() {
                let arg = #args;
                let span = Span::from(arg);
                if let Ok(_) = #original_fn(span) {
                    panic!("expected err but get ok , input is {:?}",arg);
                }
            }
        }
        .into()
    }
}
