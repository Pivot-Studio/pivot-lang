use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};
/// node is a macro to embed new fields, add some macros and implement
/// crate::ast::node::FmtTrait and crate::ast::node::RangeTrait
/// traits for the structure.
/// currently, it supports arguemnts as 'comment' and 'copy':
///
/// The node without any argument embeds a new field 'range' into a structure
/// ```rust
/// #[node]
/// pub struct demo {
///     pub filed: i32,
///     pub field2: String,
/// }
/// ```
/// The embedded structure looks like:
/// ```rust
/// #[derive(Clone, PartialEq, Eq, derivative::Derivative)]
/// #[derivative(Debug)]
/// pub struct demo {
///    pub filed: i32,
///    pub field2: String,
///    pub range: crate::ast::range::Range,
/// }
/// ```
///
/// If we pass the argument:
/// ```rust
/// #[node]
/// pub struct demo {
///     pub filed: i32,
///     pub field2: String,
/// }
/// ```
///
/// The embedded structure looks like:
/// ```rust
/// #[derive(Clone, PartialEq, Eq, derivative::Derivative)]
/// #[derivative(Debug)]
/// pub struct demo {
///    pub filed: i32,
///    pub field2: String,
///    pub comments: Vec<Vec<Box<crate::ast::node::NodeEnum>>>,
///    pub range: crate::ast::range::Range,
/// }
/// ```
#[proc_macro_attribute]
pub fn node(args: TokenStream, input: TokenStream) -> TokenStream {
    let mut ast = parse_macro_input!(input as DeriveInput);
    let need_comment = args
        .clone()
        .into_iter()
        .any(|x| x.to_string().to_lowercase() == "comment");
    let need_copy = args
        .into_iter()
        .last()
        .map(|x| x.to_string().to_lowercase().contains("copy"))
        .unwrap_or(false);
    match &mut ast.data {
        syn::Data::Struct(_) => {
            let extra_proc = if need_comment {
                quote! { #[internal_macro::comments] }
            } else {
                quote!()
            };
            let derive = if need_copy {
                quote! { #[derive(Clone, PartialEq, Eq, derivative::Derivative, Copy)] }
            } else {
                quote! { #[derive(Clone, PartialEq, Eq, derivative::Derivative)] }
            };
            quote! {
                #extra_proc
                #[internal_macro::range]
                #[internal_macro::fmt]
                #derive
                #[derivative(Debug)]
                #ast
            }
            .into()
        }
        _ => panic!("`node` has to be used with structs "),
    }
}
