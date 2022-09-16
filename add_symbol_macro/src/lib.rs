use proc_macro::TokenStream;
use syn::{self, parse, parse_macro_input, ImplItem, ItemImpl, DeriveInput, ImplItemMethod, Ident};
use quote::{quote, ToTokens, TokenStreamExt};


#[proc_macro_attribute]
pub fn is_runtime(attr: TokenStream, item: TokenStream) -> TokenStream {
    // Construct a representation of Rust code as a syntax tree
    // that we can manipulate
    let mut input = parse_macro_input!(item as ItemImpl);

    // Build the trait implementation
    impl_macro(& input)
}





fn impl_macro<'a>(ast: & ItemImpl) -> TokenStream {
    let mut fnids = Vec::<Ident>::new();
    let mut fns = Vec::<String>::new();
    let ident = match &*ast.self_ty {
        syn::Type::Path(tp) => tp.path.get_ident().unwrap(),
        _ => panic!("not supported tokens"),
    };

    for i in ast.items.iter() {
        if let ImplItem::Method(m) = i  {
            if let syn::Visibility::Public(_) = m.vis {
                let id = m.sig.ident.clone();
                let str = id.to_string();
                fns.push(str);
                fnids.push(id);
            }
        }
    }
    let gen = quote! {
        #ast
        impl #ident {
            pub fn add_symbol() {
                #(
                    let ptr = Self::#fnids as * const ();
                    let name = #fns;
                    unsafe{
                        crate::utils::add_symbol(name, ptr);
                    }
                )*
            }
        }
    };
    return gen.into();
}


