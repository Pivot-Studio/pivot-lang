use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    self,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    token::Comma,
    Expr, FnArg, Ident, ImplItem, ItemFn, ItemImpl,
};

/// # The `#[is_runtime]` attribute.  
///
/// used to tag a function as a runtime function  
/// or tag an impl block to indicate that all the pub fn in impl block are runtime functions  
///
/// those functions will be added to the llvm symbol table  
///
/// while tagging a function, you can specify the name of the function in the llvm symbol table like this:  
///
/// ```ignore
/// #[is_runtime("myfunc")]
/// pub fn myfunc1() {
///    // ...
/// }
/// ```
/// if the name is not specified, the name of the function will be used as the name in the llvm symbol table.  
///
/// while tagging an impl block, the name of the function in the llvm symbol table will be like {block_type_name}__{fn_name}.   
///
/// you can override the name of the block_type_name just like the function sample above.
///
/// ```ignore
/// struct MyStruct;
/// #[is_runtime("struct")]
/// impl MyStruct {
///    pub fn myfunc1() {
///       // ...
///    }
/// }
/// ```
/// the function myfunc1 will be added to the llvm symbol table with the name struct__myfunc1
#[proc_macro_attribute]
pub fn is_runtime(attr: TokenStream, item: TokenStream) -> TokenStream {
    // Construct a representation of Rust code as a syntax tree
    // that we can manipulate

    let input = parse_macro_input!(item as MacroInput);
    let arg = parse_macro_input!(attr as AttrInput);

    // Build the trait implementation
    match input.input {
        AcceptInput::ItemFn(input) => {
            let mut str1;
            if let AcceptAttrInput::None = arg.input {
                str1 = input.sig.ident.to_string();
            } else if let AcceptAttrInput::Literal(arg) = arg.input {
                str1 = arg.token().to_string();
                str1.pop();
                str1 = str1[1..].to_string();
            } else {
                panic!("failed to get annotated func/struct name");
            }

            let initfnid = format_ident!("add_symbol_{}", str1);
            let fnid = input.sig.ident.clone();
            // #[cfg(feature = "jit")]

            // #[cfg(not(feature = "jit"))]
            quote!(
                #[cfg(not(feature = "jit"))]
                #[no_mangle]
                pub unsafe extern "C" #input
                #[cfg(feature = "jit")]
                pub unsafe #input
                #[cfg(feature = "jit")]
                #[internal_macro::ctor::ctor]
                fn #initfnid() {
                    let ptr = #fnid as * const ();
                    let name = #str1;
                    unsafe{
                        internal_macro::add_symbol(name, ptr);
                    }
                }
            )
            .into()
        }
        AcceptInput::ItemImpl(input) => impl_macro_impl(&arg.input, &input),
    }
}

struct AttrInput {
    input: AcceptAttrInput,
}

#[derive(Clone)]
enum AcceptAttrInput {
    Literal(syn::LitStr),
    None,
}

impl Parse for AttrInput {
    fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        let f = input.parse::<syn::LitStr>();
        if let Ok(f) = f {
            return Ok(AttrInput {
                input: AcceptAttrInput::Literal(f),
            });
        }
        Ok(AttrInput {
            input: AcceptAttrInput::None,
        })
    }
}

struct MacroInput {
    input: AcceptInput,
}

enum AcceptInput {
    ItemFn(ItemFn),
    ItemImpl(ItemImpl),
}

impl Parse for MacroInput {
    fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        let f = input.parse::<ItemFn>();
        if let Ok(f) = f {
            return Ok(MacroInput {
                input: AcceptInput::ItemFn(f),
            });
        }
        let imp = input.parse::<ItemImpl>();
        if let Ok(imp) = imp {
            return Ok(MacroInput {
                input: AcceptInput::ItemImpl(imp),
            });
        }
        Err(imp.err().unwrap())
    }
}

fn impl_macro_impl(arg: &AcceptAttrInput, ast: &ItemImpl) -> TokenStream {
    let mut fnids = Vec::<Ident>::new();
    let mut extern_c_fns = Vec::<Ident>::new();
    let mut fns = Vec::<String>::new();
    let mut sigs = Vec::<quote::__private::TokenStream>::new();
    let ident = match &*ast.self_ty {
        syn::Type::Path(tp) => tp.path.get_ident().unwrap(),
        _ => panic!("not supported tokens"),
    };
    let mut tp;
    if let AcceptAttrInput::None = arg {
        tp = ident.to_string();
    } else if let AcceptAttrInput::Literal(arg) = arg {
        tp = arg.token().to_string();
        tp.pop();
        tp = tp[1..].to_string();
    } else {
        panic!("failed to get annotated func/struct name");
    }
    for i in ast.items.iter() {
        if let ImplItem::Method(m) = i {
            let mut clonedsig = m.sig.clone();
            let id = m.sig.ident.clone();
            let str = id.to_string();
            let fname = format!("{}__{}", tp, str);
            clonedsig.ident = format_ident!("{}", fname);
            let findent = format_ident!("{}", fname);
            extern_c_fns.push(findent.clone());
            let cl = clonedsig.inputs.clone();
            let inputs = clonedsig.inputs.clone().into_iter();
            let inputs2 = clonedsig.inputs.into_iter();

            let expr = transform_params(cl);

            let ret = clonedsig.output;
            let sfty = ast.self_ty.clone();
            let first = inputs.clone().next();
            let cfn;
            if Option::is_none(&first) {
                cfn = quote!(
                    #[no_mangle]
                    pub unsafe extern "C" fn #findent(#(#inputs,)*) #ret {
                        #sfty::#id #expr
                    }
                );
            } else {
                let first = first.unwrap();
                if let FnArg::Receiver(_) = first {
                    let inputs1 = inputs.clone().skip(1);
                    let inputs2 = inputs.skip(1);
                    cfn = quote!(
                        #[cfg(not(feature = "jit"))]
                        #[no_mangle]
                        pub unsafe extern "C" fn #findent(me: * mut #sfty,#(#inputs1,)*) #ret {
                            let me = &mut *me;
                            me.#id #expr
                        }
                        #[cfg(feature = "jit")]
                        pub unsafe fn #findent(me: * mut #sfty,#(#inputs2,)*) #ret {
                            let me = &mut *me;
                            me.#id #expr
                        }
                    );
                } else {
                    cfn = quote!(
                        #[cfg(not(feature = "jit"))]
                        #[no_mangle]
                        pub unsafe extern "C" fn #findent(#(#inputs,)*) #ret {
                            #sfty::#id #expr
                        }
                        #[cfg(feature = "jit")]
                        pub unsafe fn #findent(#(#inputs2,)*) #ret {
                            #sfty::#id #expr
                        }
                    );
                }
            }
            sigs.push(cfn);
            fns.push(fname);
            fnids.push(id);
        }
    }
    let initfnid = format_ident!("add_symbol_impl_{}", tp.to_lowercase());
    let gen = quote! {
        #ast
        #[cfg(feature = "jit")]
        #[internal_macro::ctor::ctor]
        fn #initfnid() {
            #(
                let ptr = #extern_c_fns as * const ();
                let name = #fns;
                unsafe{
                    internal_macro::add_symbol(name, ptr);
                }
            )*
        }
        #(
            #sigs
        )*
    };
    gen.into()
}

fn transform_params(params: Punctuated<syn::FnArg, syn::token::Comma>) -> Expr {
    // 1. Filter the params, so that only typed arguments remain
    // 2. Extract the ident (in case the pattern type is ident)
    let idents = params.iter().filter_map(|param| {
        if let syn::FnArg::Typed(pat_type) = param {
            if let syn::Pat::Ident(pat_ident) = *pat_type.pat.clone() {
                return Some(pat_ident.ident);
            }
        }
        None
    });

    // Add all idents to a Punctuated => param1, param2, ...
    let mut punctuated: Punctuated<syn::Ident, Comma> = Punctuated::new();
    idents.for_each(|ident| punctuated.push(ident));

    // Generate expression from Punctuated (and wrap with parentheses)
    let transformed_params = parse_quote!((#punctuated));
    transformed_params
}
