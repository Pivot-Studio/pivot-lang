use proc_macro::TokenStream;
use quote::{__private::Literal, format_ident, quote};
use syn::{
    self,
    parse::{Parse, ParseStream},
    parse_macro_input, Ident, ImplItem, ItemFn, ItemImpl,
};

/// The `#[is_runtime]` attribute.  
///
/// used to tag a function as a runtime function  
/// or tag an impl block to indicate that all the pub fn in impl block are runtime functions  
///
/// those functions will be added to the llvm symbol table  
///
/// while tagging a function, you can specify the name of the function in the llvm symbol table like this:  
///
/// ```no_run
/// #[internal_macro::is_runtime("myfunc")]
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
/// ```no_run
/// struct MyStruct;
/// #[internal_macro::is_runtime("struct")]
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
                str1 = arg.to_string();
                str1.pop();
                str1 = str1[1..].to_string();
            } else {
                panic!("failed to get annotated func/struct name");
            }

            let initfnid = format_ident!("add_symbol_{}", str1);
            let fnid = input.sig.ident.clone();

            return quote!(
                #input
                #[internal_macro::ctor::ctor]
                fn #initfnid() {
                    let ptr = #fnid as * const ();
                    let name = #str1;
                    unsafe{
                        internal_macro::add_symbol(name, ptr);
                    }
                }
            )
            .into();
        }
        AcceptInput::ItemImpl(input) => impl_macro_impl(&arg.input, &input),
    }
}

struct AttrInput {
    input: AcceptAttrInput,
}

#[derive(Debug, Clone)]
enum AcceptAttrInput {
    Literal(Literal),
    None,
}

impl Parse for AttrInput {
    fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        let f = input.parse::<Literal>();
        if let Ok(f) = f {
            return Ok(AttrInput {
                input: AcceptAttrInput::Literal(f),
            });
        }
        return Ok(AttrInput {
            input: AcceptAttrInput::None,
        });
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
        return Err(imp.err().unwrap());
    }
}

fn impl_macro_impl(arg: &AcceptAttrInput, ast: &ItemImpl) -> TokenStream {
    let mut fnids = Vec::<Ident>::new();
    let mut fns = Vec::<String>::new();
    let ident = match &*ast.self_ty {
        syn::Type::Path(tp) => tp.path.get_ident().unwrap(),
        _ => panic!("not supported tokens"),
    };
    let mut tp;
    if let AcceptAttrInput::None = arg {
        tp = ident.to_string();
    } else if let AcceptAttrInput::Literal(arg) = arg {
        tp = arg.to_string();
        tp.pop();
        tp = tp[1..].to_string();
    } else {
        panic!("failed to get annotated func/struct name");
    }
    for i in ast.items.iter() {
        if let ImplItem::Method(m) = i {
            if let syn::Visibility::Public(_) = m.vis {
                let id = m.sig.ident.clone();
                let str = id.to_string();
                let fname = format!("{}__{}", tp, str);
                fns.push(fname);
                fnids.push(id);
            }
        }
    }
    let initfnid = format_ident!("add_symbol_impl_{}", tp.to_lowercase());
    let gen = quote! {
        #ast
        #[internal_macro::ctor::ctor]
        fn #initfnid() {
            #(
                let ptr = #ident::#fnids as * const ();
                let name = #fns;
                unsafe{
                    internal_macro::add_symbol(name, ptr);
                }
            )*
        }
    };
    return gen.into();
}
