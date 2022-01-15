#![allow(dead_code)]

use itertools::izip;
use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let target_name = &input.ident;
    let builder_name = Ident::new(&format!("{}Builder", target_name), Span::call_site());

    let builder_struct = builder_struct(&input.data, &builder_name);

    // target impl
    let builder_impl = builder_impl(&input.data, &builder_name);

    // builder impl
    let setter_impl = setter_impl(&input.data);
    let build_impl = build_impl(&input.data, target_name);

    let expanded = quote! {
        #builder_struct

        impl #target_name {
            #builder_impl
        }

        impl #builder_name {
            #setter_impl
            #build_impl
        }
    };

    // Hand the output tokens back to the compiler.
    proc_macro::TokenStream::from(expanded)
}

fn builder_struct(data: &syn::Data, builder_name: &Ident) -> TokenStream {
    let fields = parse_field(data);

    let field_def = fields.into_iter().map(
        |syn::Field {
             ident: name, ty, ..
         }| {
            let ty = unwrap_option(ty).unwrap_or(ty.clone());
            quote! {
                #name: ::std::option::Option<#ty>,
            }
        },
    );

    quote! {
        pub struct #builder_name {
            #(
                #field_def
            )*
        }
    }
}

fn builder_impl(data: &syn::Data, builder_name: &Ident) -> TokenStream {
    let fields = parse_field(data);
    let field_name = fields.into_iter().map(|syn::Field { ident, .. }| ident);

    quote! {
        pub fn builder() -> #builder_name {
            #builder_name {
                #(
                    #field_name: ::std::option::Option::None,
                )*
            }
        }
    }
}

fn setter_impl(data: &syn::Data) -> TokenStream {
    let fields = parse_field(data);
    // eprintln!("{:#?}", fields);

    let fn_def = fields.into_iter().map(
        |f @ syn::Field {
             ident: name,
             ty,
             attrs,
             ..
         }| {
            let attrs = parse_attrs(attrs);

            if let Some(tok) = get_attrs_error(&attrs) {
                return tok;
            }

            match get_attr_each(&attrs) {
                Ok(fn_name) => {
                    if let Some(ty) = unwrap_vec(ty) {
                        let fn_name = Ident::new(&fn_name, Span::call_site());
                        let vec_name = Ident::new(&format!("{}_vec", fn_name), Span::call_site());

                        return quote! {
                            pub fn #fn_name(&mut self, #fn_name: #ty) -> &mut Self {
                                if let ::std::option::Option::Some(mut #vec_name) = self.#name.as_mut() {
                                    #vec_name.push(#fn_name);
                                } else {
                                    let mut #vec_name = ::std::vec::Vec::new();
                                    #vec_name.push(#fn_name);
                                    self.#name = ::std::option::Option::Some(#vec_name);
                                }
                                self
                            }
                        };
                    } else {
                        return mk_error(f, Errors::AttrEachToInvalidType);
                    }
                }
                Err(Some(tok)) => return tok,
                Err(None) => {}
            }

            let ty = unwrap_option(ty).unwrap_or(ty.clone());
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = ::std::option::Option::Some(#name);
                    self
                }
            }
        },
    );

    quote! {
        #(
            #fn_def
        )*
    }
}

fn build_impl(data: &syn::Data, target_name: &Ident) -> TokenStream {
    let fields = parse_field(data);

    let field_def = fields
        .into_iter()
        .map(
            |syn::Field {
                 ident: name,
                 ty,
                 attrs,
                 ..
             }| {
                let attrs = parse_attrs(attrs);

                if let Some(tok) = get_attrs_error(&attrs) {
                    return Err(tok);
                }

                match get_attr_each(&attrs) {
                    Ok(_) => {
                        return Ok(quote! {
                            #name: self.#name.clone().unwrap_or(::std::vec::Vec::new()),
                        })
                    }
                    Err(Some(tok)) => return Err(tok),
                    _ => {}
                }

                if let Some(_) = unwrap_option(ty) {
                    Ok(quote! {
                        #name: self.#name.clone(),
                    })
                } else {
                    let err_msg = format!(
                        "'{}' not set",
                        name.clone()
                            .map_or(String::from("unknown field"), |ident| ident.to_string())
                    );
                    Ok(quote! {
                        #name: self.#name.clone().ok_or(#err_msg)?,
                    })
                }
            },
        )
        .collect::<Vec<_>>();

    if let Some(err) = (|| {
        for f in &field_def {
            if let Err(e) = f {
                return Some(e.clone());
            }
        }
        None
    })() {
        return err;
    }

    let field_def = field_def.iter().map(|f| f.as_ref().unwrap());

    quote! {
        pub fn build(&mut self) -> ::std::result::Result<#target_name, ::std::boxed::Box<dyn ::std::error::Error>> {
            ::std::result::Result::Ok(
                #target_name {
                    #(
                        #field_def
                    )*
                }
            )
        }
    }
}

// =====================
// enums
// =====================
enum Attrs {
    Each(String),
}

enum Errors {
    Unhandled(String),
    UnknownAttr(String),
    ParseArgs(String),
    AttrEachAppearedMultiple,
    AttrEachToInvalidType,
}

impl std::fmt::Display for Errors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unhandled(name) => write!(f, "Unhandled error: {}", name),
            Self::UnknownAttr(attr) => {
                writeln!(f, "unknown attr '{}' was given", attr)?;
                write!(f, "consider writing `each = \"...\"`")
            }
            Self::ParseArgs(error) => write!(f, "Parse args error: {}", error),
            Self::AttrEachAppearedMultiple => {
                write!(f, "attr 'each' was implemented multiple times")
            }
            Self::AttrEachToInvalidType => write!(f, "each attr is only for `Vec<T>`"),
        }
    }
}

// =====================
// utils
// =====================
fn parse_field(data: &syn::Data) -> &syn::punctuated::Punctuated<syn::Field, syn::token::Comma> {
    match data {
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Named(fields),
            ..
        }) => &fields.named,
        _ => {
            panic!("Only for struct with named fields");
        }
    }
}

/// if matches given ident and params_count, returns `Some(Vec<params>)`
/// else, returns `None`
fn parse_type(ident: &str, params_count: usize, ty: &syn::Type) -> Option<Vec<syn::Type>> {
    if let syn::Type::Path(syn::TypePath { path, .. }) = ty {
        if path.segments.len() == 1 {
            let seg = &path.segments[0];

            if seg.ident != ident {
                return None;
            }

            match &seg.arguments {
                // The `<T>` in Option<T>
                syn::PathArguments::AngleBracketed(args) => {
                    let mut res = vec![];
                    for a in &args.args {
                        if let syn::GenericArgument::Type(t) = a {
                            res.push(t.clone());
                        }
                    }

                    if res.len() == params_count {
                        return Some(res);
                    } else {
                        return None;
                    }
                }
                // without params
                syn::PathArguments::None => {
                    if params_count == 0 {
                        return Some(vec![]);
                    } else {
                        return None;
                    }
                }
                _ => {
                    // don't handle parenth
                    return None;
                }
            }
        } else {
            // don't support with module path (e.g. std::option::Option<T>)
            // only Option<T>
            return None;
        }
    }

    // unknown or too difficult to parse
    None
}

fn unwrap_option(ty: &syn::Type) -> Option<syn::Type> {
    parse_type("Option", 1, ty).map(|v| v[0].clone())
}

fn unwrap_vec(ty: &syn::Type) -> Option<syn::Type> {
    parse_type("Vec", 1, ty).map(|v| v[0].clone())
}

fn mk_error(tokens: impl quote::ToTokens, err: Errors) -> TokenStream {
    syn::Error::new_spanned(tokens, err).to_compile_error()
}

fn parse_attr<'a>(
    attr_ident: &str,
    key_ident: &str,
    attrs: &'a Vec<syn::Attribute>,
) -> Vec<Result<(String, &'a syn::Attribute), TokenStream>> {
    attrs
        .iter()
        .map(|attr| {
            if attr.path.segments.len() == 1 && attr.path.segments[0].ident == attr_ident {
                match attr.parse_args::<syn::Meta>() {
                    Ok(meta) => {
                        // eprintln!("{:#?}", meta);
                        if let syn::Meta::NameValue(meta) = meta {
                            if meta.path.segments.len() == 1
                                && meta.path.segments[0].ident == key_ident
                            {
                                if let syn::Lit::Str(s) = meta.lit {
                                    return Ok((s.value(), attr));
                                }
                            } else {
                                // return Err(mk_error(attr, "expected `builder(each = \"...\")`"));
                                // eprintln!("{:#?}", );
                                return Err(mk_error(
                                    &meta,
                                    Errors::UnknownAttr(meta.path.segments[0].ident.to_string()),
                                ));
                            }
                        }
                    }
                    Err(e) => return Err(mk_error(attr, Errors::ParseArgs(e.to_string()))),
                }
            }

            Err(mk_error(
                attr,
                Errors::Unhandled(String::from("parse_attr")),
            ))
        })
        .collect::<Vec<_>>()
}

fn parse_attrs<'a>(
    attrs: &'a Vec<syn::Attribute>,
) -> Vec<Result<(Attrs, &'a syn::Attribute), TokenStream>> {
    let each_attrs = parse_attr("builder", "each", attrs);

    izip!(each_attrs)
        .map(|attrs| match attrs {
            Ok((each_attr, attr)) => Ok((Attrs::Each(each_attr), attr)),
            Err(err) => Err(err),
        })
        .collect::<Vec<_>>()
}

fn get_attrs_error(
    attrs: &Vec<Result<(Attrs, &syn::Attribute), TokenStream>>,
) -> Option<TokenStream> {
    for attr in attrs {
        if let Err(tok) = attr {
            return Some(tok.clone());
        }
    }

    None
}

fn get_attr_each(
    attrs: &Vec<Result<(Attrs, &syn::Attribute), TokenStream>>,
) -> Result<String, Option<TokenStream>> {
    let keys = attrs
        .iter()
        .filter_map(|a| {
            if let Ok((Attrs::Each(e), attr)) = a {
                Some((e, attr))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    if keys.len() > 1 {
        let attr = keys.iter().last().unwrap().1;

        return Err(Some(mk_error(attr, Errors::AttrEachAppearedMultiple)));
    }

    if keys.len() == 1 {
        return Ok(keys[0].0.to_string());
    }

    Err(None)
}
