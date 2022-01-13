#![allow(dead_code)]
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
                #name: Option<#ty>,
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
                    #field_name: None,
                )*
            }
        }
    }
}

fn setter_impl(data: &syn::Data) -> TokenStream {
    let fields = parse_field(data);
    // eprintln!("{:#?}", fields);

    let fn_def = fields.into_iter().map(
        |syn::Field {
             ident: name,
             ty,
             attrs,
             ..
         }| {
            if let Some(fn_name) = parse_each_attr(attrs) {
                if let Some(ty) = unwrap_vec(ty) {
                    let fn_name = Ident::new(&fn_name, Span::call_site());
                    let vec_name = Ident::new(&format!("{}_vec", fn_name), Span::call_site());

                    return quote! {
                        pub fn #fn_name(&mut self, #fn_name: #ty) -> &mut Self {
                            if let Some(mut #vec_name) = self.#name.as_mut() {
                                #vec_name.push(#fn_name);
                            } else {
                                self.#name = Some(vec![#fn_name]);
                            }
                            self
                        }
                    };
                } else {
                    panic!("each attr is only for `Vec<T>`")
                }
            }

            let ty = unwrap_option(ty).unwrap_or(ty.clone());
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
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

    let field_def = fields.into_iter().map(
        |syn::Field {
             ident: name,
             ty,
             attrs,
             ..
         }| {
            if let Some(_) = parse_each_attr(attrs) {
                quote! {
                    #name: self.#name.clone().unwrap_or(vec![]),
                }
            } else if let Some(_) = unwrap_option(ty) {
                quote! {
                    #name: self.#name.clone(),
                }
            } else {
                let err_msg = format!(
                    "'{}' not set",
                    name.clone()
                        .map_or(String::from("unknown field"), |ident| ident.to_string())
                );
                quote! {
                    #name: self.#name.clone().ok_or(#err_msg)?,
                }
            }
        },
    );

    quote! {
        pub fn build(&mut self) -> std::result::Result<#target_name, Box<dyn std::error::Error>> {
            std::result::Result::Ok(
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

fn parse_attrs(
    attr_ident: &str,
    key_ident: &str,
    attrs: &Vec<syn::Attribute>,
) -> Vec<Option<String>> {
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
                                    return Some(s.value());
                                }
                            } else {
                                panic!(
                                    "parse_attrs: unknown attr '{}' was given",
                                    meta.path.segments[0].ident
                                );
                            }
                        }
                    }
                    Err(e) => {
                        panic!("parse_attrs: {}", e);
                    }
                }
            }

            None
        })
        .collect::<Vec<_>>()
}

fn parse_each_attr(attrs: &Vec<syn::Attribute>) -> Option<String> {
    let each_key = parse_attrs("builder", "each", attrs);
    let each_key = each_key
        .iter()
        .filter_map(|k| k.as_ref())
        .collect::<Vec<_>>();
    if each_key.len() > 1 {
        panic!("attr 'key' was implemented multiple times");
    }

    if each_key.len() == 1 {
        return Some(each_key[0].to_string());
    }
    None
}
