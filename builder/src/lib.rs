use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder)]
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
    let (field_name, field_type) = parse_field(data);

    let field_def = field_name
        .into_iter()
        .zip(field_type.into_iter())
        .map(|(name, ty)| {
            let ty = unwrap_option(ty).unwrap_or(ty.clone());
            quote! {
                #name: Option<#ty>,
            }
        });

    quote! {
        pub struct #builder_name {
            #(
                #field_def
            )*
        }
    }
}

fn builder_impl(data: &syn::Data, builder_name: &Ident) -> TokenStream {
    let (field_name, _) = parse_field(data);

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
    let (field_name, field_type) = parse_field(data);

    let fn_def = field_name
        .into_iter()
        .zip(field_type.into_iter())
        .map(|(name, ty)| {
            let ty = unwrap_option(ty).unwrap_or(ty.clone());
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        });

    quote! {
        #(
            #fn_def
        )*
    }
}

fn build_impl(data: &syn::Data, target_name: &Ident) -> TokenStream {
    let (field_name, field_type) = parse_field(data);

    let field_def = field_name
        .into_iter()
        .zip(field_type.into_iter())
        .map(|(name, ty)| {
            if let Some(_) = unwrap_option(ty) {
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
        });

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
fn parse_field(data: &syn::Data) -> (Vec<&Option<syn::Ident>>, Vec<&syn::Type>) {
    let fields = match data {
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Named(fields),
            ..
        }) => &fields.named,
        _ => {
            panic!("Only for struct with named fields");
        }
    };
    let field_name = fields.iter().map(|f| &f.ident);
    let field_type = fields.iter().map(|f| &f.ty);

    (field_name.collect(), field_type.collect())
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
