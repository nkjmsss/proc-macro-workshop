use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let target_name = &input.ident;
    let builder_name = Ident::new(&format!("{}Builder", target_name), Span::call_site());

    let (field_name, field_type) = parse_field(&input.data);
    let builder_impl = builder_impl(&input.data, &builder_name);

    let expanded = quote! {
        pub struct #builder_name {
            #(
                #field_name: Option<#field_type>,
            )*
        }

        impl #target_name {
            #builder_impl
        }
    };

    // Hand the output tokens back to the compiler.
    proc_macro::TokenStream::from(expanded)
}

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
