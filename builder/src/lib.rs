use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let target_name = &input.ident;
    let builder_name = Ident::new(&format!("{}Builder", target_name), Span::call_site());

    let (field_name, field_type) = parse_field(&input.data);

    // target impl
    let builder_impl = builder_impl(&input.data, &builder_name);

    // builder impl
    let setter_impl = setter_impl(&input.data);
    let build_impl = build_impl(&input.data, target_name);

    let expanded = quote! {
        pub struct #builder_name {
            #(
                #field_name: Option<#field_type>,
            )*
        }

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

fn setter_impl(data: &syn::Data) -> TokenStream {
    let (field_name, field_type) = parse_field(data);
    let field_name2 = field_name.clone();
    let field_name3 = field_name.clone();
    let field_name4 = field_name.clone();

    quote! {
        #(
            pub fn #field_name(&mut self, #field_name2: #field_type) -> &mut Self {
                self.#field_name3 = Some(#field_name4);
                self
            }
        )*
    }
}

fn build_impl(data: &syn::Data, target_name: &Ident) -> TokenStream {
    let (field_name, _) = parse_field(data);
    let field_name2 = field_name.clone();
    let err_msg = field_name.clone().into_iter().map(|v| {
        if let Some(v) = v {
            Some(format!("'{}' not set", v.to_string()))
        } else {
            None
        }
    });

    quote! {
        pub fn build(&mut self) -> std::result::Result<#target_name, Box<dyn std::error::Error>> {
            std::result::Result::Ok(
                #target_name {
                    #(
                        #field_name: self.#field_name2.clone().ok_or(#err_msg)?,
                    )*
                }
            )
        }
    }
}
