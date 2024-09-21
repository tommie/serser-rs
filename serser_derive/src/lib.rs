extern crate proc_macro;
use proc_macro::TokenStream;

use quote::format_ident;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Error};

/// Provides the [IntoTokens] trait for a struct or enum. This is used
/// to serialize Rust data.
#[proc_macro_derive(IntoTokens)]
pub fn derive_into_tokens(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    match into_tokens(&input) {
        Ok(output) => output,
        Err(err) => err.into_compile_error().into(),
    }
}

fn into_tokens(input: &DeriveInput) -> Result<TokenStream, Error> {
    match &input.data {
        syn::Data::Struct(data) => into_tokens_struct(data, input),
        syn::Data::Enum(data) => into_tokens_enum(data, input),
        syn::Data::Union(_data) => Err(Error::new_spanned(
            input,
            "union is not supported by IntoTokens",
        )),
    }
}

fn into_tokens_struct(data: &syn::DataStruct, input: &DeriveInput) -> Result<TokenStream, Error> {
    let generics_params = &input.generics.params;
    let where_clause = &input.generics.where_clause;
    let ident = &input.ident;
    let (start_token, end_token) = match &data.fields {
        syn::Fields::Named(fields) => {
            let fields = fields
                .named
                .iter()
                .map(|field| {
                    syn::LitStr::new(
                        format!("{}", field.ident.as_ref().unwrap()).as_str(),
                        proc_macro2::Span::call_site(),
                    )
                })
                .collect::<Vec<_>>();

            (
                quote! {
                    ::serser::token::Token::Struct(::serser::token::StructMeta {
                        fields: Some(&[#(#fields),*]),
                    })
                },
                quote! { ::serser::token::Token::EndStruct },
            )
        }
        syn::Fields::Unnamed(fields) => {
            let size = fields.unnamed.len();

            (
                quote! {
                        ::serser::token::Token::Tuple(::serser::token::TupleMeta {
                            size_hint: Some(#size),
                        })
                },
                quote! { ::serser::token::Token::EndTuple },
            )
        }
        syn::Fields::Unit => (
            quote! {
                    ::serser::token::Token::Tuple(::serser::token::TupleMeta {
                        size_hint: Some(0),
                    })
            },
            quote! { ::serser::token::Token::EndTuple },
        ),
    };
    let yield_fields = match &data.fields {
        syn::Fields::Named(fields) => fields
            .named
            .iter()
            .map(|field| yield_field(field))
            .collect::<Result<Vec<_>, _>>()?,
        syn::Fields::Unnamed(fields) => fields
            .unnamed
            .iter()
            .enumerate()
            .map(|(i, _field)| {
                let ident = syn::Member::Unnamed(syn::Index {
                    index: i as u32,
                    span: proc_macro2::Span::call_site(),
                });
                quote! { self.#ident.into_tokens(&mut subsink)?; }
            })
            .collect::<Vec<_>>(),
        syn::Fields::Unit => vec![],
    };

    Ok(quote! {
        impl<#generics_params> ::serser::IntoTokens for #ident<#generics_params> #where_clause {
            fn into_tokens<S: ::serser::TokenSink>(&self, sink: &mut S) -> Result<(), S::Error> {
                use ::serser::IntoTokens;

                let mut subsink = sink.yield_start(#start_token)?;

                #(#yield_fields)*

                sink.yield_end(#end_token, subsink)
            }
        }
    }
    .into())
}

fn yield_field(field: &syn::Field) -> Result<proc_macro2::TokenStream, Error> {
    let ident = field.ident.as_ref().unwrap();
    let name_str = syn::LitStr::new(
        format!("{}", ident).as_str(),
        proc_macro2::Span::call_site(),
    );

    Ok(quote! {
        subsink.yield_token(::serser::token::Token::Field(#name_str))?;
        self.#ident.into_tokens(&mut subsink)?;
    })
}

fn into_tokens_enum(data: &syn::DataEnum, input: &DeriveInput) -> Result<TokenStream, Error> {
    let generics_params = &input.generics.params;
    let where_clause = &input.generics.where_clause;
    let ident = &input.ident;
    let variants = data
        .variants
        .iter()
        .map(|variant| {
            syn::LitStr::new(
                format!("{}", variant.ident).as_str(),
                proc_macro2::Span::call_site(),
            )
        })
        .collect::<Vec<_>>();
    let variant_names = data.variants.iter().map(|variant| {
        let ident = &variant.ident;
        let name_str = syn::LitStr::new(
            format!("{}", ident).as_str(),
            proc_macro2::Span::call_site(),
        );
        let fields = if let syn::Fields::Unnamed(fields) = &variant.fields {
            fields
                .unnamed
                .iter()
                .map(|_| format_ident!("_"))
                .collect::<Vec<_>>()
        } else {
            Vec::new()
        };
        let captures = if fields.is_empty() {
            quote! {}
        } else {
            quote! { (#(#fields),*) }
        };

        quote! { Self::#ident #captures => #name_str, }
    });
    let yield_fields = data
        .variants
        .iter()
        .map(|variant| {
            let ident = &variant.ident;
            let fields = if let syn::Fields::Unnamed(fields) = &variant.fields {
                fields
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(i, _)| format_ident!("f{}", i))
                    .collect::<Vec<_>>()
            } else {
                Vec::new()
            };
            let captures = if fields.is_empty() {
                quote! {}
            } else {
                quote! { (#(#fields),*) }
            };
            let yields = fields
                .iter()
                .map(|ident| quote! { #ident.into_tokens(&mut subsink)?; })
                .collect::<Vec<_>>();

            quote! {
                Self::#ident #captures => {
                    #(#yields)*
                }
            }
        })
        .collect::<Vec<_>>();

    Ok(quote! {
        impl<#generics_params> ::serser::IntoTokens for #ident<#generics_params> #where_clause {
            fn into_tokens<S: ::serser::TokenSink>(&self, sink: &mut S) -> Result<(), S::Error> {
                use ::serser::IntoTokens;

                let mut subsink = sink.yield_start(::serser::token::Token::Enum(EnumMeta { variants: Some(&[#(::serser::token::EnumVariant::Str(#variants)),*]) }))?;

                subsink.yield_token(::serser::token::Token::Variant(::serser::token::EnumVariant::Str(match self {
                    #(#variant_names)*
                })))?;

                match self {
                    #(#yield_fields)*
                }

                sink.yield_end(::serser::token::Token::EndEnum, subsink)
            }
        }
    }
    .into())
}
