use proc_macro::TokenStream;

use quote::format_ident;
use quote::quote;
use syn::{DeriveInput, Error};

pub(crate) fn into_tokens(input: &DeriveInput) -> Result<TokenStream, Error> {
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
                quote! { self.#ident.into_tokens(sink)?; }
            })
            .collect::<Vec<_>>(),
        syn::Fields::Unit => vec![],
    };

    Ok(quote! {
        impl<#generics_params> ::serser::IntoTokens for #ident<#generics_params> #where_clause {
            fn into_tokens<S: ::serser::TokenSink>(&self, sink: &mut S) -> Result<(), S::Error> {
                use ::serser::IntoTokens;

                sink.yield_token(#start_token)?;

                #(#yield_fields)*

                sink.yield_token(#end_token).map(|_| ())
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
        sink.yield_token(::serser::token::Token::Field(#name_str))?;
        self.#ident.into_tokens(sink)?;
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
    let kind_matches = data
        .variants
        .iter()
        .map(|variant| {
            let ident = &variant.ident;
            let fields = variant
                .fields
                .iter()
                .map(|_| format_ident!("_"))
                .collect::<Vec<_>>();

            match &variant.fields {
                syn::Fields::Unnamed(_) => {
                    quote! { Self::#ident (#(#fields),*) => Some(::serser::token::EnumKind::Tuple) }
                }
                syn::Fields::Named(_) => {
                    quote! { Self::#ident {..} => Some(::serser::token::EnumKind::Struct) }
                }
                syn::Fields::Unit => quote! { Self::#ident => None },
            }
        })
        .collect::<Vec<_>>();
    let variant_names = data.variants.iter().map(|variant| {
        let ident = &variant.ident;
        let name_str = syn::LitStr::new(
            format!("{}", ident).as_str(),
            proc_macro2::Span::call_site(),
        );
        let fields = variant
            .fields
            .iter()
            .map(|_| format_ident!("_"))
            .collect::<Vec<_>>();
        let captures = match &variant.fields {
            syn::Fields::Unnamed(_) => quote! { (#(#fields),*) },
            syn::Fields::Named(_) => quote! { {..} },
            syn::Fields::Unit => quote! {},
        };

        quote! { Self::#ident #captures => #name_str, }
    });
    let yield_fields = data
        .variants
        .iter()
        .map(|variant| {
            let ident = &variant.ident;

            match &variant.fields {
                syn::Fields::Unnamed(_) => {
                    let fields = variant
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(i, _)| format_ident!("f{}", i))
                        .collect::<Vec<_>>();
                    let num_fields = fields.len();
                    let yields = fields
                        .iter()
                        .map(|ident| quote! { #ident.into_tokens(sink)?; })
                        .collect::<Vec<_>>();

                    quote! {
                        Self::#ident (#(#fields),*) => {
                            sink.yield_token(::serser::token::Token::Tuple(TupleMeta {
                                size_hint: Some(#num_fields),
                            }))?;
                            #(#yields)*
                            sink.yield_token(::serser::token::Token::EndTuple)?;
                        }
                    }
                }
                syn::Fields::Named(orig_fields) => {
                    let fields = variant
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(j, field)| {
                            let orig_ident = field.ident.as_ref().unwrap();
                            let ident = format_ident!("f{}", j);
                            quote! { #orig_ident: #ident }
                        })
                        .collect::<Vec<_>>();
                    let field_names = orig_fields
                        .named
                        .iter()
                        .map(|field| format!("{}", field.ident.as_ref().unwrap()))
                        .collect::<Vec<_>>();
                    let yields = orig_fields
                        .named
                        .iter()
                        .enumerate()
                        .map(|(j, field)| {
                            let name_str = format!("{}", field.ident.as_ref().unwrap());
                            let ident = format_ident!("f{}", j);

                            quote! {
                                sink.yield_token(::serser::token::Token::Field(#name_str))?;
                                #ident.into_tokens(sink)?;
                            }
                        })
                        .collect::<Vec<_>>();

                    quote! {
                        Self::#ident { #(#fields),* } => {
                            sink.yield_token(::serser::token::Token::Struct(StructMeta {
                                fields: Some(&[#(#field_names),*]),
                            }))?;
                            #(#yields)*
                            sink.yield_token(::serser::token::Token::EndStruct)?;
                        }
                    }
                }
                syn::Fields::Unit => quote! { Self::#ident => {} },
            }
        })
        .collect::<Vec<_>>();

    Ok(quote! {
        impl<#generics_params> ::serser::IntoTokens for #ident<#generics_params> #where_clause {
            fn into_tokens<S: ::serser::TokenSink>(&self, sink: &mut S) -> Result<(), S::Error> {
                use ::serser::IntoTokens;

                sink.yield_token(::serser::token::Token::Enum(EnumMeta {
                    variants: Some(&[#(::serser::token::EnumVariant::Str(#variants)),*]),
                    kind: match self {
                        #(#kind_matches),*
                    },
                }))?;
                sink.yield_token(::serser::token::Token::Variant(::serser::token::EnumVariant::Str(match self {
                    #(#variant_names)*
                })))?;

                match self {
                    #(#yield_fields)*
                }

                sink.yield_token(::serser::token::Token::EndEnum).map(|_| ())
            }
        }
    }
    .into())
}
