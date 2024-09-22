extern crate proc_macro;
use proc_macro::TokenStream;

use quote::format_ident;
use quote::quote;
use syn::{DeriveInput, Error};

pub(crate) fn from_tokens(input: &DeriveInput) -> Result<TokenStream, Error> {
    match &input.data {
        syn::Data::Struct(_data) => todo!(),
        syn::Data::Enum(data) => from_tokens_enum(data, input),
        syn::Data::Union(_data) => Err(Error::new_spanned(
            input,
            "union is not supported by FromTokens",
        )),
    }
}

fn from_tokens_enum(data: &syn::DataEnum, input: &DeriveInput) -> Result<TokenStream, Error> {
    let generics_params = &input.generics.params;
    let where_clause = &input.generics.where_clause;
    let ident = &input.ident;
    let sink_ident = format_ident!("{}SerSerTokenSink", ident);
    let sink_variants = data.variants.iter().enumerate().map(|(i, variant)| {
        let ident = format_ident!("V{}", i);
        let fields = if let syn::Fields::Unnamed(fields) = &variant.fields {
            fields
                .unnamed
                .iter()
                .map(|field| {
                    let ty = &field.ty;

                    quote! { Option<#ty> }
                })
                .collect::<Vec<_>>()
        } else {
            Vec::new()
        };

        quote! { #ident (usize, #(#fields),*) }
    });
    let variant_str_matches = data.variants.iter().enumerate().map(|(i, variant)| {
        let ident = format_ident!("V{}", i);
        let name_str = syn::LitStr::new(
            format!("{}", variant.ident).as_str(),
            proc_macro2::Span::call_site(),
        );
        let fields = if let syn::Fields::Unnamed(fields) = &variant.fields {
            fields
                .unnamed
                .iter()
                .map(|_| quote! { None })
                .collect::<Vec<_>>()
        } else {
            Vec::new()
        };

        quote! {
            #name_str => {
                *self = Self::#ident(0, #(#fields),*);
                Ok(true)
            }
        }
    });
    let variant_state_matches = data
        .variants
        .iter()
        .enumerate()
        .map(|(i, variant)| {
            let ident = format_ident!("V{}", i);
            let field_params = if let syn::Fields::Unnamed(fields) = &variant.fields {
                fields
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(j, _)| {
                        let ident = format_ident!("f{}", j);

                        quote! { ref mut #ident }
                    })
                    .collect::<Vec<_>>()
            } else {
                Vec::new()
            };
            let field_matches = if let syn::Fields::Unnamed(fields) = &variant.fields {
                fields
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(j, field)| {
                        let fident = format_ident!("f{}", j);
                        let ty = &field.ty;

                        quote! {
                            Self::#ident(ref mut i @ #j, #(#field_params),*) => {
                                *#fident = Some(#ty::from_tokens(token)?);
                                *i += 1;
                                Ok(true)
                            }
                        }
                    })
                    .collect::<Vec<_>>()
            } else {
                Vec::new()
            };
            let field_placeholders = if let syn::Fields::Unnamed(fields) = &variant.fields {
                fields
                    .unnamed
                    .iter()
                    .map(|_| quote! { _ })
                    .collect::<Vec<_>>()
            } else {
                Vec::new()
            };
            let num_fields = field_placeholders.len();

            quote! {
                #(#field_matches)*

                Self::#ident(#num_fields, #(#field_placeholders),*) => match token {
                    ::serser::token::Token::EndEnum => Ok(false),
                    _ => Err(Self::Error::invalid_token(token, Some(::serser::token::TokenTypes::new(::serser::token::TokenType::EndEnum)))),
                }
            }
        });
    let expect_matches = data
        .variants
        .iter()
        .enumerate()
        .map(|(i, variant)| {
            let ident = format_ident!("V{}", i);
            let field_placeholders = if let syn::Fields::Unnamed(fields) = &variant.fields {
                fields
                    .unnamed
                    .iter()
                    .map(|_| quote! { _ })
                    .collect::<Vec<_>>()
            } else {
                Vec::new()
            };
            let field_matches = if let syn::Fields::Unnamed(fields) = &variant.fields {
                fields
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(j, field)| {
                        let ty = &field.ty;

                        quote! {
                            Self::#ident(#j, #(#field_placeholders),*) => <#ty as ::serser::FromTokenSink>::expect_initial(),
                        }
                    })
                    .collect::<Vec<_>>()
            } else {
                Vec::new()
            };

            // End-of-fields is handled by the common _ match.
            quote! { #(#field_matches)* }
        });
    let variant_from_sink_matches = data
        .variants
        .iter()
        .enumerate()
        .map(|(i, variant)| {
            let orig_ident = &variant.ident;
            let ident = format_ident!("V{}", i);
            let field_params = if let syn::Fields::Unnamed(fields) = &variant.fields {
                fields
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(j, _)| {
                        let ident = format_ident!("f{}", j);

                        quote! { Some(#ident) }
                    })
                    .collect::<Vec<_>>()
            } else {
                Vec::new()
            };
            let field_args = if let syn::Fields::Unnamed(fields) = &variant.fields {
                fields
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(j, _)| format_ident!("f{}", j))
                    .collect::<Vec<_>>()
            } else {
                Vec::new()
            };
            let num_fields = field_params.len();

            if num_fields == 0 {
                quote! {
                    Self::Sink::#ident(#num_fields) => Some(Self::#orig_ident),
                }
            } else {
                quote! {
                    Self::Sink::#ident(#num_fields, #(#field_params),*) => Some(Self::#orig_ident(#(#field_args),*)),
                }
            }
        });

    Ok(quote! {
        enum #sink_ident<#generics_params> {
            Unknown,
            Start,
            #(#sink_variants),*
        }

        impl<#generics_params> ::serser::TokenSink for #sink_ident<#generics_params> #where_clause {
            type Error = TokenError;

            fn yield_token(&mut self, token: Token<'_>) -> Result<bool, Self::Error> {
                match self {
                    Self::Unknown => match token {
                        ::serser::token::Token::Enum(_) => {
                            *self = Self::Start;
                            Ok(true)
                        }
                        _ => Err(Self::Error::invalid_token(token, Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Enum)))),
                    }
                    Self::Start => match token {
                        ::serser::token::Token::Variant(var @ ::serser::token::EnumVariant::Str(s)) => match s {
                            #(#variant_str_matches)*
                            _ => Err(Self::Error::invalid_variant(var)),
                        }
                        ::serser::token::Token::Variant(var) => Err(Self::Error::invalid_variant(var)),
                        _ => Err(Self::Error::invalid_token(token, Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Variant)))),
                    }

                    #(#variant_state_matches)*

                    _ => Err(Self::Error::invalid_token(token, None)),
                }
            }

            fn expect_tokens(&mut self) -> Option<TokenTypes> {
                match self {
                    Self::Unknown => Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Enum)),
                    Self::Start => Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Variant)),
                    #(#expect_matches)*
                    _ => Some(::serser::token::TokenTypes::EMPTY),
                }
            }
        }

        impl<#generics_params> ::serser::FromTokenSink for #ident<#generics_params> #where_clause {
            type Sink = #sink_ident;

            fn new_sink() -> Self::Sink {
                Self::Sink::Unknown
            }

            fn from_sink(sink: Self::Sink) -> Option<Self> {
                match sink {
                    #(#variant_from_sink_matches)*

                    _ => None,
                }
            }

            fn expect_initial() -> Option<::serser::token::TokenTypes> {
                Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Enum))
            }
        }
    }
    .into())
}
