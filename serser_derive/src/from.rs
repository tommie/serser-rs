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
    let state_ident = format_ident!("{}SerSerTokenSinkState", ident);
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
                self.state = #state_ident::#ident(0, #(#fields),*);
                Ok(true)
            }
        }
    });
    let end_state_matches = data
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
                            #state_ident::#ident(ref mut i @ #j, #(#field_params),*) => {
                                // We created the subsink when we set i = j, so it should be valid.
                                let cast_subsink = subsink.into_any().downcast::<<#ty as ::serser::FromTokenSink>::Sink>().unwrap();
                                if let Some(v) = #ty::from_sink(*cast_subsink) {
                                    *#fident = Some(v);
                                    *i += 1;
                                    Ok(true)
                                } else {
                                    unreachable!("subsink yield_token signaled end, but no value is available")
                                }
                            }
                        }
                    })
                    .collect::<Vec<_>>()
            } else {
                Vec::new()
            };

            quote! {
                #(#field_matches)*
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
                            #state_ident::#ident(ref mut i @ #j, #(#field_params),*) => {
                                let mut subsink = #ty::new_sink();

                                if subsink.yield_token(token)? {
                                    self.subsink = Some(Box::new(subsink));
                                } else if let Some(v) = #ty::from_sink(subsink) {
                                    *#fident = Some(v);
                                    *i += 1;
                                } else {
                                    unreachable!("subsink yield_token signaled end, but no value is available");
                                }
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

                #state_ident::#ident(#num_fields, #(#field_placeholders),*) => match token {
                    ::serser::token::Token::EndEnum => Ok(false),
                    t => Err(Self::Error::invalid_token(t, Some(::serser::token::TokenTypes::new(::serser::token::TokenType::EndEnum)))),
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
                            #state_ident::#ident(#j, #(#field_placeholders),*) => <#ty as ::serser::FromTokenSink>::expect_initial(),
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
                    #state_ident::#ident(#num_fields) => Some(Self::#orig_ident),
                }
            } else {
                quote! {
                    #state_ident::#ident(#num_fields, #(#field_params),*) => Some(Self::#orig_ident(#(#field_args),*)),
                }
            }
        });

    Ok(quote! {
        enum #state_ident<#generics_params> {
            Unknown,
            Start,
            #(#sink_variants),*
        }

        struct #sink_ident<#generics_params> {
            state: #state_ident<#generics_params>,
            subsink: Option<Box<dyn ::serser::TokenSink<Error = TokenError>>>,
        }

        impl<#generics_params> ::serser::TokenSink for #sink_ident<#generics_params> #where_clause {
            type Error = TokenError;

            fn yield_token(&mut self, token: Token<'_>) -> Result<bool, Self::Error> {
                if let Some(subsink) = self.subsink.as_mut() {
                    let want_more = subsink.yield_token(token)?;

                    if !want_more {
                        let subsink = self.subsink.take().unwrap();

                        // Empty enums will have only the default arm.
                        #[allow(unreachable_code)]
                        return match self.state {
                            #(#end_state_matches)*

                            _ => unreachable!("FromTokens enum state machine failed"),
                        };
                    }

                    return Ok(true);
                }

                match self.state {
                    #state_ident::Unknown => match token {
                        ::serser::token::Token::Enum(_) => {
                            self.state = #state_ident::Start;
                            Ok(true)
                        }
                        _ => Err(Self::Error::invalid_token(token, Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Enum)))),
                    }
                    #state_ident::Start => match token {
                        ::serser::token::Token::Variant(var @ ::serser::token::EnumVariant::Str(s)) => match s {
                            #(#variant_str_matches)*
                            _ => Err(Self::Error::invalid_variant(var)),
                        }
                        ::serser::token::Token::Variant(var) => Err(Self::Error::invalid_variant(var)),
                        _ => Err(Self::Error::invalid_token(token, Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Variant)))),
                    }

                    #(#variant_state_matches)*

                    _ => unreachable!("FromTokens enum state machine failed"),
                }
            }

            fn expect_tokens(&mut self) -> Option<TokenTypes> {
                match self.state {
                    #state_ident::Unknown => Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Enum)),
                    #state_ident::Start => Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Variant)),
                    #(#expect_matches)*
                    _ => Some(::serser::token::TokenTypes::EMPTY),
                }
            }

            fn into_any(self: Box<Self>) -> Box<dyn std::any::Any> { self as _ }
        }

        impl<#generics_params> ::serser::FromTokenSink for #ident<#generics_params> #where_clause {
            type Sink = #sink_ident;

            fn new_sink() -> Self::Sink {
                Self::Sink {
                    state: #state_ident::Unknown,
                    subsink: None,
                }
            }

            fn from_sink(sink: Self::Sink) -> Option<Self> {
                match sink.state {
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
