extern crate proc_macro;
use proc_macro::TokenStream;

use quote::format_ident;
use quote::quote;
use syn::{DeriveInput, Error};

pub(crate) fn from_tokens(input: &DeriveInput) -> Result<TokenStream, Error> {
    match &input.data {
        syn::Data::Struct(data) => from_tokens_struct(data, input),
        syn::Data::Enum(data) => from_tokens_enum(data, input),
        syn::Data::Union(_) => Err(Error::new_spanned(
            input,
            "union is not supported by FromTokens",
        )),
    }
}

fn from_tokens_struct(data: &syn::DataStruct, input: &DeriveInput) -> Result<TokenStream, Error> {
    match &data.fields {
        syn::Fields::Named(fields) => from_tokens_struct_named(fields, input),
        syn::Fields::Unnamed(fields) => from_tokens_struct_unnamed(fields, input),
        syn::Fields::Unit => from_tokens_struct_unnamed(
            &syn::FieldsUnnamed {
                paren_token: syn::token::Paren {
                    span: proc_macro2::Group::new(
                        proc_macro2::Delimiter::None,
                        proc_macro2::TokenStream::new(),
                    )
                    .delim_span(),
                },
                unnamed: syn::punctuated::Punctuated::new(),
            },
            input,
        ),
    }
}

fn from_tokens_struct_named(
    fields: &syn::FieldsNamed,
    input: &DeriveInput,
) -> Result<TokenStream, Error> {
    let generics_params = &input.generics.params;
    let where_clause = &input.generics.where_clause;
    let ident = &input.ident;
    let sink_ident = format_ident!("{}SerSerTokenSink", ident);
    let sink_fields = fields
        .named
        .iter()
        .enumerate()
        .map(|(i, field)| {
            let ident = format_ident!("f{}", i);
            let ty = &field.ty;

            quote! { #ident: Option<#ty> }
        })
        .collect::<Vec<_>>();
    let end_state_matches = fields
            .named
            .iter()
            .enumerate()
            .map(|(i, field)| {
                let ident = format_ident!("f{}", i);
                let ty = &field.ty;
                let ii = i as isize;

                quote! {
                    #ii => {
                        // We created the subsink when we set field = i, so it should be valid.
                        let cast_subsink = subsink.into_any().downcast::<<#ty as ::serser::FromTokenSink>::Sink>().unwrap();
                        if let Some(v) = <#ty as ::serser::FromTokenSink>::from_sink(*cast_subsink) {
                            self.#ident = Some(v);
                            self.field_mask &= !(1 << #i);
                            self.state = -2;
                            Ok(true)
                        } else {
                            unreachable!("subsink yield_token signaled end, but no value is available")
                        }
                    }
                }
            })
            .collect::<Vec<_>>();
    let field_str_matches = fields.named.iter().enumerate().map(|(i, field)| {
        let name_str = syn::LitStr::new(
            format!("{}", field.ident.as_ref().unwrap()).as_str(),
            proc_macro2::Span::call_site(),
        );

        quote! {
            #name_str => {
                self.state = #i as isize;
                Ok(true)
            }
        }
    });
    let field_missing_checks = fields.named.iter().enumerate().map(|(i, field)| {
        let name_str = syn::LitStr::new(
            format!("{}", field.ident.as_ref().unwrap()).as_str(),
            proc_macro2::Span::call_site(),
        );

        quote! {
            if self.field_mask & (1 << #i) != 0 {
                missing_fields.push(#name_str)
            }
        }
    });
    let field_state_matches = fields.named.iter().enumerate().map(|(i, field)| {
        let ident = format_ident!("f{}", i);
        let ty = &field.ty;
        let ii = i as isize;

        quote! {
            #ii => {
                let mut subsink = <#ty as ::serser::FromTokenSink>::new_sink();

                if subsink.yield_token(token)? {
                    self.subsink = Some(Box::new(subsink));
                } else if let Some(v) = <#ty as ::serser::FromTokenSink>::from_sink(subsink) {
                    self.#ident = Some(v);
                    self.field_mask &= !(1 << #i);
                    self.state = -2;
                } else {
                    unreachable!("subsink yield_token signaled end, but no value is available");
                }
                Ok(true)
            }
        }
    });
    let expect_matches = fields.named.iter().enumerate().map(|(i, field)| {
        let ty = &field.ty;
        let ii = i as isize;

        quote! {
            #ii => <#ty as ::serser::FromTokenSink>::expect_initial(),
        }
    });
    let field_inits = fields.named.iter().enumerate().map(|(i, _)| {
        let ident = format_ident!("f{}", i);

        quote! { #ident: None }
    });
    let num_fields = field_inits.len();
    let from_sink_fields = fields.named.iter().enumerate().map(|(i, field)| {
        let orig_ident = &field.ident;
        let ident = format_ident!("f{}", i);

        quote! { #orig_ident: sink.#ident.unwrap() }
    });

    Ok(quote! {
        struct #sink_ident<#generics_params> {
            // Remaining fields.
            field_mask: usize,
            // -3 waits for Struct, -2 waits for Field, -1 means EndStruct seen.
            state: isize,
            subsink: Option<Box<dyn ::serser::TokenSink<Error = ::serser::TokenError>>>,
            #(#sink_fields),*
        }

        impl<#generics_params> ::serser::TokenSink for #sink_ident<#generics_params> #where_clause {
            type Error = ::serser::TokenError;

            fn yield_token(&mut self, token: ::serser::token::Token<'_>) -> Result<bool, Self::Error> {
                if let Some(subsink) = self.subsink.as_mut() {
                    let want_more = subsink.yield_token(token)?;

                    if !want_more {
                        let subsink = self.subsink.take().unwrap();

                        // Empty structs will have only the default arm.
                        #[allow(unreachable_code)]
                        return match self.state {
                            #(#end_state_matches)*

                            _ => unreachable!("FromTokens struct state machine failed"),
                        };
                    }

                    return Ok(true);
                }

                match self.state {
                    -3 => match token {
                        ::serser::token::Token::Struct(_) => {
                            self.state = -2;
                            Ok(true)
                        }
                        _ => Err(<Self::Error as ::serser::Error>::invalid_token(token, Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Struct)))),
                    }
                    -2 => match token {
                        ::serser::token::Token::Field(s) => match s {
                            #(#field_str_matches)*
                            _ => Err(<Self::Error as ::serser::Error>::invalid_field(s)),
                        }
                        ::serser::token::Token::EndStruct => if self.field_mask == 0 {
                            self.state = -1;
                            Ok(false)
                        } else {
                            let mut missing_fields = Vec::with_capacity(#num_fields);

                            #(#field_missing_checks)*

                            Err(<Self::Error as ::serser::Error>::missing_fields(missing_fields.as_slice()))
                        }
                        _ => Err(<Self::Error as ::serser::Error>::invalid_token(token, Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Field)))),
                    }

                    #(#field_state_matches)*

                    _ => Err(<Self::Error as ::serser::Error>::invalid_token(token, Some(::serser::token::TokenTypes::EMPTY))),
                }
            }

            fn expect_tokens(&mut self) -> Option<::serser::token::TokenTypes> {
                match self.state {
                    -3 => Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Struct)),
                    -2 => if self.field_mask != 0 {
                        Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Field).with(::serser::token::TokenType::EndStruct))
                    } else {
                        Some(::serser::token::TokenTypes::new(::serser::token::TokenType::EndStruct))
                    }

                    #(#expect_matches)*

                    _ => if self.field_mask != 0 {
                        Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Field))
                    } else {
                        Some(::serser::token::TokenTypes::EMPTY)
                    }
                }
            }

            fn into_any(self: Box<Self>) -> Box<dyn std::any::Any> { self as _ }
        }

        impl<#generics_params> ::serser::FromTokenSink for #ident<#generics_params> #where_clause {
            type Sink = #sink_ident;

            fn new_sink() -> Self::Sink {
                Self::Sink {
                    field_mask: (1 << #num_fields) - 1,
                    state: -3,
                    subsink: None,
                    #(#field_inits),*
                }
            }

            fn from_sink(sink: Self::Sink) -> Option<Self> {
                if sink.field_mask == 0 {
                    Some(Self {
                        #(#from_sink_fields),*
                    })
                } else {
                    None
                }
            }

            fn expect_initial() -> Option<::serser::token::TokenTypes> {
                Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Enum))
            }
        }
    }
    .into())
}

fn from_tokens_struct_unnamed(
    fields: &syn::FieldsUnnamed,
    input: &DeriveInput,
) -> Result<TokenStream, Error> {
    let generics_params = &input.generics.params;
    let where_clause = &input.generics.where_clause;
    let ident = &input.ident;
    let sink_ident = format_ident!("{}SerSerTokenSink", ident);
    let sink_fields = fields
        .unnamed
        .iter()
        .enumerate()
        .map(|(i, field)| {
            let ident = format_ident!("f{}", i);
            let ty = &field.ty;

            quote! { #ident: Option<#ty> }
        })
        .collect::<Vec<_>>();
    let end_state_matches = fields
            .unnamed
            .iter()
            .enumerate()
            .map(|(i, field)| {
                let ident = format_ident!("f{}", i);
                let ty = &field.ty;
                let ii = i as isize;

                quote! {
                    #ii => {
                        // We created the subsink when we set field = i, so it should be valid.
                        let cast_subsink = subsink.into_any().downcast::<<#ty as ::serser::FromTokenSink>::Sink>().unwrap();
                        if let Some(v) = <#ty as ::serser::FromTokenSink>::from_sink(*cast_subsink) {
                            self.#ident = Some(v);
                            self.state += 1;
                            Ok(true)
                        } else {
                            unreachable!("subsink yield_token signaled end, but no value is available")
                        }
                    }
                }
            })
            .collect::<Vec<_>>();
    let field_state_matches = fields.unnamed.iter().enumerate().map(|(i, field)| {
        let ident = format_ident!("f{}", i);
        let ty = &field.ty;
        let ii = i as isize;

        quote! {
            #ii => {
                let mut subsink = <#ty as ::serser::FromTokenSink>::new_sink();

                if subsink.yield_token(token)? {
                    self.subsink = Some(Box::new(subsink));
                } else if let Some(v) = <#ty as ::serser::FromTokenSink>::from_sink(subsink) {
                    self.#ident = Some(v);
                    self.state += 1;
                } else {
                    unreachable!("subsink yield_token signaled end, but no value is available");
                }
                Ok(true)
            }
        }
    });
    let expect_matches = fields.unnamed.iter().enumerate().map(|(i, field)| {
        let ty = &field.ty;
        let ii = i as isize;

        quote! {
            #ii => <#ty as ::serser::FromTokenSink>::expect_initial(),
        }
    });
    let field_inits = fields.unnamed.iter().enumerate().map(|(i, _)| {
        let ident = format_ident!("f{}", i);

        quote! { #ident: None }
    });
    let num_fields = field_inits.len();
    let inum_fields = num_fields as isize;
    let from_sink_fields = fields.unnamed.iter().enumerate().map(|(i, _)| {
        let ident = format_ident!("f{}", i);

        quote! { sink.#ident.unwrap() }
    });

    Ok(quote! {
        struct #sink_ident<#generics_params> {
            // -2 waits for Tuple, -1 is after EndTuple.
            state: isize,
            subsink: Option<Box<dyn ::serser::TokenSink<Error = ::serser::TokenError>>>,
            #(#sink_fields),*
        }

        impl<#generics_params> ::serser::TokenSink for #sink_ident<#generics_params> #where_clause {
            type Error = ::serser::TokenError;

            fn yield_token(&mut self, token: ::serser::token::Token<'_>) -> Result<bool, Self::Error> {
                if let Some(subsink) = self.subsink.as_mut() {
                    let want_more = subsink.yield_token(token)?;

                    if !want_more {
                        let subsink = self.subsink.take().unwrap();

                        // Empty structs will have only the default arm.
                        #[allow(unreachable_code)]
                        return match self.state {
                            #(#end_state_matches)*

                            _ => unreachable!("FromTokens struct state machine failed"),
                        };
                    }

                    return Ok(true);
                }

                match self.state {
                    -2 => match token {
                        ::serser::token::Token::Tuple(_) => {
                            self.state = 0;
                            Ok(true)
                        }
                        _ => Err(<Self::Error as ::serser::Error>::invalid_token(token, Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Tuple)))),
                    }

                    #(#field_state_matches)*

                    #inum_fields => {
                        self.state = -1;
                        Ok(false)
                    }
                    _ => Err(<Self::Error as ::serser::Error>::invalid_token(token, Some(::serser::token::TokenTypes::EMPTY))),
                }
            }

            fn expect_tokens(&mut self) -> Option<::serser::token::TokenTypes> {
                match self.state {
                    -2 => Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Tuple)),

                    #(#expect_matches)*

                    #inum_fields => Some(::serser::token::TokenTypes::new(::serser::token::TokenType::EndTuple)),
                    _ => Some(::serser::token::TokenTypes::EMPTY),
                }
            }

            fn into_any(self: Box<Self>) -> Box<dyn std::any::Any> { self as _ }
        }

        impl<#generics_params> ::serser::FromTokenSink for #ident<#generics_params> #where_clause {
            type Sink = #sink_ident;

            fn new_sink() -> Self::Sink {
                Self::Sink {
                    state: -2,
                    subsink: None,
                    #(#field_inits),*
                }
            }

            fn from_sink(sink: Self::Sink) -> Option<Self> {
                if sink.state == -1 {
                    Some(Self(#(#from_sink_fields),*))
                } else {
                    None
                }
            }

            fn expect_initial() -> Option<::serser::token::TokenTypes> {
                Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Enum))
            }
        }
    }
    .into())
}

fn from_tokens_enum(data: &syn::DataEnum, input: &DeriveInput) -> Result<TokenStream, Error> {
    let generics_params = &input.generics.params;
    let where_clause = &input.generics.where_clause;
    let ident = &input.ident;
    let state_ident = format_ident!("{}SerSerTokenSinkState", ident);
    let sink_ident = format_ident!("{}SerSerTokenSink", ident);
    let sink_variants = data.variants.iter().enumerate().map(|(i, variant)| {
        let ident = format_ident!("V{}", i);
        match &variant.fields {
            syn::Fields::Unnamed(fields) => {
                let fields = fields
                    .unnamed
                    .iter()
                    .map(|field| {
                        let ty = &field.ty;

                        quote! { Option<#ty> }
                    })
                    .collect::<Vec<_>>();

                // The first value is the current field number.
                // -1 means waiting for Tuple/Struct.
                // num_fields means waiting for EndTuple.
                // num_fields+1 means waiting for EndEnum.
                // num_fields+2 means complete.
                quote! { #ident (isize, #(#fields),*) }
            }
            syn::Fields::Named(fields) => {
                let fields = fields
                    .named
                    .iter()
                    .map(|field| {
                        let ty = &field.ty;

                        quote! { Option<#ty> }
                    })
                    .collect::<Vec<_>>();

                // The first value is the current field number.
                // -2 means waiting for Field. See also above.
                // The second is a mask of required fields remaining.
                quote! { #ident (isize, usize, #(#fields),*) }
            }
            syn::Fields::Unit => {
                // The first value is the current field number. See above.
                quote! { #ident (isize) }
            }
        }
    });
    let variant_str_matches = data.variants.iter().enumerate().map(|(i, variant)| {
        let ident = format_ident!("V{}", i);
        let name_str = syn::LitStr::new(
            format!("{}", variant.ident).as_str(),
            proc_macro2::Span::call_site(),
        );
        let fields = variant
            .fields
            .iter()
            .map(|_| quote! { None })
            .collect::<Vec<_>>();

        match &variant.fields {
            syn::Fields::Unnamed(_) => quote! {
                #name_str => {
                    self.state = #state_ident::#ident(-1, #(#fields),*);
                    Ok(true)
                }
            },
            syn::Fields::Named(_) => {
                let mask: usize = (1 << fields.len()) - 1;

                quote! {
                    #name_str => {
                        self.state = #state_ident::#ident(-1, #mask, #(#fields),*);
                        Ok(true)
                    }
                }
            }
            syn::Fields::Unit => quote! {
                #name_str => {
                    self.state = #state_ident::#ident(-1, #(#fields),*);
                    Ok(true)
                }
            },
        }
    });
    let end_state_matches = data
        .variants
        .iter()
        .enumerate()
        .map(|(i, variant)| {
            let ident = format_ident!("V{}", i);
            let field_params = get_field_tuple_params(&variant.fields);
            let field_matches = match &variant.fields {
                syn::Fields::Unnamed(fields) => fields
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(j, field)| {
                        let fident = format_ident!("f{}", j);
                        let ty = &field.ty;
                        let jj = j as isize;

                        quote! {
                            #state_ident::#ident(ref mut i @ #jj, #(#field_params),*) => {
                                // We created the subsink when we set i = #jj, so it should be valid.
                                let cast_subsink = subsink.into_any().downcast::<<#ty as ::serser::FromTokenSink>::Sink>().unwrap();
                                if let Some(v) = <#ty as ::serser::FromTokenSink>::from_sink(*cast_subsink) {
                                    *#fident = Some(v);
                                    *i += 1;
                                    Ok(true)
                                } else {
                                    unreachable!("subsink yield_token signaled end, but no value is available")
                                }
                            }
                        }
                    })
                    .collect::<Vec<_>>(),
                syn::Fields::Named(fields) => fields
                    .named
                    .iter()
                    .enumerate()
                    .map(|(j, field)| {
                        let fident = format_ident!("f{}", j);
                        let ty = &field.ty;
                        let jj = j as isize;

                        quote! {
                            #state_ident::#ident(ref mut i @ #jj, ref mut mask, #(#field_params),*) => {
                                // We created the subsink when we set i = #jj, so it should be valid.
                                let cast_subsink = subsink.into_any().downcast::<<#ty as ::serser::FromTokenSink>::Sink>().unwrap();
                                if let Some(v) = <#ty as ::serser::FromTokenSink>::from_sink(*cast_subsink) {
                                    *#fident = Some(v);
                                    *mask &= !(1 << #j);
                                    *i = -2;
                                    Ok(true)
                                } else {
                                    unreachable!("subsink yield_token signaled end, but no value is available")
                                }
                            }
                        }
                    })
                    .collect::<Vec<_>>(),
                syn::Fields::Unit => Vec::new(),
            };

            quote! { #(#field_matches)* }
        });
    let variant_state_matches = data
        .variants
        .iter()
        .enumerate()
        .map(|(i, variant)| {
            let ident = format_ident!("V{}", i);
            let field_params = get_field_tuple_params(&variant.fields);
            let field_matches = match &variant.fields {
                syn::Fields::Unnamed(fields) => fields
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(j, field)| {
                        let fident = format_ident!("f{}", j);
                        let ty = &field.ty;
                        let jj = j as isize;

                        quote! {
                            #state_ident::#ident(ref mut i @ #jj, #(#field_params),*) => {
                                let mut subsink = <#ty as ::serser::FromTokenSink>::new_sink();

                                if subsink.yield_token(token)? {
                                    self.subsink = Some(Box::new(subsink));
                                } else if let Some(v) = <#ty as ::serser::FromTokenSink>::from_sink(subsink) {
                                    *#fident = Some(v);
                                    *i += 1;
                                } else {
                                    unreachable!("subsink yield_token signaled end, but no value is available");
                                }
                                Ok(true)
                            }
                        }
                    })
                    .collect::<Vec<_>>(),
                syn::Fields::Named(fields) => fields
                    .named
                    .iter()
                    .enumerate()
                    .map(|(j, field)| {
                        let fident = format_ident!("f{}", j);
                        let ty = &field.ty;
                        let jj = j as isize;

                        quote! {
                            #state_ident::#ident(ref mut i @ #jj, ref mut mask, #(#field_params),*) => {
                                let mut subsink = <#ty as ::serser::FromTokenSink>::new_sink();

                                if subsink.yield_token(token)? {
                                    self.subsink = Some(Box::new(subsink));
                                } else if let Some(v) = <#ty as ::serser::FromTokenSink>::from_sink(subsink) {
                                    *#fident = Some(v);
                                    *mask &= !(1 << #j);
                                    *i = -2;
                                } else {
                                    unreachable!("subsink yield_token signaled end, but no value is available");
                                }
                                Ok(true)
                            }
                        }
                    })
                    .collect::<Vec<_>>(),
                syn::Fields::Unit => Vec::new(),
            };
            let field_placeholders = &variant.fields
                    .iter()
                    .map(|_| quote! { _ })
                    .collect::<Vec<_>>();
            let num_fields = field_placeholders.len() as isize;
            let end_tuple = num_fields + 1;

            match &variant.fields {
                syn::Fields::Unnamed(_) => quote! {
                    #state_ident::#ident(ref mut i @ -1, #(#field_placeholders),*) => match token {
                        ::serser::token::Token::Tuple(_) => {
                            *i = 0;
                            Ok(true)
                        }
                        ::serser::token::Token::EndEnum if #num_fields == 0 => {
                            *i = 2;
                            Ok(false)
                        }
                        t => Err(<Self::Error as ::serser::Error>::invalid_token(t, Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Tuple)))),
                    }

                    #(#field_matches)*

                    #state_ident::#ident(ref mut i @ #num_fields, #(#field_placeholders),*) => match token {
                        ::serser::token::Token::EndTuple => {
                            *i += 1;
                            Ok(true)
                        }
                        t => Err(<Self::Error as ::serser::Error>::invalid_token(t, Some(::serser::token::TokenTypes::new(::serser::token::TokenType::EndTuple)))),
                    }
                    #state_ident::#ident(ref mut i @ #end_tuple, #(#field_placeholders),*) => match token {
                        ::serser::token::Token::EndEnum => {
                            *i += 1;
                            Ok(false)
                        }
                        t => Err(<Self::Error as ::serser::Error>::invalid_token(t, Some(::serser::token::TokenTypes::new(::serser::token::TokenType::EndEnum)))),
                    }
                },
                syn::Fields::Named(fields) => {
                    let field_name_matches = fields.named.iter().enumerate().map(|(j, field)| {
                        let name_str = format!("{}", field.ident.as_ref().unwrap());
                        let jj = j as isize;

                        quote! {
                            #name_str => {
                                *i = #jj;
                                Ok(true)
                            }
                        }
                    });

                    quote! {
                        #state_ident::#ident(ref mut i @ -1, _, #(#field_placeholders),*) => match token {
                            ::serser::token::Token::Struct(_) => {
                                *i = -2;
                                Ok(true)
                            }
                            ::serser::token::Token::EndEnum if #num_fields == 0 => {
                                *i = 2;
                                Ok(false)
                            }
                            t => Err(<Self::Error as ::serser::Error>::invalid_token(t, Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Struct)))),
                        }
                        #state_ident::#ident(ref mut i @ -2, _, #(#field_params),*) => match token {
                            ::serser::token::Token::Field(s) => match s {
                                #(#field_name_matches)*
                                s => Err(<Self::Error as ::serser::Error>::invalid_field(s)),
                            }
                            ::serser::token::Token::EndStruct => {
                                *i = #num_fields + 1;
                                Ok(true)
                            }
                            t => Err(<Self::Error as ::serser::Error>::invalid_token(t, Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Field).with(::serser::token::TokenType::EndStruct)))),
                        }

                        #(#field_matches)*

                        #state_ident::#ident(ref mut i @ #num_fields, _, #(#field_placeholders),*) => match token {
                            ::serser::token::Token::EndStruct => {
                                *i += 1;
                                Ok(true)
                            }
                            t => Err(<Self::Error as ::serser::Error>::invalid_token(t, Some(::serser::token::TokenTypes::new(::serser::token::TokenType::EndStruct)))),
                        }
                        #state_ident::#ident(ref mut i @ #end_tuple, _, #(#field_placeholders),*) => match token {
                            ::serser::token::Token::EndEnum => {
                                *i += 1;
                                Ok(false)
                            }
                            t => Err(<Self::Error as ::serser::Error>::invalid_token(t, Some(::serser::token::TokenTypes::new(::serser::token::TokenType::EndEnum)))),
                        }
                    }
                },
                syn::Fields::Unit => quote! {
                    #state_ident::#ident(ref mut i @ -1, #(#field_placeholders),*) => match token {
                        ::serser::token::Token::Tuple(_) => {
                            *i = 0;
                            Ok(true)
                        }
                        ::serser::token::Token::EndEnum if #num_fields == 0 => {
                            *i = 2;
                            Ok(false)
                        }
                        t => Err(<Self::Error as ::serser::Error>::invalid_token(t, Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Tuple)))),
                    }

                    #state_ident::#ident(ref mut i @ 0, #(#field_placeholders),*) => match token {
                        ::serser::token::Token::EndTuple => {
                            *i += 1;
                            Ok(true)
                        }
                        t => Err(<Self::Error as ::serser::Error>::invalid_token(t, Some(::serser::token::TokenTypes::new(::serser::token::TokenType::EndTuple)))),
                    }
                    #state_ident::#ident(ref mut i @ 1, #(#field_placeholders),*) => match token {
                        ::serser::token::Token::EndEnum => {
                            *i += 1;
                            Ok(false)
                        }
                        t => Err(<Self::Error as ::serser::Error>::invalid_token(t, Some(::serser::token::TokenTypes::new(::serser::token::TokenType::EndEnum)))),
                    }
                },
            }
        });
    let expect_matches = data
        .variants
        .iter()
        .enumerate()
        .map(|(i, variant)| {
            let ident = format_ident!("V{}", i);
            let field_placeholders = match &variant.fields {
                syn::Fields::Unnamed(fields) => fields
                    .unnamed
                    .iter()
                    .map(|_| quote! { _ })
                    .collect::<Vec<_>>(),
                syn::Fields::Named(fields) => fields
                    .named
                    .iter()
                    .map(|_| quote! { _ })
                    .collect::<Vec<_>>(),
                syn::Fields::Unit => Vec::new(),
            };
            let field_matches = match &variant.fields {
                syn::Fields::Unnamed(fields) => fields
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(j, field)| {
                        let ty = &field.ty;
                        let jj = j as isize;

                        quote! {
                            #state_ident::#ident(#jj, #(#field_placeholders),*) => <#ty as ::serser::FromTokenSink>::expect_initial(),
                        }
                    })
                    .collect::<Vec<_>>(),
                syn::Fields::Named(fields) => fields
                    .named
                    .iter()
                    .enumerate()
                    .map(|(j, field)| {
                        let ty = &field.ty;
                        let jj = j as isize;

                        quote! {
                            #state_ident::#ident(#jj, _, #(#field_placeholders),*) => <#ty as ::serser::FromTokenSink>::expect_initial(),
                        }
                    })
                    .collect::<Vec<_>>(),
                syn::Fields::Unit => Vec::new(),
            };
            let num_fields = field_placeholders.len() as isize;
            let end_tuple = num_fields + 1;
            let end_enum = num_fields + 2;

            // End-of-fields is handled by the common _ match.
            match &variant.fields {
                syn::Fields::Unnamed(_) => quote! {
                    #(#field_matches)*

                    #state_ident::#ident(#end_tuple, #(#field_placeholders),*) => Some(::serser::token::TokenTypes::new(::serser::token::TokenType::EndTuple)),
                    #state_ident::#ident(#end_enum, #(#field_placeholders),*) => Some(::serser::token::TokenTypes::new(::serser::token::TokenType::EndEnum)),
                },
                syn::Fields::Named(_) => quote! {
                    #(#field_matches)*

                    #state_ident::#ident(#end_tuple, _, #(#field_placeholders),*) => Some(::serser::token::TokenTypes::new(::serser::token::TokenType::EndTuple)),
                    #state_ident::#ident(#end_enum, _, #(#field_placeholders),*) => Some(::serser::token::TokenTypes::new(::serser::token::TokenType::EndEnum)),
                },
                syn::Fields::Unit => quote! {
                    #state_ident::#ident(#end_tuple, #(#field_placeholders),*) => Some(::serser::token::TokenTypes::new(::serser::token::TokenType::EndTuple)),
                    #state_ident::#ident(#end_enum, #(#field_placeholders),*) => Some(::serser::token::TokenTypes::new(::serser::token::TokenType::EndEnum)),
                },
            }
        });
    let variant_from_sink_matches = data
        .variants
        .iter()
        .enumerate()
        .map(|(i, variant)| {
            let orig_ident = &variant.ident;
            let ident = format_ident!("V{}", i);
            let field_idents = match &variant.fields {
                syn::Fields::Unnamed(fields) => fields
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(j, _)| format_ident!("f{}", j))
                    .collect::<Vec<_>>(),
                syn::Fields::Named(fields) => fields
                    .named
                    .iter()
                    .enumerate()
                    .map(|(j, _)| format_ident!("f{}", j))
                    .collect::<Vec<_>>(),
                syn::Fields::Unit => Vec::new(),
            };
            let field_params = field_idents.iter().map(|ident| quote! { Some(#ident) }).collect::<Vec<_>>();
            let field_args = match &variant.fields {
                syn::Fields::Unnamed(_) => field_idents.iter().map(|ident| quote! { #ident }).collect::<Vec<_>>(),
                syn::Fields::Named(fields) => fields
                    .named
                    .iter()
                    .enumerate()
                    .map(|(j, field)| {
                        let orig_ident = &field.ident;
                        let ident = format_ident!("f{}", j);

                        quote! { #orig_ident: #ident }
                    })
                    .collect::<Vec<_>>(),
                syn::Fields::Unit => Vec::new(),
            };
            let num_fields = field_params.len() as isize;
            let end_enum = num_fields + 2;

            match &variant.fields {
                syn::Fields::Unnamed(_) => if num_fields == 0 {
                    quote! {
                        #state_ident::#ident(#end_enum) => Some(Self::#orig_ident),
                    }
                } else {
                    quote! {
                        #state_ident::#ident(#end_enum, #(#field_params),*) => Some(Self::#orig_ident(#(#field_args),*)),
                    }
                }
                syn::Fields::Named(_) => quote! {
                    #state_ident::#ident(#end_enum, _, #(#field_params),*) => Some(Self::#orig_ident { #(#field_args),* }),
                },
                syn::Fields::Unit => quote! {
                    #state_ident::#ident(#end_enum) => Some(Self::#orig_ident),
                }
            }
        });

    Ok(quote! {
        enum #state_ident<#generics_params> {
            Unknown,
            Start,
            Kind,
            #(#sink_variants),*
        }

        struct #sink_ident<#generics_params> {
            state: #state_ident<#generics_params>,
            subsink: Option<Box<dyn ::serser::TokenSink<Error = ::serser::TokenError>>>,
        }

        impl<#generics_params> ::serser::TokenSink for #sink_ident<#generics_params> #where_clause {
            type Error = ::serser::TokenError;

            fn yield_token(&mut self, token: ::serser::token::Token<'_>) -> Result<bool, Self::Error> {
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
                        _ => Err(<Self::Error as ::serser::Error>::invalid_token(token, Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Enum)))),
                    }
                    #state_ident::Start => match token {
                        ::serser::token::Token::Variant(var @ ::serser::token::EnumVariant::Str(s)) => match s {
                            #(#variant_str_matches)*
                            _ => Err(<Self::Error as ::serser::Error>::invalid_variant(var)),
                        }
                        ::serser::token::Token::Variant(var) => Err(<Self::Error as ::serser::Error>::invalid_variant(var)),
                        _ => Err(<Self::Error as ::serser::Error>::invalid_token(token, Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Variant)))),
                    }

                    #(#variant_state_matches)*

                    _ => unreachable!("FromTokens enum state machine failed"),
                }
            }

            fn expect_tokens(&mut self) -> Option<::serser::token::TokenTypes> {
                match self.state {
                    #state_ident::Unknown => Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Enum)),
                    #state_ident::Start => Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Variant)),
                    #state_ident::Kind => Some(::serser::token::TokenTypes::new(::serser::token::TokenType::Tuple)),
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

fn get_field_tuple_params(fields: &syn::Fields) -> Vec<proc_macro2::TokenStream> {
    match fields {
        syn::Fields::Unnamed(fields) => fields
            .unnamed
            .iter()
            .enumerate()
            .map(|(j, _)| {
                let ident = format_ident!("f{}", j);

                quote! { ref mut #ident }
            })
            .collect::<Vec<_>>(),
        syn::Fields::Named(fields) => fields
            .named
            .iter()
            .enumerate()
            .map(|(j, _)| {
                let ident = format_ident!("f{}", j);

                quote! { ref mut #ident }
            })
            .collect::<Vec<_>>(),
        syn::Fields::Unit => Vec::new(),
    }
}
