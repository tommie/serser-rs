use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

mod from;
mod into;

/// Provides the [IntoTokens] trait for a struct or enum. This is used
/// to serialize Rust data structures.
#[proc_macro_derive(IntoTokens)]
pub fn derive_into_tokens(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    match into::into_tokens(&input) {
        Ok(output) => output,
        Err(err) => err.into_compile_error().into(),
    }
}

/// Provides the `FromTokenSink` trait for a struct or enum. This is
/// used to deserialize into Rust data structures.
///
/// The [FromTokens] trait has a blanket implementation that uses
/// `FromTokenSink`.
#[proc_macro_derive(FromTokens)]
pub fn derive_from_tokens(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);

    match from::from_tokens(&input) {
        Ok(output) => output,
        Err(err) => err.into_compile_error().into(),
    }
}
