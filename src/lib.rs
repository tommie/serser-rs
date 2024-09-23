//! serser is a serialization/deserialization framework. It uses a
//! stream of tokens (e.g. [U32](Token::U32), [Seq](Token::Seq), and
//! [EndSeq](Token::EndSeq)) as the intermediate representation.
//!
//! ## Compared To serde
//!
//! The [serde](https://docs.rs/serde/latest/serde/) crate is the
//! defacto standard for serialization, both for JSON and as used in
//! wasm-bindgen for communicating with the host from WebAssembly.
//!
//! This crate aims to improve on the serde design in these ways:
//!
//! * A smaller API, making it easier to write custom (de)serializers.
//!   Only two functions need to be implemented for a sink: one for
//!   simple tokens, and one for tokens that start nested data.
//! * A bufferable intermediate representation that can be shared
//!   between threads, or stored for later use. Primarily lightweight,
//!   reference-based, [Token] objects are used. An [OwningToken] is
//!   provided for easy storage and IPC.
//! * A push-based pipeline, inverting the [Iterator] pattern to allow
//!   correct lifetimes to be expressed more easily. Tokens only live
//!   during the [yield_token](TokenSink::yield_token) invocation; if
//!   the callee needs to store it, it has to make a copy. This is
//!   similar to serde, but avoids the ping-pong between
//!   [Deserializer](https://docs.rs/serde/latest/serde/de/trait.Deserializer.html),
//!   [\*Access](https://docs.rs/serde/latest/serde/de/trait.SeqAccess.html)
//!   and
//!   [Visitor](https://docs.rs/serde/latest/serde/de/trait.Visitor.html).
//! * No reliance on `'static` for metadata.
//!
//! The aim is for the implementation to be on a par with serde in
//! terms of performance.
//!
//! The name serser comes from the idea that serialization and
//! deserialization are fundamentally the same thing, just switching
//! perspectives of the source and sink. It is a conversion from one
//! representation to another, through a common data model.
#![doc = include_str!("../README.md")]
#![doc(html_playground_url = "https://play.rust-lang.org/")]

use crate::token::*;

/// The derive macro that automatically implements the [IntoTokens]
/// for your structs and enums.
pub mod derive {
    pub use serser_derive::FromTokens;
    pub use serser_derive::IntoTokens;
}

mod error;
pub use error::Error;
pub use error::TokenError;

mod from;
pub use from::FromTokenSink;
pub use from::FromTokens;

mod into;
pub use into::iter_into_tokens;
pub use into::IntoTokens;

pub mod json;

/// A module that puts the useful traits into scope.
pub mod prelude {
    pub use crate::from::FromTokenSink as _;
    pub use crate::from::FromTokens as _;
    pub use crate::into::IntoTokens as _;
}

pub mod test;
pub mod token;

mod vec;
pub use vec::TokenVec;

/// A receiver of tokens. This forms the basis for a push-parser. The
/// source side calls [Self::yield_token] for each simple
/// token. Nested structures are handled by the sink, i.e. any subsink
/// required to parse a data structure is owned by the parent sink,
/// which forwards tokens.
///
/// Optionally, [Self::expect_tokens] can be overridden to provide
/// hints of which token types are acceptable to come next.
pub trait TokenSink {
    type Error: Error;

    /// Handles the next token from the stream. Returns true if the
    /// sink requires more tokens.
    fn yield_token(&mut self, token: Token<'_>) -> Result<bool, Self::Error>;

    /// Returns the set of token types expected next. The
    /// [Self::yield_token] function may accept or refuse any token;
    /// this is an optional type negotation mechanism. E.g. if
    /// `String` isn't expected, but `U32` is, then the source may
    /// choose to do a conversion. In general, sources should not
    /// expect sinks to do type conversions.
    ///
    /// Returning `None` means the sink has no expectation, and
    /// returning `Some(0)` indicates it wants nothing (i.e. it has
    /// finished.) The provided implementation returns `None`.
    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        None
    }

    /// Converts the boxed sink into a boxed Any. This is used in the
    /// FromTokens derive macro to support dynamic dispatch for fields
    /// and variants. It only needs to be overridden for types where
    /// [Self::yield_token] returns true.
    ///
    /// Because `Any: 'static`, implementations using generics will
    /// have to require that all generic types are `'static`. This
    /// isn't a problem for sinks for normal types, but intercepting
    /// sinks, like [test::PrintingTokenSink] will be difficult to use
    /// with dynamic dispatching (i.e. from generated code.)
    fn into_any(self: Box<Self>) -> Box<dyn std::any::Any> {
        unimplemented!("into_any is not implemented for this type")
    }
}

/// Uses [FromTokens] and [IntoTokens] to port from source to sink,
/// using a stream of [Token] as intermediary representation.
pub fn port<T: FromTokens, I: IntoTokens>(into: I) -> Result<T, <T::Sink as TokenSink>::Error> {
    T::from_tokens(into)
}
