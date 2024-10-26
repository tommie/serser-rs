#![doc = include_str!("../README.md")]
#![doc(html_playground_url = "https://play.rust-lang.org/")]
//!
//! ## Processing Model
//!
//! This shows a flow where a recursive data structure like `[42]` is being parsed.
//! The [FromTokens] trait is a convenience wrapper around [FromTokenSink].
//! It has a blanket implementation for all [FromTokenSink].
//!
//! The [FromTokenSink] acts as a per-type [TokenSink] factory.
//! It is implemented for most Rust types.
//!
//! The [IntoTokens] trait drives the whole chain.
//! It calls [TokenSink::yield_token] for every token, and may use [TokenSink::expect_tokens] to negotiate data types with the sink.
//! The diagram below only shows one lane per trait, but the calls may go to different implementations.
//! It may recursively call other [IntoTokens].
//!
//! The [TokenSink] is responsible for handling the sinks.
//! This may be writing to a [std::fmt::Write], or writing into a Rust data structure buffer.
//! The [FromTokenSink::from_sink] function knows how to extract the buffered value from a particular [TokenSink] implementation.
//! A sink may use [FromTokenSink] to off-load handling of constituent pieces of data to another sink.
//! It owns the sinks it creates, forwards tokens to them, meaning that [IntoTokens] is oblivious to any subsinks.
//! The [TokenSink::expect_tokens] function can use [FromTokenSink::expect_initial] to figure out what a nested data type requires, without creating a sink.
//!
//! ```text
//!    Trait               Trait              Trait             Trait
//! FromTokens         FromTokenSink       IntoTokens         TokenSink
//! -----------------------------------------------------------------------
//! from_tokens
//!              --> new_sink
//!             <..
//!              ------------------------> into_tokens
//!                                                    ---> yield_token
//!                                                    ---> (expect_tokens)
//!                  (expect_initial) <-------------------
//!                                    ...................>
//!                  new_sink         <-------------------
//!                                    ...................>
//!                  from_sink        <-------------------
//!                                    ...................>
//!                                                    ---> ...
//!                                            |
//!                                            V
//!                                        into_tokens
//!                                                    ---> yield_token
//!                                                    ---> (expect_tokens)
//!                  (expect_initial) <-------------------
//!                                                    ---> ...
//!                                            :
//!                                            V
//!             <........................
//!              --> from_sink
//!             <..
//! ```

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
mod from_tuple;

mod into;
pub use into::iter_into_tokens;
pub use into::IntoTokens;
mod into_tuple;

pub mod json;

/// A module that puts the useful traits into scope.
pub mod prelude {
    pub use crate::from::FromTokenSink as _;
    pub use crate::from::FromTokens as _;
    pub use crate::into::IntoTokens as _;
}

pub mod test;
pub mod token;

mod utf8read;

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
