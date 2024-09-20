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
//! The name comes from the idea that serialization and
//! deserialization are fundamentally the same thing, just switching
//! perspectives of the source and sink. It is a conversion from one
//! representation to another, through a common data model.
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

use crate::token::*;

pub mod derive {
    pub use serser_derive::IntoTokens;
}

mod error;
pub use error::Error;
pub use error::TokenError;

mod from;
pub use from::FromTokenSink;
pub use from::FromTokens;

mod into;
pub use into::IntoTokens;
pub use into::iter_into_tokens;

pub mod json;

pub mod test;
pub mod token;

mod vec;
pub use vec::TokenVec;

/// A receiver of tokens. This forms the basis for a push-parser. The
/// source side calls [Self::yield_token] for each simple token. For
/// nested structures, [Self::yield_start] and [Self::end] is called
/// to handle them.
///
/// Optionally, [Self::expect_tokens] can be overridden to provide
/// hints of which token types are acceptable to come next.
pub trait TokenSink {
    type Error: Error;
    type Subsink<'c>: TokenSink<Error = Self::Error> + 'c
    where
        Self: 'c;

    /// Returns the sink to yield tokens to until the corresponding
    /// end token. The last token yielded to the subsink must be
    /// [Token::is_end].
    ///
    /// Sinks are encouraged to panic if `!token.is_start()`.
    fn yield_start<'b, 'c>(&mut self, token: Token<'b>) -> Result<Self::Subsink<'c>, Self::Error>
    where
        Self: 'c;

    /// Handles a token from the stream. This may be a start token, if
    /// it's destined for this sink, as opposed to a subsink. If so,
    /// it is the first yielded token. Returns true if it doesn't
    /// expect more tokens.
    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error>;

    /// Called when the subsink from [Self::yield_start] has received
    /// its end token. It can be used to fetch the final value from
    /// `sink`.
    fn end<'b>(&mut self, sink: Self::Subsink<'b>)
    where
        Self: 'b;

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
}

/// Uses [FromTokens] and [IntoTokens] to port from source to sink,
/// using a stream of [Token] as intermediary representation.
pub fn port<T: FromTokens, I: IntoTokens>(into: I) -> Result<T, <T::Sink as TokenSink>::Error> {
    T::from_tokens(into)
}
