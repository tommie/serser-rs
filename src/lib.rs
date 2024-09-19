use crate::error::*;
use crate::token::*;

pub mod error;
pub mod from;
pub mod into;
pub mod meta;
pub mod printsink;
pub mod token;
pub mod vec;

pub trait TokenSink {
    type Error: Error;
    type Subsink<'c>: TokenSink<Error = Self::Error> + 'c where Self: 'c;

    /// Returns the sink to yield tokens to until the corresponding
    /// end token. Sinks are encouraged to panic if `!token.is_start()`.
    ///
    /// The last token yielded to the subsink must be `Token::is_end()`.
    fn yield_start<'b, 'c>(&'c mut self, token: Token<'b>) -> Result<Self::Subsink<'c>, Self::Error> where Self: 'c;

    /// Returns true if it doesn't expect more tokens. Sinks are
    /// encouraged to panic if `token.is_start()`.
    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error>;

    /// Returns the set of possible token types expected next. The
    /// yield_token function may accept or refuse any token; this is
    /// an optional type negotation mechanism. E.g. if `String` isn't
    /// expected, but `U32` is, then the source may choose to do a
    /// conversion. In general, sources should not expect sinks to do
    /// type conversions.
    ///
    /// Returning `None` means the sink has no expectation, and
    /// returning `Some(0)` indicates it wants nothing (i.e. it has
    /// finished.) The provided implementation returns `None`.
    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        None
    }
}
