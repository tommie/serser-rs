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

    // Returns true if it doesn't expect more tokens.
    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error>;

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        None
    }
}
