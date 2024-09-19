use std::error;
use std::fmt;

use crate::token::*;

/// Errors returned from [TokenSink](super::TokenSink) must implement this trait.
///
/// It allows the source and intermediary code to generate internal
/// errors, while giving the sink implementor full control over the
/// error type.
pub trait Error: error::Error {
    /// The token sink received a token it cannot process.
    fn invalid_token(token: Token<'_>, expected: Option<TokenTypes>) -> Self;

    /// The token sink is missing tokens, and the value reconstruction
    /// is not complete.
    fn unexpected_end(expected: Option<TokenTypes>) -> Self;
}

/// A simple [Error] implementation for when nothing special is needed.
#[derive(Debug, Eq, PartialEq)]
pub enum TokenError {
    InvalidToken(OwningToken, Option<TokenTypes>),
    UnexpectedEnd(Option<TokenTypes>),
}

impl Error for TokenError {
    fn invalid_token(token: Token<'_>, expected: Option<TokenTypes>) -> Self {
        TokenError::InvalidToken(token.into(), expected)
    }

    fn unexpected_end(expected: Option<TokenTypes>) -> Self {
        TokenError::UnexpectedEnd(expected)
    }
}

impl fmt::Display for TokenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::InvalidToken(_, _) => write!(f, "invalid token"),
            Self::UnexpectedEnd(_) => write!(f, "unexpected end"),
        }
    }
}

impl std::error::Error for TokenError {}
