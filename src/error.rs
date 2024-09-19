use std::error;
use std::fmt;

use crate::token::*;

pub trait Error: error::Error {
    /// The token sink received a token it cannot process.
    fn invalid_token(token: Token<'_>, expected: Option<TokenTypes>) -> Self;

    /// The token sink is missing tokens, and the value reconstruction
    /// is not complete.
    fn unexpected_end(expected: Option<TokenTypes>) -> Self;
}

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
