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
pub enum E {
    InvalidToken(OwningToken, Option<TokenTypes>),
    UnexpectedEnd(Option<TokenTypes>),
}

impl Error for E {
    fn invalid_token(token: Token<'_>, expected: Option<TokenTypes>) -> Self {
        E::InvalidToken(token.into(), expected)
    }

    fn unexpected_end(expected: Option<TokenTypes>) -> Self {
        E::UnexpectedEnd(expected)
    }
}

impl fmt::Display for E {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::InvalidToken(_, _) => write!(f, "invalid token"),
            Self::UnexpectedEnd(_) => write!(f, "unexpected end"),
        }
    }
}

impl std::error::Error for E {}

#[derive(Debug)]
pub struct NoError;

impl Error for NoError {
    fn invalid_token(_token: Token<'_>, _expected: Option<TokenTypes>) -> Self {
        unreachable!()
    }

    fn unexpected_end(_expected: Option<TokenTypes>) -> Self {
        unreachable!()
    }
}

impl fmt::Display for NoError {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        unreachable!()
    }
}

impl std::error::Error for NoError {}
