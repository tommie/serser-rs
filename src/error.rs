use crate::token::*;

pub trait Error {
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
