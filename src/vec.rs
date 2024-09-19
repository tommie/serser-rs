use crate::error::*;
use crate::into::*;
use crate::token::*;
use crate::TokenSink;

#[derive(Debug, Eq, PartialEq)]
pub struct TokenVec(Vec<OwningToken>);

impl TokenVec {
    pub fn new() -> Self {
        TokenVec(Vec::new())
    }

    pub fn from_iter<'a, I: Iterator<Item = &'a Token<'a>>>(it: I) -> Self {
        TokenVec(it.map(|t| t.clone().into()).collect())
    }

    pub fn into_vec(self) -> Vec<OwningToken> {
        self.0
    }
}

impl From<Vec<OwningToken>> for TokenVec {
    fn from(v: Vec<OwningToken>) -> Self {
        TokenVec(v)
    }
}

impl Into<Vec<OwningToken>> for TokenVec {
    fn into(self) -> Vec<OwningToken> {
        self.0
    }
}

impl TokenSink for TokenVec {
    type Error = NoError;

    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error> {
        self.0.push(token.into());

        Ok(false)
    }
}

impl<'a> IntoTokens for TokenVec {
    fn into_tokens<S: TokenSink>(&self, sink: &mut S) -> Result<(), S::Error> {
        for token in self.0.iter() {
            sink.yield_token(token.into())?;
        }

        Ok(())
    }
}
