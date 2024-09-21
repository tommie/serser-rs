use std::cell::RefCell;
use std::rc::Rc;

use crate::error::*;
use crate::into::*;
use crate::token::*;
use crate::TokenSink;

/// A vector of tokens, which can be used as a source or sink.
#[derive(Debug, Eq, PartialEq)]
pub struct TokenVec(Rc<RefCell<Vec<OwningToken>>>);

impl TokenVec {
    pub fn new() -> Self {
        TokenVec(Rc::new(RefCell::new(Vec::new())))
    }

    pub fn from_iter<'b, I: Iterator<Item = Token<'b>>>(it: I) -> Self {
        TokenVec(Rc::new(RefCell::new(
            it.map(|t| t.clone().into()).collect(),
        )))
    }

    pub fn into_vec(self) -> Vec<OwningToken> {
        Rc::into_inner(self.0).unwrap().into_inner()
    }
}

impl From<Vec<OwningToken>> for TokenVec {
    fn from(v: Vec<OwningToken>) -> Self {
        TokenVec(Rc::new(RefCell::new(v)))
    }
}

impl Into<Vec<OwningToken>> for TokenVec {
    fn into(self) -> Vec<OwningToken> {
        Rc::into_inner(self.0).unwrap().into_inner()
    }
}

impl TokenSink for TokenVec {
    type Error = TokenError;
    type Subsink<'c> = TokenVec where Self: 'c;

    fn yield_start<'b, 'c>(&mut self, token: Token<'b>) -> Result<Self::Subsink<'c>, Self::Error>
    where
        Self: 'c,
    {
        self.yield_token(token)?;

        // No need to call yield_token.

        Ok(Self(self.0.clone()))
    }

    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<(), Self::Error> {
        self.0.borrow_mut().push(token.into());

        Ok(())
    }

    fn yield_end<'b>(
        &mut self,
        token: Token<'b>,
        _sink: Self::Subsink<'b>,
    ) -> Result<(), Self::Error>
    where
        Self: 'b,
    {
        // No need to call yield_token.

        self.yield_token(token)
    }
}

impl TokenVec {
    fn into_tokens_rec<'b, S: TokenSink, I: Iterator<Item = &'b OwningToken>>(
        &self,
        sink: &mut S,
        it: &mut I,
    ) -> Result<Option<&'b OwningToken>, S::Error> {
        while let Some(token) = it.next() {
            if token.is_start() {
                let mut subsink = sink.yield_start(token.into())?;
                let end = self.into_tokens_rec(&mut subsink, it)?;
                sink.yield_end(end.unwrap().into(), subsink)?;
            } else if token.is_end() {
                return Ok(Some(token));
            } else {
                sink.yield_token(token.into())?;
            }
        }

        Ok(None)
    }
}

impl<'a> IntoTokens for TokenVec {
    fn into_tokens<S: TokenSink>(&self, sink: &mut S) -> Result<(), S::Error> {
        self.into_tokens_rec(sink, &mut self.0.borrow().iter())?;

        Ok(())
    }
}
