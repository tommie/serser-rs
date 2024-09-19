use std::cell::RefCell;

use crate::error::*;
use crate::into::*;
use crate::token::*;
use crate::TokenSink;

#[derive(Debug, Eq, PartialEq)]
pub struct TokenVec(RefCell<Vec<OwningToken>>);

impl TokenVec {
    pub fn new() -> Self {
        TokenVec(RefCell::new(Vec::new()))
    }

    pub fn from_iter<'b, I: Iterator<Item = &'b Token<'b>>>(it: I) -> Self {
        TokenVec(RefCell::new(it.map(|t| t.clone().into()).collect()))
    }

    pub fn into_vec(self) -> Vec<OwningToken> {
        self.0.into_inner()
    }
}

impl From<Vec<OwningToken>> for TokenVec {
    fn from(v: Vec<OwningToken>) -> Self {
        TokenVec(RefCell::new(v))
    }
}

impl Into<Vec<OwningToken>> for TokenVec {
    fn into(self) -> Vec<OwningToken> {
        self.0.into_inner()
    }
}

impl TokenSink for TokenVec {
    type Error = NoError;
    type Subsink<'c> = TokenVecSubsink<'c> where Self: 'c;

    fn yield_start<'b, 'c>(&'c mut self, token: Token<'b>) -> Result<Self::Subsink<'c>, Self::Error> where Self: 'c {
        self.yield_token(token)?;

        Ok(TokenVecSubsink(&self.0))
    }

    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error> {
        self.0.borrow_mut().push(token.into());

        Ok(false)
    }
}

impl TokenVec {
    fn into_tokens_rec<'b, S: TokenSink, I: Iterator<Item = &'b OwningToken>>(&self, sink: &mut S, it: &mut I) -> Result<(), S::Error> {
        while let Some(token) = it.next() {
            if token.is_start() {
                self.into_tokens_rec(&mut sink.yield_start(token.into())?, it)?;
            } else {
                let end = token.is_end();
                sink.yield_token(token.into())?;
                if end {
                    break;
                }
            }
        };

        Ok(())
    }
}

impl<'a> IntoTokens for TokenVec {
    fn into_tokens<S: TokenSink>(&self, sink: &mut S) -> Result<(), S::Error> {
        self.into_tokens_rec(sink, &mut self.0.borrow().iter())
    }
}

pub struct TokenVecSubsink<'a>(&'a RefCell<Vec<OwningToken>>);

impl<'a> TokenSink for TokenVecSubsink<'a> {
    type Error = NoError;
    type Subsink<'c> = TokenVecSubsink<'c> where Self: 'c;

    fn yield_start<'b, 'c>(&'c mut self, token: Token<'b>) -> Result<Self::Subsink<'c>, Self::Error> where Self: 'c {
        self.yield_token(token)?;

        Ok(TokenVecSubsink(self.0))
    }

    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error> {
        self.0.borrow_mut().push(token.into());

        Ok(false)
    }
}
