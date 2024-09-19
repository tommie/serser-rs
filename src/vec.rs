use std::cell::RefCell;
use std::rc::Rc;

use crate::error::*;
use crate::into::*;
use crate::token::*;
use crate::TokenSink;

#[derive(Debug, Eq, PartialEq)]
pub struct TokenVec(Rc<RefCell<Vec<OwningToken>>>);

impl TokenVec {
    pub fn new() -> Self {
        TokenVec(Rc::new(RefCell::new(Vec::new())))
    }

    pub fn from_iter<'b, I: Iterator<Item = &'b Token<'b>>>(it: I) -> Self {
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
    type Error = NoError;
    type Subsink<'c> = TokenVec where Self: 'c;

    fn yield_start<'b, 'c>(&mut self, token: Token<'b>) -> Result<Self::Subsink<'c>, Self::Error>
    where
        Self: 'c,
    {
        self.yield_token(token)?;

        Ok(TokenVec(self.0.clone()))
    }

    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error> {
        self.0.borrow_mut().push(token.into());

        Ok(false)
    }

    fn end<'b>(&mut self, _sink: Self::Subsink<'b>)
    where
        Self: 'b,
    {
    }
}

impl TokenVec {
    fn into_tokens_rec<'b, S: TokenSink, I: Iterator<Item = &'b OwningToken>>(
        &self,
        sink: &mut S,
        it: &mut I,
    ) -> Result<(), S::Error> {
        while let Some(token) = it.next() {
            if token.is_start() {
                let mut subsink = sink.yield_start(token.into())?;
                self.into_tokens_rec(&mut subsink, it)?;
                sink.end(subsink);
            } else {
                let end = token.is_end();
                sink.yield_token(token.into())?;
                if end {
                    break;
                }
            }
        }

        Ok(())
    }
}

impl<'a> IntoTokens for TokenVec {
    fn into_tokens<S: TokenSink>(&self, sink: &mut S) -> Result<(), S::Error> {
        self.into_tokens_rec(sink, &mut self.0.borrow().iter())
    }
}
