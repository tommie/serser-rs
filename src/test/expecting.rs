use std::cell::RefCell;
use std::rc::Rc;

use crate::token::*;
use crate::TokenSink;

pub struct ExpectingTokenSink<'a, S: TokenSink, F: Fn(usize) -> Option<TokenTypes>>(
    Rc<RefCell<(usize, F)>>,
    &'a mut S,
);

impl<'a, S: TokenSink, F: Fn(usize) -> Option<TokenTypes>> ExpectingTokenSink<'a, S, F> {
    pub fn new(sink: &'a mut S, fun: F) -> Self {
        Self(Rc::new(RefCell::new((0, fun))), sink)
    }
}

impl<'a, S: TokenSink, F: Fn(usize) -> Option<TokenTypes>> TokenSink
    for ExpectingTokenSink<'a, S, F>
{
    type Error = S::Error;
    type Subsink<'c> = ExpectingTokenSubsink<S::Subsink<'c>, F> where Self: 'c;

    fn yield_start<'b, 'c>(&mut self, token: Token<'b>) -> Result<Self::Subsink<'c>, Self::Error>
    where
        Self: 'c,
    {
        self.0.borrow_mut().0 += 1;
        let sink = self.1.yield_start(token)?;

        Ok(ExpectingTokenSubsink(self.0.clone(), sink))
    }

    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error> {
        self.0.borrow_mut().0 += 1;
        self.1.yield_token(token)
    }

    fn end<'b>(&mut self, sink: Self::Subsink<'b>)
    where
        Self: 'b,
    {
        self.1.end(sink.1);
    }

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        let inner = self.0.borrow_mut();

        inner.1(inner.0)
    }
}

pub struct ExpectingTokenSubsink<S: TokenSink, F: Fn(usize) -> Option<TokenTypes>>(
    Rc<RefCell<(usize, F)>>,
    S,
);

impl<S: TokenSink, F: Fn(usize) -> Option<TokenTypes>> TokenSink for ExpectingTokenSubsink<S, F> {
    type Error = S::Error;
    type Subsink<'c> = ExpectingTokenSubsink<S::Subsink<'c>, F> where Self: 'c;

    fn yield_start<'b, 'c>(&mut self, token: Token<'b>) -> Result<Self::Subsink<'c>, Self::Error>
    where
        Self: 'c,
    {
        self.0.borrow_mut().0 += 1;
        let sink = self.1.yield_start(token)?;

        Ok(ExpectingTokenSubsink(self.0.clone(), sink))
    }

    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error> {
        self.0.borrow_mut().0 += 1;
        self.1.yield_token(token)
    }

    fn end<'b>(&mut self, sink: Self::Subsink<'b>)
    where
        Self: 'b,
    {
        self.1.end(sink.1);
    }

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        self.1.expect_tokens().or_else(|| {
            let inner = self.0.borrow_mut();

            inner.1(inner.0)
        })
    }
}
