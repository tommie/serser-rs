use std::cell::RefCell;
use std::rc::Rc;

use crate::error::*;
use crate::into::*;
use crate::token::*;
use crate::TokenSink;

/// A vector of tokens, which can be used as a source or sink.
#[derive(Debug, Eq, PartialEq)]
pub struct TokenVec {
    vec: Rc<RefCell<Vec<OwningToken>>>,
    level: usize,
}

impl TokenVec {
    pub fn new() -> Self {
        TokenVec {
            vec: Rc::new(RefCell::new(Vec::new())),
            level: 0,
        }
    }

    pub fn from_iter<'b, I: Iterator<Item = Token<'b>>>(it: I) -> Self {
        TokenVec {
            vec: Rc::new(RefCell::new(it.map(|t| t.clone().into()).collect())),
            level: 0,
        }
    }

    pub fn into_vec(self) -> Vec<OwningToken> {
        Rc::into_inner(self.vec).unwrap().into_inner()
    }
}

impl From<Vec<OwningToken>> for TokenVec {
    fn from(v: Vec<OwningToken>) -> Self {
        TokenVec {
            vec: Rc::new(RefCell::new(v)),
            level: 0,
        }
    }
}

impl Into<Vec<OwningToken>> for TokenVec {
    fn into(self) -> Vec<OwningToken> {
        Rc::into_inner(self.vec).unwrap().into_inner()
    }
}

impl TokenSink for TokenVec {
    type Error = TokenError;

    fn yield_token(&mut self, token: Token<'_>) -> Result<bool, Self::Error> {
        if token.is_start() {
            self.level += 1;
        } else if token.is_end() {
            self.level -= 1;
        }

        self.vec.borrow_mut().push(token.into());

        Ok(self.level > 0)
    }

    fn into_any(self: Box<Self>) -> Box<dyn std::any::Any> { self as _ }
}

impl<'a> IntoTokens for TokenVec {
    fn into_tokens<S: TokenSink>(&self, sink: &mut S) -> Result<(), S::Error> {
        let mut want_more = true;
        for token in self.vec.borrow().iter() {
            if !want_more {
                return Err(S::Error::invalid_token(
                    token.into(),
                    Some(TokenTypes::EMPTY),
                ));
            }

            want_more = sink.yield_token(token.into())?;
        }

        Ok(())
    }
}
