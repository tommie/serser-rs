use std::cell::RefCell;
use std::rc::Rc;

use maybe_owned::MaybeOwnedMut;

use crate::token::*;
use crate::TokenSink;

/// Overrides the [TokenSink::expect_tokens] for another sink.
///
/// This can be useful when testing sources that can negotiate token
/// types, e.g. JSON-encoded enums.
pub struct ExpectingTokenSink<'a, S: TokenSink, F: Fn(usize) -> Option<TokenTypes>> {
    inner: Rc<RefCell<(usize, F)>>,
    sink: MaybeOwnedMut<'a, S>,
}

impl<'a, S: TokenSink, F: Fn(usize) -> Option<TokenTypes>> ExpectingTokenSink<'a, S, F> {
    /// Constructs a new wrapping sink. The function is called with
    /// the current token count, starting at zero. It is incremented
    /// for all tokens.
    pub fn new(sink: &'a mut S, fun: F) -> Self {
        Self {
            inner: Rc::new(RefCell::new((0, fun))),
            sink: sink.into(),
        }
    }
}

impl<'a, S: TokenSink, F: Fn(usize) -> Option<TokenTypes>> TokenSink
    for ExpectingTokenSink<'a, S, F>
{
    type Error = S::Error;
    type Subsink<'c> = ExpectingTokenSink<'c, S::Subsink<'c>, F> where Self: 'c;

    fn yield_start<'b, 'c>(&mut self, token: Token<'b>) -> Result<Self::Subsink<'c>, Self::Error>
    where
        Self: 'c,
    {
        self.inner.borrow_mut().0 += 1;
        let sink = self.sink.yield_start(token)?;

        // No need to call yield_token.
        Ok(ExpectingTokenSink {
            inner: self.inner.clone(),
            sink: sink.into(),
        })
    }

    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<(), Self::Error> {
        self.inner.borrow_mut().0 += 1;
        self.sink.yield_token(token)
    }

    fn yield_end<'b>(
        &mut self,
        token: Token<'b>,
        sink: Self::Subsink<'b>,
    ) -> Result<(), Self::Error>
    where
        Self: 'b,
    {
        // No need to call yield_token.
        self.inner.borrow_mut().0 += 1;
        if let MaybeOwnedMut::Owned(subsink) = sink.sink {
            self.sink.yield_end(token, subsink)
        } else {
            // The sink from yield_start is always owned.
            unreachable!()
        }
    }

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        let inner = self.inner.borrow_mut();

        inner.1(inner.0).or_else(|| self.sink.expect_tokens())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vec::*;

    #[test]
    fn test_first() {
        let mut got = TokenVec::new();
        let mut exp = ExpectingTokenSink::new(&mut got, |_| Some(TokenTypes::new(TokenType::U32)));
        assert_eq!(exp.expect_tokens(), Some(TokenTypes::new(TokenType::U32)));
    }

    #[test]
    fn test_second() {
        let mut got = TokenVec::new();
        let mut exp = ExpectingTokenSink::new(&mut got, |i| {
            if i == 1 {
                Some(TokenTypes::new(TokenType::U32))
            } else {
                None
            }
        });
        assert_eq!(exp.expect_tokens(), None);
        exp.yield_token(Token::Bool(true)).unwrap();
        assert_eq!(exp.expect_tokens(), Some(TokenTypes::new(TokenType::U32)));
    }

    #[test]
    fn test_default() {
        let mut got = TokenVec::new();
        let mut exp1 = ExpectingTokenSink::new(&mut got, |_| Some(TokenTypes::new(TokenType::U32)));
        let mut exp2 = ExpectingTokenSink::new(&mut exp1, |_| None);
        assert_eq!(exp2.expect_tokens(), Some(TokenTypes::new(TokenType::U32)));
    }
}
