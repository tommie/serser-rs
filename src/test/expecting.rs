use crate::token::*;
use crate::TokenSink;

/// Overrides the [TokenSink::expect_tokens] for another sink.
///
/// This can be useful when testing sources that can negotiate token
/// types, e.g. JSON-encoded enums.
pub struct ExpectingTokenSink<'a, S: TokenSink, F: Fn(usize) -> Option<TokenTypes>> {
    sink: &'a mut S,
    fun: F,
    index: usize,
}

impl<'a, S: TokenSink, F: Fn(usize) -> Option<TokenTypes>> ExpectingTokenSink<'a, S, F> {
    /// Constructs a new wrapping sink. The function is called with
    /// the current token count, starting at zero. It is incremented
    /// for all tokens.
    pub fn new(sink: &'a mut S, fun: F) -> Self {
        Self {
            sink,
            fun,
            index: 0,
        }
    }
}

impl<'a, S: TokenSink, F: Fn(usize) -> Option<TokenTypes>> TokenSink
    for ExpectingTokenSink<'a, S, F>
{
    type Error = S::Error;

    fn yield_token(&mut self, token: Token<'_>) -> Result<bool, Self::Error> {
        self.index += 1;
        self.sink.yield_token(token)
    }

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        (self.fun)(self.index).or_else(|| self.sink.expect_tokens())
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
