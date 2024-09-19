use crate::error::*;
use crate::into::*;
use crate::token::*;
use crate::TokenSink;

pub trait FromTokenSink: Sized {
    /// The type of sink used to construct this type of value.
    type Sink: TokenSink;

    /// Creates a new sink to construct a value with.
    fn new_sink() -> Self::Sink;

    /// Takes the constructed value from the sink. If it returns None,
    /// the token stream ended unexpectedly.
    fn from_sink(sink: Self::Sink) -> Option<Self>;
}

pub trait FromTokens: FromTokenSink {
    fn from_tokens<I: IntoTokens>(into: I) -> Result<Self, <Self::Sink as TokenSink>::Error>;
}

impl<T: FromTokenSink> FromTokens for T {
    fn from_tokens<I: IntoTokens>(into: I) -> Result<Self, <Self::Sink as TokenSink>::Error> {
        let mut sink = Self::new_sink();
        into.into_tokens(&mut sink)?;

        Self::from_sink(sink).ok_or(<Self::Sink as TokenSink>::Error::unexpected_end(None))
    }
}

pub struct BasicSink<T>(Option<T>);

macro_rules! basic_from_tokens [
    ($va:path, $tt:path => $ty:ty) => {
        impl TokenSink for BasicSink<$ty> {
            type Error = E;

            fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error> {
                match token {
                    $va(v) => {
                        self.0 = Some(v);
                        Ok(true)
                    }
                    t => Err(Self::Error::invalid_token(t, Some(TokenTypes::new($tt)))),
                }
            }
        }

        impl FromTokenSink for $ty {
            type Sink = BasicSink<$ty>;

            fn new_sink() -> Self::Sink {
                BasicSink(None)
            }

            fn from_sink(sink: Self::Sink) -> Option<Self> {
                sink.0
            }
        }
    }
];

basic_from_tokens![Token::Bool, TokenType::Bool => bool];
basic_from_tokens![Token::U32, TokenType::Bool => u32];

pub struct VecSink<T: FromTokenSink>(Vec<T>, VecSinkState, Option<T::Sink>);

#[derive(Eq, PartialEq)]
pub enum VecSinkState {
    Start,
    NewElem,
    Elem,
    End,
}

impl<T: FromTokenSink> TokenSink for VecSink<T> {
    type Error = <T::Sink as TokenSink>::Error;

    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error> {
        if let Some(sink) = &mut self.2 {
            self.1 = VecSinkState::Elem;

            if sink.yield_token(token)? {
                if let Some(v) = T::from_sink(self.2.take().unwrap()) {
                    self.0.push(v);
                    self.1 = VecSinkState::NewElem;
                }
            }

            return Ok(false);
        }

        match token {
            Token::Seq(meta) if self.1 == VecSinkState::Start => {
                if let Some(size) = meta.size_hint {
                    self.0 = Vec::with_capacity(size);
                    self.1 = VecSinkState::NewElem;
                }

                Ok(false)
            }
            Token::EndSeq => {
                self.1 = VecSinkState::End;

                Ok(true)
            }
            t if !t.is_end() => {
                self.2 = Some(T::new_sink());
                if self.2.as_mut().unwrap().yield_token(t)? {
                    self.1 = VecSinkState::Elem;
                    if let Some(v) = T::from_sink(self.2.take().unwrap()) {
                        self.0.push(v);
                        self.1 = VecSinkState::NewElem;
                    }
                }

                Ok(false)
            }
            t => {
                self.2 = Some(T::new_sink());
                Err(Self::Error::invalid_token(
                    t,
                    self.2
                        .as_mut()
                        .unwrap()
                        .expect_tokens()
                        .map(|tt| tt.with(TokenType::EndSeq)),
                ))
            }
        }
    }

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        match self.1 {
            VecSinkState::Start => Some(TokenTypes::new(TokenType::Seq)),
            VecSinkState::NewElem => {
                if self.2.is_none() {
                    self.2 = Some(T::new_sink());
                }

                self.2
                    .as_mut()
                    .unwrap()
                    .expect_tokens()
                    .map(|types| types.with(TokenType::EndSeq))
            }
            VecSinkState::Elem => self.2.as_mut().unwrap().expect_tokens(),
            VecSinkState::End => None,
        }
    }
}

impl<T: FromTokenSink> FromTokenSink for Vec<T> {
    type Sink = VecSink<T>;

    fn new_sink() -> Self::Sink {
        VecSink(Vec::new(), VecSinkState::Start, None)
    }

    fn from_sink(sink: Self::Sink) -> Option<Self> {
        if sink.1 == VecSinkState::End {
            Some(sink.0)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::meta::*;
    use crate::vec::*;

    #[test]
    fn test_bool_from() {
        let tokens = TokenVec::from(vec![OwningToken::Bool(true)]);
        assert_eq!(bool::from_tokens(tokens).unwrap(), true);
    }

    #[test]
    fn test_vec_u32_from() {
        let tokens = TokenVec::from(vec![
            OwningToken::Seq(SeqMeta { size_hint: Some(1) }),
            OwningToken::U32(42),
            OwningToken::EndSeq,
        ]);
        assert_eq!(Vec::<u32>::from_tokens(tokens).unwrap(), vec![42]);
    }

    #[test]
    fn test_vec_vec_u32_from() {
        let tokens = TokenVec::from(vec![
            OwningToken::Seq(SeqMeta { size_hint: Some(1) }),
            OwningToken::Seq(SeqMeta { size_hint: Some(2) }),
            OwningToken::U32(42),
            OwningToken::U32(43),
            OwningToken::EndSeq,
            OwningToken::EndSeq,
        ]);
        assert_eq!(
            Vec::<Vec<u32>>::from_tokens(tokens).unwrap(),
            vec![vec![42, 43]]
        );
    }

    #[test]
    fn test_from_end() {
        let tokens = TokenVec::from(vec![]);
        assert_eq!(
            bool::from_tokens(tokens).unwrap_err(),
            E::unexpected_end(None)
        );
    }

    #[test]
    fn test_bool_from_invalid() {
        let tokens = TokenVec::from(vec![OwningToken::U32(42)]);
        assert_eq!(
            bool::from_tokens(tokens).unwrap_err(),
            E::invalid_token(Token::U32(42), Some(TokenTypes::new(TokenType::Bool)))
        );
    }
}
