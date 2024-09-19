use crate::error::*;
use crate::into::*;
use crate::token::*;
use crate::TokenSink;

/// Provides a way to associate appropriate sinks with Rust data types.
///
/// The sinks are used as building blocks to construct Rust values. If
/// you simply want to construct a Rust value, [FromTokens] is a
/// higher-level interface.
pub trait FromTokenSink: Sized {
    /// The type of sink used to construct this type of value.
    type Sink: TokenSink;

    /// Creates a new sink to construct a value with.
    fn new_sink() -> Self::Sink;

    /// Takes the constructed value from the sink. If it returns None,
    /// the token stream ended unexpectedly.
    fn from_sink(sink: Self::Sink) -> Option<Self>;

    /// Returns the token types that a freshly created sink will
    /// expect. The default implementation creates a new
    /// sink. Implementations may want to optimize this if creating a
    /// sink is expensive. See [TokenSink::expect_tokens].
    fn expect_initial() -> Option<TokenTypes> {
        Self::new_sink().expect_tokens()
    }
}

/// Constructs a Rust value from a token source.
///
/// This is a convenience wrapper for sinking into a [FromTokenSink]
/// that is appropriate for the data type, implemented for all types
/// that have [FromTokenSink].
///
/// The primary use of this wrapper is to flatten the outer start/end
/// tokens.
pub trait FromTokens: FromTokenSink {
    /// Constructs a Rust value from a token source.
    fn from_tokens<I: IntoTokens>(into: I) -> Result<Self, <Self::Sink as TokenSink>::Error>;
}

impl<T: FromTokenSink> FromTokens for T {
    fn from_tokens<I: IntoTokens>(into: I) -> Result<Self, <Self::Sink as TokenSink>::Error> {
        let mut sink = FromTokensSink::<T> {
            sink: Some(Self::new_sink()),
            value: None,
        };
        into.into_tokens(&mut sink)?;

        let expected = sink.expect_tokens();
        sink.value
            .ok_or(<Self::Sink as TokenSink>::Error::unexpected_end(expected))
    }
}

struct FromTokensSink<T: FromTokenSink> {
    sink: Option<T::Sink>,
    value: Option<T>,
}

impl<T: FromTokenSink> TokenSink for FromTokensSink<T> {
    type Error = <T::Sink as TokenSink>::Error;
    type Subsink<'c> = T::Sink where Self: 'c;

    fn yield_start<'b, 'c>(&mut self, token: Token<'b>) -> Result<Self::Subsink<'c>, Self::Error>
    where
        Self: 'c,
    {
        if let Some(mut inner) = self.sink.take() {
            inner.yield_token(token)?;
            Ok(inner)
        } else {
            panic!("yield_start called more than once");
        }
    }

    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error> {
        if let Some(mut inner) = self.sink.take() {
            if inner.yield_token(token)? {
                self.value = T::from_sink(inner);

                Ok(true)
            } else {
                Ok(false)
            }
        } else {
            panic!("yield_token called more than once");
        }
    }

    fn end<'b>(&mut self, sink: Self::Subsink<'b>)
    where
        Self: 'b,
    {
        if self.value.is_some() {
            panic!("end called twice");
        }

        self.value = T::from_sink(sink);
    }

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        self.sink
            .as_mut()
            .map(|sink| sink.expect_tokens())
            .flatten()
    }
}

pub struct BasicSink<T>(Option<T>);

macro_rules! basic_from_tokens [
    ($va:path, $tt:path => $ty:ty) => {
        impl TokenSink for BasicSink<$ty> {
            type Error = TokenError;
            type Subsink<'c> = Self where Self: 'c;

            fn yield_start<'c, 'd>(&mut self, token: Token<'c>) -> Result<Self::Subsink<'d>, Self::Error> where Self: 'd {
                Err(Self::Error::invalid_token(token, Some(TokenTypes::new($tt))))
            }

            fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error> {
                match token {
                    $va(v) => {
                        self.0 = Some(v);
                        Ok(true)
                    }
                    t => Err(Self::Error::invalid_token(t, Some(TokenTypes::new($tt)))),
                }
            }

            fn end<'b>(&mut self, _sink: Self::Subsink<'b>) where Self: 'b {}

            fn expect_tokens(&mut self) -> Option<TokenTypes> {
                Some(TokenTypes::new($tt))
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

pub struct VecSink<T: FromTokenSink>(Option<Vec<T>>);

impl<T: FromTokenSink> TokenSink for VecSink<T> {
    type Error = <T::Sink as TokenSink>::Error;
    type Subsink<'c> = T::Sink where Self: 'c;

    fn yield_start<'c, 'd>(&mut self, token: Token<'c>) -> Result<Self::Subsink<'d>, Self::Error>
    where
        Self: 'd,
    {
        let mut sink = T::new_sink();
        if sink.yield_token(token)? {
            panic!("yield_token is expected to require an end token");
        }
        Ok(sink)
    }

    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error> {
        match token {
            Token::Seq(meta) => {
                if let Some(size) = meta.size_hint {
                    self.0 = Some(Vec::with_capacity(size));
                } else {
                    self.0 = Some(Vec::new())
                }

                Ok(false)
            }
            Token::EndSeq => Ok(true),
            t => {
                let mut sink = T::new_sink();
                if !sink.yield_token(t)? {
                    panic!("yield_token is expected to finish with a single token");
                }

                self.end(sink);

                Ok(false)
            }
        }
    }

    fn end<'b>(&mut self, sink: Self::Subsink<'b>)
    where
        Self: 'b,
    {
        if let Some(v) = T::from_sink(sink) {
            if let Some(vec) = self.0.as_mut() {
                vec.push(v);
            } else {
                panic!("No Seq token before element");
            }
        }
    }

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        match self.0 {
            None => Some(TokenTypes::new(TokenType::Seq)),
            Some(_) => T::expect_initial().map(|tt| tt.with(TokenType::EndSeq)),
        }
    }
}

impl<T: FromTokenSink> FromTokenSink for Vec<T> {
    type Sink = VecSink<T>;

    fn new_sink() -> Self::Sink {
        VecSink(None)
    }

    fn from_sink(sink: Self::Sink) -> Option<Self> {
        sink.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
            TokenError::unexpected_end(Some(TokenTypes::new(TokenType::Bool)))
        );
    }

    #[test]
    fn test_bool_from_invalid() {
        let tokens = TokenVec::from(vec![OwningToken::U32(42)]);
        assert_eq!(
            bool::from_tokens(tokens).unwrap_err(),
            TokenError::invalid_token(Token::U32(42), Some(TokenTypes::new(TokenType::Bool)))
        );
    }
}
