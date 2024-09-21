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
            panic!("yield_* called twice");
        }
    }

    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<(), Self::Error> {
        if let Some(mut inner) = self.sink.take() {
            inner.yield_token(token)?;

            self.value = T::from_sink(inner);

            Ok(())
        } else {
            panic!("yield_* called twice");
        }
    }

    fn yield_end<'b>(
        &mut self,
        token: Token<'b>,
        mut sink: Self::Subsink<'b>,
    ) -> Result<(), Self::Error>
    where
        Self: 'b,
    {
        if self.value.is_some() {
            panic!("yield_* called twice");
        }

        sink.yield_token(token)?;

        self.value = T::from_sink(sink);

        Ok(())
    }

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        if self.value.is_some() {
            Some(TokenTypes::EMPTY)
        } else {
            self.sink
                .as_mut()
                .map(|sink| sink.expect_tokens())
                .flatten()
        }
    }
}

pub struct BasicSink<T>(Option<T>);

macro_rules! basic_from_tokens [
    ($($tt:ident => $ty:ty),*$(,)?) => {
        $(
        impl TokenSink for BasicSink<$ty> {
            type Error = TokenError;
            type Subsink<'c> = Self where Self: 'c;

            fn yield_start<'c, 'd>(&mut self, token: Token<'c>) -> Result<Self::Subsink<'d>, Self::Error> where Self: 'd {
                Err(Self::Error::invalid_token(token, Some(TokenTypes::new(TokenType::$tt))))
            }

            fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<(), Self::Error> {
                match token {
                    Token::$tt(v) => {
                        self.0 = Some(v.into());
                        Ok(())
                    }
                    t => Err(Self::Error::invalid_token(t, Some(TokenTypes::new(TokenType::$tt)))),
                }
            }

            fn yield_end<'b>(
                &mut self,
                token: Token<'b>,
                _sink: Self::Subsink<'b>,
            ) -> Result<(), Self::Error>
            where
                Self: 'b {
                Err(Self::Error::invalid_token(token, Some(TokenTypes::new(TokenType::$tt))))
            }

            fn expect_tokens(&mut self) -> Option<TokenTypes> {
                match self.0 {
                    None => Some(TokenTypes::new(TokenType::$tt)),
                    Some(_) => Some(TokenTypes::EMPTY),
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
        )*
    }
];

basic_from_tokens![
    Bool => bool,

    U8 => u8,
    U16 => u16,
    U32 => u32,
    U64 => u64,
    U128 => u128,
    Usize => usize,

    I8 => i8,
    I16 => i16,
    I32 => i32,
    I64 => i64,
    I128 => i128,
    Isize => isize,

    F32 => f32,
    F64 => f64,
    Char => char,
    Str => String,
];

pub struct VecSink<T: FromTokenSink> {
    out: Option<Vec<T>>,
    is_bytes: bool,
}

impl<T: FromTokenSink> TokenSink for VecSink<T> {
    type Error = <T::Sink as TokenSink>::Error;
    type Subsink<'c> = T::Sink where Self: 'c;

    fn yield_start<'c, 'd>(&mut self, token: Token<'c>) -> Result<Self::Subsink<'d>, Self::Error>
    where
        Self: 'd,
    {
        let mut sink = T::new_sink();
        sink.yield_token(token)?;
        Ok(sink)
    }

    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<(), Self::Error> {
        match token {
            Token::Seq(meta) => {
                if let Some(size) = meta.size_hint {
                    self.out = Some(Vec::with_capacity(size));
                } else {
                    self.out = Some(Vec::new())
                }
            }
            Token::EndSeq => {}
            Token::Bytes(v) if self.is_bytes && self.out.is_none() => {
                // SAFETY: assuming the is_vec_u8 hack is correct, this is a no-op.
                let out: &mut Option<Vec<u8>> = unsafe { std::mem::transmute(&mut self.out) };
                *out = Some(v.to_vec());
            }
            t => {
                if self.out.is_none() {
                    return Err(Self::Error::invalid_token(
                        t,
                        Some(TokenTypes::new(TokenType::Seq)),
                    ));
                }

                self.yield_end(t, T::new_sink())?;
            }
        }

        Ok(())
    }

    fn yield_end<'b>(
        &mut self,
        token: Token<'b>,
        mut sink: Self::Subsink<'b>,
    ) -> Result<(), Self::Error>
    where
        Self: 'b,
    {
        sink.yield_token(token)?;

        if let Some(v) = T::from_sink(sink) {
            if let Some(vec) = self.out.as_mut() {
                vec.push(v);
            } else {
                panic!("No Seq token before element");
            }
        }

        Ok(())
    }

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        match self.out {
            None => Some(TokenTypes::new(TokenType::Seq)),
            Some(_) => T::expect_initial().map(|tt| tt.with(TokenType::EndSeq)),
        }
    }
}

impl<T: FromTokenSink> FromTokenSink for Vec<T> {
    type Sink = VecSink<T>;

    fn new_sink() -> Self::Sink {
        VecSink {
            out: None,
            is_bytes: is_vec_u8::<Vec<T>>(),
        }
    }

    fn from_sink(sink: Self::Sink) -> Option<Self> {
        sink.out
    }
}

// From https://users.rust-lang.org/t/hack-to-specialize-w-write-for-vec-u8/100366/6.
fn is_vec_u8<T>() -> bool {
    use std::cell::Cell;
    use std::marker::PhantomData;

    struct IsVecU8<'a, T>(&'a Cell<bool>, PhantomData<T>);
    // Rust specializes Copy and doesn't call Clone
    impl Copy for IsVecU8<'_, Vec<u8>> {}

    impl<T> Clone for IsVecU8<'_, T> {
        fn clone(&self) -> Self {
            self.0.set(false);
            Self(self.0, self.1)
        }
    }
    let cell = Cell::new(true);
    _ = [IsVecU8::<'_, T>(&cell, PhantomData)].clone();
    if cell.get() {
        assert_eq!(std::mem::size_of::<T>(), std::mem::size_of::<Vec<u8>>());
        assert_eq!(std::mem::align_of::<T>(), std::mem::align_of::<Vec<u8>>());
        true
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vec::*;

    macro_rules! basic_test (
        ($tt:ident $ty:path [$v:expr]) => {
            #[allow(non_snake_case)]
            #[test]
            fn $tt() {
                let tokens = TokenVec::from(vec![OwningToken::$tt($v)]);
                assert_eq!(<$ty>::from_tokens(tokens).unwrap(), $v);
            }
        }
    );

    mod basic {
        use super::super::*;
        use crate::vec::*;

        basic_test!(Bool bool [true]);
        basic_test!(U8 u8 [42]);
        basic_test!(U16 u16 [42]);
        basic_test!(U32 u32 [42]);
        basic_test!(U64 u64 [42]);
        basic_test!(U128 u128 [42]);
        basic_test!(Usize usize [42]);
        basic_test!(I8 i8 [42]);
        basic_test!(I16 i16 [42]);
        basic_test!(I32 i32 [42]);
        basic_test!(I64 i64 [42]);
        basic_test!(I128 i128 [42]);
        basic_test!(Isize isize [42]);

        basic_test!(F32 f32 [42.5]);
        basic_test!(F64 f64 [42.5]);
        basic_test!(Char char ['W']);
        basic_test!(Str String ["Hello".to_owned()]);
    }

    #[test]
    fn test_vec_u8_bytes() {
        let tokens = TokenVec::from(vec![OwningToken::Bytes(b"Hello World".to_vec())]);
        assert_eq!(Vec::<u8>::from_tokens(tokens).unwrap(), b"Hello World");
    }

    #[test]
    fn test_vec_i8_bytes_fails() {
        let input = OwningToken::Bytes(b"Hello World".to_vec());
        let tokens = TokenVec::from(vec![input.clone()]);
        assert_eq!(
            Vec::<i8>::from_tokens(tokens).unwrap_err(),
            TokenError::InvalidToken(input, Some(TokenTypes::new(TokenType::Seq)))
        );
    }

    #[test]
    fn test_vec_u8() {
        let tokens = TokenVec::from(vec![
            OwningToken::Seq(SeqMeta { size_hint: Some(5) }),
            OwningToken::U8(b'H'),
            OwningToken::U8(b'e'),
            OwningToken::U8(b'l'),
            OwningToken::U8(b'l'),
            OwningToken::U8(b'o'),
            OwningToken::EndSeq,
        ]);
        assert_eq!(Vec::<u8>::from_tokens(tokens).unwrap(), b"Hello");
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
