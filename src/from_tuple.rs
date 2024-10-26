use crate::error::*;
use crate::from::FromTokenSink;
use crate::token::*;
use crate::TokenSink;

/// A base sink used by TupleSink to set the returnable value and
/// update the state machine when the element has been set.
struct TupleSubsink<T: FromTokenSink> {
    sink: Option<T::Sink>,
    index: *mut isize,
    out: *mut Option<T>,
}

impl<'a, T: FromTokenSink> TokenSink for TupleSubsink<T> {
    type Error = <T::Sink as TokenSink>::Error;

    fn yield_token(&mut self, token: Token<'_>) -> Result<bool, Self::Error> {
        let want_more = self.sink.as_mut().unwrap().yield_token(token)?;

        if !want_more {
            let v = T::from_sink(self.sink.take().unwrap());

            // SAFETY: TupleSink outlives TupleSubsink. TupleSink does
            // not update values until TupleSubsink has returned false.
            unsafe {
                *self.out = v;
                *self.index += 1;
            }
        }

        Ok(want_more)
    }
}

pub struct TupleSink<T, E: Error> {
    out: T,
    // -2 means EndTuple was seen.
    // -1 waits for Tuple.
    // 0-n is handling the element of that index.
    index: isize,
    subsink: Option<Box<dyn TokenSink<Error = E>>>,
}

impl<'a> TokenSink for TupleSink<(), TokenError> {
    type Error = TokenError;

    fn yield_token(&mut self, token: Token<'_>) -> Result<bool, Self::Error> {
        match token {
            Token::Tuple(_) if self.index == -1 => {
                self.index = 0;

                Ok(true)
            }
            Token::Unit if self.index == -1 => {
                self.index = -2;

                Ok(false)
            }
            Token::EndTuple if self.index == 0 => {
                self.index = -2;

                Ok(false)
            }
            t if self.index == -1 => Err(Self::Error::invalid_token(
                t,
                Some(TokenTypes::new(TokenType::Tuple).with(TokenType::Unit)),
            )),
            t if self.index == 0 => Err(Self::Error::invalid_token(
                t,
                Some(TokenTypes::new(TokenType::EndTuple)),
            )),
            t => Err(Self::Error::invalid_token(t, Some(TokenTypes::EMPTY))),
        }
    }

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        match self.index {
            -1 => Some(TokenTypes::new(TokenType::Tuple)),
            0 => Some(TokenTypes::new(TokenType::EndTuple)),
            _ => None,
        }
    }
}

impl FromTokenSink for () {
    type Sink = TupleSink<(), TokenError>;

    fn new_sink() -> Self::Sink {
        Self::Sink {
            out: (),
            index: -1,
            subsink: None,
        }
    }

    fn from_sink(sink: Self::Sink) -> Option<Self> {
        if sink.index == -2 {
            Some(())
        } else {
            None
        }
    }
}

macro_rules! just_none (
    ($_:tt) => { None }
);

macro_rules! tuple_from_tokens (
    ($n:literal => $($ty:ident $f:ident $i:tt),*) => {
        impl<'a, $($ty: FromTokenSink<Sink: TokenSink<Error = E> + 'static> + 'static),*, E: Error> TokenSink
            for TupleSink<($(Option<$ty>,)*), E>
        {
            type Error = E;

            fn yield_token(&mut self, token: Token<'_>) -> Result<bool, Self::Error> {
                if let Some(subsink) = self.subsink.as_mut() {
                    let want_more = subsink.yield_token(token)?;

                    // The subsink sets out and increments index.
                    if !want_more {
                        self.subsink.take();
                    }

                    return Ok(true);
                }

                match token {
                    Token::Tuple(_) if self.index == -1 => {
                        self.index = 0;

                        Ok(true)
                    }
                    Token::EndTuple if self.index == $n => {
                        self.index = -2;

                        Ok(false)
                    }
                    $(
                        t if self.index == $i => {
                            let mut subsink = TupleSubsink {
                                sink: Some($ty::new_sink()),
                                index: &mut self.index,
                                out: &mut self.out.$i,
                            };

                            // The subsink sets out and increments index.
                            if subsink.yield_token(t)? {
                                self.subsink = Some(Box::new(subsink));
                            }

                            Ok(true)
                        }
                    )*
                    t if self.index == -1 => Err(Self::Error::invalid_token(
                        t,
                        Some(TokenTypes::new(TokenType::Tuple)),
                    )),
                    t if self.index == $n => Err(Self::Error::invalid_token(
                        t,
                        Some(TokenTypes::new(TokenType::EndTuple)),
                    )),
                    t => Err(Self::Error::invalid_token(t, Some(TokenTypes::EMPTY))),
                }
            }

            fn expect_tokens(&mut self) -> Option<TokenTypes> {
                if let Some(subsink) = self.subsink.as_mut() {
                    return subsink.expect_tokens();
                }

                match self.index {
                    -1 => Some(TokenTypes::new(TokenType::Tuple)),
                    $($i => $ty::expect_initial(),)*
                    $n => Some(TokenTypes::new(TokenType::EndTuple)),
                    _ => None,
                }
            }
        }

        impl<$($ty: FromTokenSink<Sink: TokenSink<Error = E> + 'static> + 'static),*, E: Error> FromTokenSink for ($($ty,)*) {
            type Sink = TupleSink<($(Option<$ty>,)*), <T0::Sink as TokenSink>::Error>;

            fn new_sink() -> Self::Sink {
                Self::Sink {
                    out: ($(just_none!($i),)*),
                    index: -1,
                    subsink: None,
                }
            }

            fn from_sink(sink: Self::Sink) -> Option<Self> {
                match sink.out {
                    ($(Some($f),)*) => Some(($($f,)*)),
                    _ => None,
                }
            }
        }
    }
);

tuple_from_tokens!( 1 => T0 f0 0);
tuple_from_tokens!( 2 => T0 f0 0, T1 f1 1);
tuple_from_tokens!( 3 => T0 f0 0, T1 f1 1, T2 f2 2);
tuple_from_tokens!( 4 => T0 f0 0, T1 f1 1, T2 f2 2, T3 f3 3);
tuple_from_tokens!( 5 => T0 f0 0, T1 f1 1, T2 f2 2, T3 f3 3, T4 f4 4);
tuple_from_tokens!( 6 => T0 f0 0, T1 f1 1, T2 f2 2, T3 f3 3, T4 f4 4, T5 f5 5);
tuple_from_tokens!( 7 => T0 f0 0, T1 f1 1, T2 f2 2, T3 f3 3, T4 f4 4, T5 f5 5, T6 f6 6);
tuple_from_tokens!( 8 => T0 f0 0, T1 f1 1, T2 f2 2, T3 f3 3, T4 f4 4, T5 f5 5, T6 f6 6, T7 f7 7);
tuple_from_tokens!( 9 => T0 f0 0, T1 f1 1, T2 f2 2, T3 f3 3, T4 f4 4, T5 f5 5, T6 f6 6, T7 f7 7, T8 f8 8);
tuple_from_tokens!(10 => T0 f0 0, T1 f1 1, T2 f2 2, T3 f3 3, T4 f4 4, T5 f5 5, T6 f6 6, T7 f7 7, T8 f8 8, T9 f9 9);
tuple_from_tokens!(11 => T0 f0 0, T1 f1 1, T2 f2 2, T3 f3 3, T4 f4 4, T5 f5 5, T6 f6 6, T7 f7 7, T8 f8 8, T9 f9 9, T10 f10 10);
tuple_from_tokens!(12 => T0 f0 0, T1 f1 1, T2 f2 2, T3 f3 3, T4 f4 4, T5 f5 5, T6 f6 6, T7 f7 7, T8 f8 8, T9 f9 9, T10 f10 10, T11 f11 11);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::from::FromTokens as _;
    use crate::vec::*;

    #[test]
    fn test_from_token_sink_unit_seq() {
        let tokens = TokenVec::from(vec![
            OwningToken::Tuple(TupleMeta { size_hint: Some(0) }),
            OwningToken::EndTuple,
        ]);
        assert_eq!(<()>::from_tokens(tokens).unwrap(), ());
    }

    #[test]
    fn test_from_token_sink_unit() {
        let tokens = TokenVec::from(vec![OwningToken::Unit]);
        assert_eq!(<()>::from_tokens(tokens).unwrap(), ());
    }

    #[test]
    fn test_from_token_sink_tuple1() {
        let tokens = TokenVec::from(vec![
            OwningToken::Tuple(TupleMeta { size_hint: Some(1) }),
            OwningToken::U32(42),
            OwningToken::EndTuple,
        ]);
        assert_eq!(<(u32,)>::from_tokens(tokens).unwrap(), (42,));
    }

    #[test]
    fn test_from_token_sink_tuple2() {
        let tokens = TokenVec::from(vec![
            OwningToken::Tuple(TupleMeta { size_hint: Some(2) }),
            OwningToken::U32(42),
            OwningToken::Str("Hello".to_owned()),
            OwningToken::EndTuple,
        ]);
        assert_eq!(
            <(u32, String)>::from_tokens(tokens).unwrap(),
            (42, "Hello".to_owned())
        );
    }

    #[test]
    fn test_from_token_sink_tuple3() {
        let tokens = TokenVec::from(vec![
            OwningToken::Tuple(TupleMeta { size_hint: Some(2) }),
            OwningToken::U32(42),
            OwningToken::Str("Hello".to_owned()),
            OwningToken::Bool(true),
            OwningToken::EndTuple,
        ]);
        assert_eq!(
            <(u32, String, bool)>::from_tokens(tokens).unwrap(),
            (42, "Hello".to_owned(), true)
        );
    }
}
