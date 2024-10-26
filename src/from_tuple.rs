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
    index: isize,
    subsink: Option<Box<dyn TokenSink<Error = E>>>,
}

impl<'a, T0: FromTokenSink<Sink: TokenSink<Error = E> + 'static> + 'static, E: Error> TokenSink
    for TupleSink<(Option<T0>,), E>
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
            Token::Seq(_) if self.index == -1 => {
                self.index = 0;

                Ok(true)
            }
            Token::EndSeq if self.index == 1 => {
                self.index = -2;

                Ok(false)
            }
            t if self.index == 0 => {
                let mut subsink = TupleSubsink {
                    sink: Some(T0::new_sink()),
                    index: &mut self.index,
                    out: &mut self.out.0,
                };

                // The subsink sets out and increments index.
                if subsink.yield_token(t)? {
                    self.subsink = Some(Box::new(subsink));
                }

                Ok(true)
            }
            t if self.index == -1 => Err(Self::Error::invalid_token(
                t,
                Some(TokenTypes::new(TokenType::Seq)),
            )),
            t if self.index == 1 => Err(Self::Error::invalid_token(
                t,
                Some(TokenTypes::new(TokenType::EndSeq)),
            )),
            t => Err(Self::Error::invalid_token(t, Some(TokenTypes::EMPTY))),
        }
    }

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        if let Some(subsink) = self.subsink.as_mut() {
            return subsink.expect_tokens();
        }

        match self.index {
            -1 => Some(TokenTypes::new(TokenType::Seq)),
            0 => T0::expect_initial(),
            1 => Some(TokenTypes::new(TokenType::EndSeq)),
            _ => None,
        }
    }
}

impl<T0: FromTokenSink<Sink: 'static> + 'static> FromTokenSink for (T0,) {
    type Sink = TupleSink<(Option<T0>,), <T0::Sink as TokenSink>::Error>;

    fn new_sink() -> Self::Sink {
        Self::Sink {
            out: (None,),
            index: -1,
            subsink: None,
        }
    }

    fn from_sink(sink: Self::Sink) -> Option<Self> {
        match sink.out {
            (Some(f0),) => Some((f0,)),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::from::FromTokens as _;
    use crate::vec::*;

    #[test]
    fn test_from_token_sink_tuple1() {
        let tokens = TokenVec::from(vec![
            OwningToken::Seq(SeqMeta { size_hint: Some(1) }),
            OwningToken::U32(42),
            OwningToken::EndSeq,
        ]);
        assert_eq!(<(u32,)>::from_tokens(tokens).unwrap(), (42,));
    }
}
