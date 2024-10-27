use core::cell::Cell;
use core::marker::PhantomData;
use core::task::Poll;

use std::boxed::Box;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::task::Context;
use std::task::Wake;
use std::task::Waker;

use crate::token::*;
use crate::TokenSink;

struct NoWake;

impl Wake for NoWake {
    fn wake(self: Arc<Self>) {
        unreachable!("AsyncTokenSink only supports awaiting AsyncTokenReader::next")
    }
}

pub struct AsyncTokenReader<E: crate::error::Error = crate::error::TokenError>(
    Arc<Cell<Option<OwningToken>>>,
    Arc<Cell<Option<TokenTypes>>>,
    PhantomData<E>,
);

impl<E: crate::error::Error> AsyncTokenReader<E> {
    fn next(&mut self, expected: Option<TokenTypes>) -> NextToken {
        self.1.set(expected);

        NextToken(self.0.clone())
    }
}

pub struct NextToken(Arc<Cell<Option<OwningToken>>>);

impl Future for NextToken {
    type Output = Option<OwningToken>;

    fn poll(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<Self::Output> {
        if let Some(token) = self.0.take() {
            Poll::Ready(Some(token))
        } else {
            Poll::Pending
        }
    }
}

/// Provides a simpler interface for reading a stream of [Token]s and
/// producing a value, using async blocks.
///
/// This is *not* able to do async I/O, but converts the push
/// interface of [TokenSink] into a pull interface called
/// [AsyncTokenReader]. The only awaitable Future allowed is
/// [AsyncTokenReader::next].
///
/// This interface is slower than building a [TokenSink], but may be
/// useful where code clarity is more important than
/// performance. E.g. [OwningToken] is used instead of [Token] to
/// allow returning it from a function. The machinery of running a
/// Future is also not free, e.g. due to the many `Arc`s.
pub struct AsyncTokenSink<F: Future<Output = Result<T, E>>, T, E: crate::error::Error>(
    Pin<Box<F>>,
    Arc<Cell<Option<OwningToken>>>,
    Arc<Cell<Option<TokenTypes>>>,
    Arc<NoWake>,
    Option<Result<T, E>>,
);

impl<F: Future<Output = Result<T, E>>, T, E: crate::error::Error> AsyncTokenSink<F, T, E> {
    /// Creates a new token sink for the async function. Whatever the
    /// function returns is available at the return of
    /// [AsyncTokenSink::end]. The end function should always be
    /// called to ensure values in the async function body are dropped
    /// properly.
    fn new(f: impl FnOnce(AsyncTokenReader<E>) -> F) -> Result<Self, E> {
        let token_buf = Arc::new(Cell::new(None));
        let expected_buf = Arc::new(Cell::new(None));
        Ok(Self(
            Box::pin(f(AsyncTokenReader::<E>(
                token_buf.clone(),
                expected_buf.clone(),
                PhantomData,
            ))),
            token_buf,
            expected_buf,
            Arc::new(NoWake {}),
            None,
        ))
    }
}

impl<F: Future<Output = Result<T, E>>, T, E: crate::error::Error> AsyncTokenSink<F, T, E> {
    pub fn end(&mut self) -> Result<T, E> {
        if let Some(ret) = self.4.take() {
            ret
        } else {
            let w = Waker::from(self.3.clone());
            let mut cx = Context::from_waker(&w);

            match self.0.as_mut().poll(&mut cx) {
                Poll::Ready(ret) => ret,
                Poll::Pending => Err(E::unexpected_end(self.2.get())),
            }
        }
    }
}

impl<F: Future<Output = Result<T, E>>, T, E: crate::error::Error> TokenSink
    for AsyncTokenSink<F, T, E>
{
    type Error = E;

    fn yield_token(&mut self, token: Token<'_>) -> Result<bool, Self::Error> {
        if self.4.is_none() {
            self.1.set(Some(token.into()));

            let w = Waker::from(self.3.clone());
            let mut cx = Context::from_waker(&w);

            match self.0.as_mut().poll(&mut cx) {
                Poll::Pending => Ok(true),
                Poll::Ready(ret @ Ok(_)) => {
                    self.4 = Some(ret);
                    Ok(false)
                }
                Poll::Ready(Err(err)) => Err(err),
            }
        } else {
            Err(E::invalid_token(token, Some(TokenTypes::EMPTY)))
        }
    }

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        self.2.get()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new() {
        let mut sink = AsyncTokenSink::new(
            |mut tokens: AsyncTokenReader<crate::error::TokenError>| async move {
                tokens.next(None).await;

                Ok(())
            },
        )
        .unwrap();

        assert!(!sink.yield_token(Token::Bool(true)).unwrap());
        sink.end().unwrap();
    }
}
