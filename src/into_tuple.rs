use super::into::IntoTokens;
use super::token::Token;
use super::token::TupleMeta;
use super::TokenSink;

impl IntoTokens for () {
    fn into_tokens<S: TokenSink>(&self, sink: &mut S) -> Result<(), S::Error> {
        sink.yield_token(Token::Unit).map(|_| ())
    }
}

macro_rules! tuple_into_tokens (
    ($n:literal => $($ty:ident $_:ident $i:tt),*) => {
        impl<$($ty: IntoTokens),*> IntoTokens for ($($ty,)*) {
            fn into_tokens<S: TokenSink>(&self, sink: &mut S) -> Result<(), S::Error> {
                sink.yield_token(Token::Tuple(TupleMeta {
                    size_hint: Some($n),
                }))?;
                $(
                    $ty::into_tokens(&self.$i, sink)?;
                )*
                sink.yield_token(Token::EndTuple).map(|_| ())
            }
        }
    };
);

tuple_into_tokens!( 1 => T0 f0 0);
tuple_into_tokens!( 2 => T0 f0 0, T1 f1 1);
tuple_into_tokens!( 3 => T0 f0 0, T1 f1 1, T2 f2 2);
tuple_into_tokens!( 4 => T0 f0 0, T1 f1 1, T2 f2 2, T3 f3 3);
tuple_into_tokens!( 5 => T0 f0 0, T1 f1 1, T2 f2 2, T3 f3 3, T4 f4 4);
tuple_into_tokens!( 6 => T0 f0 0, T1 f1 1, T2 f2 2, T3 f3 3, T4 f4 4, T5 f5 5);
tuple_into_tokens!( 7 => T0 f0 0, T1 f1 1, T2 f2 2, T3 f3 3, T4 f4 4, T5 f5 5, T6 f6 6);
tuple_into_tokens!( 8 => T0 f0 0, T1 f1 1, T2 f2 2, T3 f3 3, T4 f4 4, T5 f5 5, T6 f6 6, T7 f7 7);
tuple_into_tokens!( 9 => T0 f0 0, T1 f1 1, T2 f2 2, T3 f3 3, T4 f4 4, T5 f5 5, T6 f6 6, T7 f7 7, T8 f8 8);
tuple_into_tokens!(10 => T0 f0 0, T1 f1 1, T2 f2 2, T3 f3 3, T4 f4 4, T5 f5 5, T6 f6 6, T7 f7 7, T8 f8 8, T9 f9 9);
tuple_into_tokens!(11 => T0 f0 0, T1 f1 1, T2 f2 2, T3 f3 3, T4 f4 4, T5 f5 5, T6 f6 6, T7 f7 7, T8 f8 8, T9 f9 9, T10 f10 10);
tuple_into_tokens!(12 => T0 f0 0, T1 f1 1, T2 f2 2, T3 f3 3, T4 f4 4, T5 f5 5, T6 f6 6, T7 f7 7, T8 f8 8, T9 f9 9, T10 f10 10, T11 f11 11);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::*;
    use crate::vec::*;

    #[test]
    fn test_into_tokens_unit() {
        let mut got = TokenVec::new();
        ().into_tokens(&mut got).unwrap();
        assert_eq!(got.into_vec(), vec![OwningToken::Unit]);
    }

    #[test]
    fn test_into_tokens_tuple1() {
        let mut got = TokenVec::new();
        (42u32,).into_tokens(&mut got).unwrap();
        assert_eq!(
            got.into_vec(),
            vec![
                OwningToken::Tuple(TupleMeta { size_hint: Some(1) }),
                OwningToken::U32(42),
                OwningToken::EndTuple,
            ]
        );
    }

    #[test]
    fn test_into_tokens_tuple2() {
        let mut got = TokenVec::new();
        (42u32, "Hello").into_tokens(&mut got).unwrap();
        assert_eq!(
            got.into_vec(),
            vec![
                OwningToken::Tuple(TupleMeta { size_hint: Some(2) }),
                OwningToken::U32(42),
                OwningToken::Str("Hello".to_owned()),
                OwningToken::EndTuple,
            ]
        );
    }

    #[test]
    fn test_into_tokens_tuple3() {
        let mut got = TokenVec::new();
        (42u32, "Hello", true).into_tokens(&mut got).unwrap();
        assert_eq!(
            got.into_vec(),
            vec![
                OwningToken::Tuple(TupleMeta { size_hint: Some(3) }),
                OwningToken::U32(42),
                OwningToken::Str("Hello".to_owned()),
                OwningToken::Bool(true),
                OwningToken::EndTuple,
            ]
        );
    }
}
