use crate::token::*;
use crate::TokenSink;

/// Provides a way to associate appropriate sources with Rust data types.
pub trait IntoTokens {
    /// Yields tokens to the sink, one by one. Any start token (see
    /// [Token::is_start]) must be yielded using
    /// [TokenSink::yield_start]. Other tokens use
    /// [TokenSink::yield_token].
    fn into_tokens<S: TokenSink>(&self, sink: &mut S) -> Result<(), S::Error>;
}

macro_rules! basic_into_tokens [
    ($($id:ident => $ty:ty),*$(,)?) => {
        $(impl IntoTokens for $ty {
            fn into_tokens<S: TokenSink>(&self, sink: &mut S) -> Result<(), S::Error> {
                sink.yield_token(Token::$id(*self)).map(|_| ())
            }
        })*
    };
];

basic_into_tokens![
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
    Bytes => &'_ [u8],
    Str => &'_ str,
];

impl IntoTokens for () {
    fn into_tokens<S: TokenSink>(&self, sink: &mut S) -> Result<(), S::Error> {
        sink.yield_token(Token::Unit).map(|_| ())
    }
}

impl<'a, K, V> IntoTokens for (K, V)
where
    K: IntoTokens,
    V: IntoTokens,
{
    fn into_tokens<S: TokenSink>(&self, sink: &mut S) -> Result<(), S::Error> {
        sink.yield_token(Token::Tuple(TupleMeta { size_hint: Some(2) }))?;
        self.0.into_tokens(sink)?;
        self.1.into_tokens(sink)?;
        sink.yield_token(Token::EndTuple).map(|_| ())
    }
}

impl<T> IntoTokens for [T]
where
    T: IntoTokens,
{
    fn into_tokens<S: TokenSink>(&self, sink: &mut S) -> Result<(), S::Error> {
        iter_into_tokens(self.iter(), sink).map(|_| ())
    }
}

impl<T> IntoTokens for Vec<T>
where
    T: IntoTokens,
{
    fn into_tokens<S: TokenSink>(&self, sink: &mut S) -> Result<(), S::Error> {
        iter_into_tokens(self.iter(), sink).map(|_| ())
    }
}

/// Yields the contents of an iterator as a [Token::Seq].
pub fn iter_into_tokens<'a, I: Iterator<Item = &'a T>, S: TokenSink, T>(
    it: I,
    sink: &mut S,
) -> Result<(), S::Error>
where
    T: 'a + IntoTokens,
{
    let (_, size_hint) = it.size_hint();
    let mut subsink = sink.yield_start(Token::Seq(SeqMeta { size_hint }))?;

    for elem in it {
        elem.into_tokens(&mut subsink)?;
    }

    sink.yield_end(Token::EndSeq, subsink)
}

impl<'a> IntoTokens for Token<'a> {
    fn into_tokens<S: TokenSink>(&self, sink: &mut S) -> Result<(), S::Error> {
        sink.yield_token(self.clone())?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vec::*;

    #[test]
    fn test_bool_into() {
        let mut got = TokenVec::new();
        true.into_tokens(&mut got).unwrap();
        assert_eq!(got.into_vec(), vec![OwningToken::Bool(true)]);
    }

    #[test]
    fn test_u32_into() {
        let mut got = TokenVec::new();
        42u32.into_tokens(&mut got).unwrap();
        assert_eq!(got.into_vec(), vec![OwningToken::U32(42)]);
    }

    #[test]
    fn test_tuple_into() {
        let mut got = TokenVec::new();
        (42u32, true).into_tokens(&mut got).unwrap();
        assert_eq!(
            got.into_vec(),
            vec![
                OwningToken::Tuple(TupleMeta { size_hint: Some(2) }),
                OwningToken::U32(42),
                OwningToken::Bool(true),
                OwningToken::EndTuple,
            ]
        );
    }

    #[test]
    fn test_vec_u32_into() {
        let mut got = TokenVec::new();
        vec![42u32].into_tokens(&mut got).unwrap();
        assert_eq!(
            got.into_vec(),
            vec![
                OwningToken::Seq(SeqMeta { size_hint: Some(1) }),
                OwningToken::U32(42),
                OwningToken::EndSeq,
            ],
        );
    }
}
