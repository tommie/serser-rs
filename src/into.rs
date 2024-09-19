use crate::meta::*;
use crate::token::*;
use crate::TokenSink;

pub trait IntoTokens {
    fn into_tokens<S: TokenSink>(&self, sink: &mut S) -> Result<(), S::Error>;
}

impl IntoTokens for bool {
    fn into_tokens<S: TokenSink>(&self, sink: &mut S) -> Result<(), S::Error> {
        sink.yield_token(Token::Bool(*self)).map(|_| ())
    }
}

impl IntoTokens for u32 {
    fn into_tokens<S: TokenSink>(&self, sink: &mut S) -> Result<(), S::Error> {
        sink.yield_token(Token::U32(*self)).map(|_| ())
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
    subsink.yield_token(Token::EndSeq)?;
    sink.end(subsink);

    Ok(())
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
