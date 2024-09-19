#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TokenType {
    Bool,
    U32,
    Seq,
    EndSeq,
    Tuple,
    EndTuple,
    Struct,
    Field,
    EndStruct,
}

#[derive(Clone, Copy)]
pub struct TokenTypes(pub u64);

impl TokenTypes {
    pub fn new(tt: TokenType) -> Self {
        Self(1 << (tt as u64))
    }

    pub fn from_iter<I: Iterator<Item = TokenType>>(it: I) -> Self {
        let mut v = 0u64;

        for tt in it {
            v |= 1 << (tt as u64);
        }

        Self(v)
    }

    pub fn with(self, tt: TokenType) -> Self {
        Self(self.0 | (1 << (tt as u64)))
    }

    pub fn without(self, tt: TokenType) -> Self {
        Self(self.0 & !(1 << (tt as u64)))
    }

    pub fn union(self, tt: TokenTypes) -> Self {
        Self(self.0 | tt.0)
    }

    pub fn subtract(self, tt: TokenTypes) -> Self {
        Self(self.0 & !tt.0)
    }
}

impl std::fmt::Debug for TokenTypes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut found = false;

        for i in 0..64u64 {
            if self.0 & (1 << i) == 0 {
                continue;
            }

            if found {
                write!(f, "|")?;
            } else {
                found = true;
            }
            write!(
                f,
                "{:?}",
                match i {
                    x if x == TokenType::Bool as u64 => TokenType::Bool,
                    x if x == TokenType::U32 as u64 => TokenType::U32,
                    x if x == TokenType::Seq as u64 => TokenType::Seq,
                    x if x == TokenType::EndSeq as u64 => TokenType::EndSeq,
                    x if x == TokenType::Tuple as u64 => TokenType::Tuple,
                    x if x == TokenType::EndTuple as u64 => TokenType::EndTuple,
                    x if x == TokenType::Struct as u64 => TokenType::Struct,
                    x if x == TokenType::Field as u64 => TokenType::Field,
                    x if x == TokenType::EndStruct as u64 => TokenType::EndStruct,
                    _ => unreachable!(),
                }
            )?;
        }

        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Token<'a> {
    Bool(bool),
    U32(u32),
    Seq(SeqMeta),
    EndSeq,
    Tuple(TupleMeta),
    EndTuple,
    Struct(StructMeta<'a>),
    Field(&'a str),
    EndStruct,
}

impl<'a> Token<'a> {
    pub fn is_start(&self) -> bool {
        match self {
            Self::Seq(_) => true,
            Self::Tuple(_) => true,
            Self::Struct(_) => true,
            _ => false,
        }
    }

    pub fn is_end(&self) -> bool {
        match self {
            Self::EndSeq => true,
            Self::EndTuple => true,
            Self::EndStruct => true,
            _ => false,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum OwningToken {
    Bool(bool),
    U32(u32),
    Seq(SeqMeta),
    EndSeq,
    Tuple(TupleMeta),
    EndTuple,
    Struct(OwningStructMeta),
    Field(String),
    EndStruct,
}

impl<'a> From<&'a Token<'a>> for OwningToken {
    fn from(v: &'a Token<'a>) -> Self {
        match v {
            Token::Bool(v) => Self::Bool(*v),
            Token::U32(v) => Self::U32(*v),
            Token::Seq(meta) => Self::Seq(meta.clone()),
            Token::EndSeq => Self::EndSeq,
            Token::Tuple(meta) => Self::Tuple(meta.clone()),
            Token::EndTuple => Self::EndTuple,
            Token::Struct(meta) => Self::Struct(meta.into()),
            Token::Field(name) => Self::Field((*name).to_owned()),
            Token::EndStruct => Self::EndStruct,
        }
    }
}

impl<'a> From<&'a OwningToken> for Token<'a> {
    fn from(v: &'a OwningToken) -> Self {
        match v {
            OwningToken::Bool(v) => Self::Bool(*v),
            OwningToken::U32(v) => Self::U32(*v),
            OwningToken::Seq(meta) => Self::Seq(meta.clone()),
            OwningToken::EndSeq => Self::EndSeq,
            OwningToken::Tuple(meta) => Self::Tuple(meta.clone()),
            OwningToken::EndTuple => Self::EndTuple,
            OwningToken::Struct(meta) => Self::Struct(meta.into()),
            OwningToken::Field(name) => Self::Field(name.as_str()),
            OwningToken::EndStruct => Self::EndStruct,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SeqMeta {
    size_hint: Option<usize>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TupleMeta {
    size_hint: Option<usize>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct StructMeta<'a> {
    size_hint: Option<usize>,
    fields: Option<&'a [&'a str]>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct OwningStructMeta {
    size_hint: Option<usize>,
    fields: Option<Vec<String>>,
}

impl<'a> From<&'a StructMeta<'a>> for OwningStructMeta {
    fn from(v: &'a StructMeta<'a>) -> Self {
        Self {
            size_hint: v.size_hint,
            fields: v
                .fields
                .map(|fields| fields.into_iter().map(|k| (*k).to_owned()).collect()),
        }
    }
}

impl<'a> From<&'a OwningStructMeta> for StructMeta<'a> {
    fn from(v: &'a OwningStructMeta) -> Self {
        Self {
            size_hint: v.size_hint,
            fields: None, // TODO
        }
    }
}

pub trait TokenSink {
    type Error;

    // Returns true if it doesn't expect more tokens.
    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error>;

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        None
    }
}

pub struct PrintingTokenSink<'a, S: TokenSink, W: std::io::Write>(&'a mut S, &'a str, W, usize);

impl<'a, S: TokenSink, W: std::io::Write> PrintingTokenSink<'a, S, W> {
    pub fn new(sink: &'a mut S, w: W, prefix: &'a str) -> Self {
        Self(sink, prefix, w, 0)
    }

    fn print_token(&mut self, token: &Token<'_>) {
        if token.is_end() {
            self.3 -= 1;
        }

        writeln!(
            self.2,
            "{}{:indent$}{:?}",
            self.1,
            "",
            token,
            indent = 2 * self.3
        )
        .unwrap();

        if token.is_start() {
            self.3 += 1;
        }
    }
}

impl<'a, S: TokenSink, W: std::io::Write> TokenSink for PrintingTokenSink<'a, S, W> {
    type Error = S::Error;

    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error> {
        self.print_token(&token);

        self.0.yield_token(token)
    }

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        self.0.expect_tokens()
    }
}

pub trait IntoTokens {
    type Error;

    fn into_tokens<S: TokenSink<Error = Self::Error>>(
        &self,
        sink: &mut S,
    ) -> Result<(), Self::Error>;
}

impl IntoTokens for bool {
    type Error = ();

    fn into_tokens<S: TokenSink<Error = Self::Error>>(
        &self,
        sink: &mut S,
    ) -> Result<(), Self::Error> {
        sink.yield_token(Token::Bool(*self)).map(|_| ())
    }
}

impl IntoTokens for u32 {
    type Error = ();

    fn into_tokens<S: TokenSink<Error = Self::Error>>(
        &self,
        sink: &mut S,
    ) -> Result<(), Self::Error> {
        sink.yield_token(Token::U32(*self)).map(|_| ())
    }
}

impl<'a, K, V> IntoTokens for (K, V)
where
    K: IntoTokens<Error = ()>,
    V: IntoTokens<Error = ()>,
{
    type Error = ();

    fn into_tokens<S: TokenSink<Error = Self::Error>>(
        &self,
        sink: &mut S,
    ) -> Result<(), Self::Error> {
        sink.yield_token(Token::Tuple(TupleMeta { size_hint: Some(2) }))?;
        self.0.into_tokens(sink)?;
        self.1.into_tokens(sink)?;
        sink.yield_token(Token::EndTuple).map(|_| ())
    }
}

impl<T> IntoTokens for [T]
where
    T: IntoTokens<Error = ()>,
{
    type Error = ();

    fn into_tokens<S: TokenSink<Error = Self::Error>>(
        &self,
        sink: &mut S,
    ) -> Result<(), Self::Error> {
        iter_into_tokens(self.iter(), sink).map(|_| ())
    }
}

impl<T> IntoTokens for Vec<T>
where
    T: IntoTokens<Error = ()>,
{
    type Error = ();

    fn into_tokens<S: TokenSink<Error = Self::Error>>(
        &self,
        sink: &mut S,
    ) -> Result<(), Self::Error> {
        iter_into_tokens(self.iter(), sink).map(|_| ())
    }
}

pub fn iter_into_tokens<'a, I: Iterator<Item = &'a T>, S: TokenSink<Error = E>, T, E>(
    it: I,
    sink: &mut S,
) -> Result<(), E>
where
    T: 'a + IntoTokens<Error = E>,
{
    let (_, size_hint) = it.size_hint();
    sink.yield_token(Token::Seq(SeqMeta { size_hint }))?;
    for elem in it {
        elem.into_tokens(sink)?;
    }
    sink.yield_token(Token::EndSeq).map(|_| ())
}

pub trait FromTokenSink: Sized {
    type Error;
    type Sink: TokenSink<Error = Self::Error>;

    fn new_sink() -> Self::Sink;
    fn from_sink(sink: Self::Sink) -> Option<Self>;
}

pub trait FromTokens: FromTokenSink {
    fn from_tokens<I: IntoTokens<Error = Self::Error>>(into: I) -> Result<Self, Self::Error>;
}

impl<T: FromTokenSink<Error = ()>> FromTokens for T {
    fn from_tokens<I: IntoTokens<Error = Self::Error>>(into: I) -> Result<Self, Self::Error> {
        let mut sink = Self::new_sink();
        into.into_tokens(&mut sink)?;

        Self::from_sink(sink).ok_or(())
    }
}

pub struct BasicSink<T>(Option<T>);

macro_rules! basic_from_tokens [
    ($va:path => $ty:ty) => {
        impl TokenSink for BasicSink<$ty> {
            type Error = ();

            fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error> {
                match token {
                    $va(v) => {
                        self.0 = Some(v);
                        Ok(true)
                    }
                    _ => Err(()),
                }
            }
        }

        impl FromTokenSink for $ty {
            type Error = ();
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

basic_from_tokens![Token::Bool => bool];
basic_from_tokens![Token::U32 => u32];

pub struct VecSink<T: FromTokenSink<Error = ()>>(Vec<T>, VecSinkState, Option<T::Sink>);

#[derive(Eq, PartialEq)]
pub enum VecSinkState {
    Start,
    NewElem,
    Elem,
    End,
}

impl<T: FromTokenSink<Error = ()>> TokenSink for VecSink<T> {
    type Error = ();

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
            t => {
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

impl<T: FromTokenSink<Error = ()>> FromTokenSink for Vec<T> {
    type Error = ();
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

#[derive(Debug, Eq, PartialEq)]
struct TokenVec(Vec<OwningToken>);

impl TokenVec {
    pub fn new() -> Self {
        TokenVec(Vec::new())
    }

    pub fn from_iter<'a, I: Iterator<Item = &'a Token<'a>>>(it: I) -> Self {
        TokenVec(it.map(|t| t.into()).collect())
    }
}

impl TokenSink for TokenVec {
    type Error = ();

    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error> {
        self.0.push((&token).into());

        Ok(false)
    }
}

impl<'a> IntoTokens for TokenVec {
    type Error = ();

    fn into_tokens<S: TokenSink<Error = Self::Error>>(
        &self,
        sink: &mut S,
    ) -> Result<(), Self::Error> {
        for token in self.0.iter() {
            sink.yield_token(token.into())?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_printing_token_sink() {
        let mut got = TokenVec::new();
        let mut output = Vec::new();
        vec![42u32]
            .into_tokens(&mut PrintingTokenSink::new(&mut got, &mut output, "T: "))
            .unwrap();
        assert_eq!(
            String::from_utf8(output).unwrap(),
            "T: Seq(SeqMeta { size_hint: Some(1) })\nT:   U32(42)\nT: EndSeq\n"
        );
    }

    #[test]
    fn test_bool_into() {
        let mut got = TokenVec::new();
        true.into_tokens(&mut got).unwrap();
        assert_eq!(got.0, vec![OwningToken::Bool(true)]);
    }

    #[test]
    fn test_u32_into() {
        let mut got = TokenVec::new();
        42u32.into_tokens(&mut got).unwrap();
        assert_eq!(got.0, vec![OwningToken::U32(42)]);
    }

    #[test]
    fn test_tuple_into() {
        let mut got = TokenVec::new();
        (42u32, true).into_tokens(&mut got).unwrap();
        assert_eq!(
            got.0,
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
            got.0,
            vec![
                OwningToken::Seq(SeqMeta { size_hint: Some(1) }),
                OwningToken::U32(42),
                OwningToken::EndSeq,
            ],
        );
    }

    #[test]
    fn test_bool_from() {
        let tokens = TokenVec(vec![OwningToken::Bool(true)]);
        assert_eq!(bool::from_tokens(tokens).unwrap(), true);
    }

    #[test]
    fn test_vec_u32_from() {
        let tokens = TokenVec(vec![
            OwningToken::Seq(SeqMeta { size_hint: Some(1) }),
            OwningToken::U32(42),
            OwningToken::EndSeq,
        ]);
        assert_eq!(Vec::<u32>::from_tokens(tokens).unwrap(), vec![42]);
    }

    #[test]
    fn test_vec_vec_u32_from() {
        let tokens = TokenVec(vec![
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
}
