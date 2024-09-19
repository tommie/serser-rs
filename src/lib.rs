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

#[derive(Clone, Copy, Eq, PartialEq)]
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

#[derive(Clone, Debug, Eq, PartialEq)]
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

impl<'a> From<Token<'a>> for OwningToken {
    fn from(v: Token<'a>) -> Self {
        match v {
            Token::Bool(v) => Self::Bool(v),
            Token::U32(v) => Self::U32(v),
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StructMeta<'a> {
    size_hint: Option<usize>,
    fields: Option<&'a [&'a str]>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct OwningStructMeta {
    size_hint: Option<usize>,
    fields: Option<Vec<String>>,
}

impl<'a> From<StructMeta<'a>> for OwningStructMeta {
    fn from(v: StructMeta<'a>) -> Self {
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

pub trait Error {
    /// The token sink received a token it cannot process.
    fn invalid_token(token: Token<'_>, expected: Option<TokenTypes>) -> Self;

    /// The token sink is missing tokens, and the value reconstruction
    /// is not complete.
    fn unexpected_end(expected: Option<TokenTypes>) -> Self;
}

#[derive(Debug, Eq, PartialEq)]
pub enum E {
    InvalidToken(OwningToken, Option<TokenTypes>),
    UnexpectedEnd(Option<TokenTypes>),
}

impl Error for E {
    fn invalid_token(token: Token<'_>, expected: Option<TokenTypes>) -> Self {
        E::InvalidToken(token.into(), expected)
    }

    fn unexpected_end(expected: Option<TokenTypes>) -> Self {
        E::UnexpectedEnd(expected)
    }
}

#[derive(Debug)]
pub struct NoError;

impl Error for NoError {
    fn invalid_token(_token: Token<'_>, _expected: Option<TokenTypes>) -> Self {
        unreachable!()
    }

    fn unexpected_end(_expected: Option<TokenTypes>) -> Self {
        unreachable!()
    }
}

pub trait TokenSink {
    type Error: Error;

    // Returns true if it doesn't expect more tokens.
    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error>;

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        None
    }
}

#[derive(Debug)]
pub enum PrintError<DE: Error> {
    Downstream(DE),
    Print(std::io::Error),
}

impl<DE: Error> Error for PrintError<DE> {
    fn invalid_token(token: Token<'_>, expected: Option<TokenTypes>) -> Self {
        Self::Downstream(DE::invalid_token(token, expected))
    }

    fn unexpected_end(expected: Option<TokenTypes>) -> Self {
        Self::Downstream(DE::unexpected_end(expected))
    }
}

pub struct PrintingTokenSink<'a, S: TokenSink, W: std::io::Write>(&'a mut S, &'a str, W, usize);

impl<'a, S: TokenSink, W: std::io::Write> PrintingTokenSink<'a, S, W> {
    pub fn new(sink: &'a mut S, w: W, prefix: &'a str) -> Self {
        Self(sink, prefix, w, 0)
    }

    fn print_token(&mut self, token: &Token<'_>) -> std::io::Result<()> {
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
        )?;

        if token.is_start() {
            self.3 += 1;
        }

        Ok(())
    }
}

impl<'a, S: TokenSink, W: std::io::Write> TokenSink for PrintingTokenSink<'a, S, W> {
    type Error = PrintError<S::Error>;

    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error> {
        self.print_token(&token)
            .map_err(|err| PrintError::Print(err))?;

        self.0
            .yield_token(token)
            .map_err(|err| PrintError::Downstream(err))
    }

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        self.0.expect_tokens()
    }
}

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
    sink.yield_token(Token::Seq(SeqMeta { size_hint }))?;
    for elem in it {
        elem.into_tokens(sink)?;
    }
    sink.yield_token(Token::EndSeq).map(|_| ())
}

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

#[derive(Debug, Eq, PartialEq)]
struct TokenVec(Vec<OwningToken>);

impl TokenVec {
    pub fn new() -> Self {
        TokenVec(Vec::new())
    }

    pub fn from_iter<'a, I: Iterator<Item = &'a Token<'a>>>(it: I) -> Self {
        TokenVec(it.map(|t| t.clone().into()).collect())
    }
}

impl TokenSink for TokenVec {
    type Error = NoError;

    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error> {
        self.0.push(token.into());

        Ok(false)
    }
}

impl<'a> IntoTokens for TokenVec {
    fn into_tokens<S: TokenSink>(&self, sink: &mut S) -> Result<(), S::Error> {
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

    #[test]
    fn test_from_end() {
        let tokens = TokenVec(vec![]);
        assert_eq!(
            bool::from_tokens(tokens).unwrap_err(),
            E::unexpected_end(None)
        );
    }

    #[test]
    fn test_bool_from_invalid() {
        let tokens = TokenVec(vec![OwningToken::U32(42)]);
        assert_eq!(
            bool::from_tokens(tokens).unwrap_err(),
            E::invalid_token(Token::U32(42), Some(TokenTypes::new(TokenType::Bool)))
        );
    }
}
