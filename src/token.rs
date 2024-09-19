//! Definitions of the tokens and metadata.
//!
//! In general, one piece of Rust data is one token. Structured data
//! uses start/end pairs, and may use special tokens in-between,
//! e.g. for a struct field name.

macro_rules! define_tokens {
    ($($id:ident $(=> $ref:tt => $own:tt)?),*$(,)?) => {
        /// The type of a token, excluding any value or metadata.
        ///
        /// This is used by sinks to declare expected tokens.
        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        pub enum TokenType {
            $($id),*
        }

        /// The tokens and their data. These are borrowed, generally
        /// only valid until the end of
        /// [yield_token](crate::TokenSink::yield_token). See also
        /// [OwningToken].
        ///
        /// This table lists the nested elements. An element is either
        /// one simple token, or a nested start/end pair.
        ///
        /// * [Seq](Token::Seq): Each element is one token.
        /// * [Tuple](Token::Tuple): Each element is one token.
        /// * [Struct](Token::Struct): Each field is one [Token::Field] and one value token, in that order.
        #[derive(Clone, Debug, Eq, PartialEq)]
        pub enum Token<'a> {
            $($id $($ref)?),*
        }

        /// The tokens and their data. The data is owned, making it
        /// useful for storage or sending to another thread. Prefer
        /// using [Token] to avoid duplicating data.
        #[derive(Debug, Eq, PartialEq)]
        pub enum OwningToken {
            $($id $($own)?),*
        }

        /// Converts an integer representation of the token type into
        /// [TokenType]. Returns `Err(())` if the value is invalid.
        impl TryFrom<u64> for TokenType {
            type Error = ();

            fn try_from(i: u64) -> Result<Self, Self::Error> {
                Ok(match i {
                    $(x if x == TokenType::$id as u64 => Self::$id,)*
                    _ => return Err(()),
                })
            }
        }
    }
}

define_tokens! {
    Bool       => (bool)           => (bool),
    U32        => (u32)            => (u32),
    Seq        => (SeqMeta)        => (SeqMeta),
    EndSeq,
    Tuple      => (TupleMeta)      => (TupleMeta),
    EndTuple,
    Struct     => (StructMeta<'a>) => (OwningStructMeta),
    Field      => (&'a str)        => (String),
    EndStruct,
}

/// A set of [TokenType] values.
///
/// Implemented as a bit set.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct TokenTypes(u64);

impl TokenTypes {
    /// The empty set of token types.
    pub const EMPTY: TokenTypes = TokenTypes(0);

    /// All token types.
    pub const ALL: TokenTypes = TokenTypes(!0);

    /// Constructs a new set with a single token type.
    pub fn new(tt: TokenType) -> Self {
        Self(1 << (tt as u64))
    }

    /// Constructs a new set from an iterator of token types.
    pub fn from_iter<I: Iterator<Item = TokenType>>(it: I) -> Self {
        let mut v = 0u64;

        for tt in it {
            v |= 1 << (tt as u64);
        }

        Self(v)
    }

    /// Constructs a new set by adding a type to self.
    pub fn with(self, tt: TokenType) -> Self {
        Self(self.0 | (1 << (tt as u64)))
    }

    /// Constructs a new set by removing a type to self.
    pub fn without(self, tt: TokenType) -> Self {
        Self(self.0 & !(1 << (tt as u64)))
    }

    /// Constructs a new set as a union of two sets.
    pub fn union(self, tt: TokenTypes) -> Self {
        Self(self.0 | tt.0)
    }

    /// Constructs a new set as the difference between self and `tt`.
    pub fn subtract(self, tt: TokenTypes) -> Self {
        Self(self.0 & !tt.0)
    }
}

/// Displays the set as pipe-separated tokens: `U32|Seq(...)`.
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
            write!(f, "{:?}", TokenType::try_from(i).unwrap())?;
        }

        Ok(())
    }
}

macro_rules! token_is [
    ($ty:ty) => {
        impl<'a> $ty {
            /// Returns true if this token starts a nested value,
            /// which indicates the use of [yield_start](crate::TokenSink::yield_start).
            pub fn is_start(&self) -> bool {
                match self {
                    Self::Seq(_) => true,
                    Self::Tuple(_) => true,
                    Self::Struct(_) => true,
                    _ => false,
                }
            }

            /// Returns true if this token ends a nested value.
            pub fn is_end(&self) -> bool {
                match self {
                    Self::EndSeq => true,
                    Self::EndTuple => true,
                    Self::EndStruct => true,
                    _ => false,
                }
            }
        }
    }
];

token_is![Token<'a>];
token_is![OwningToken];

/// Creates an owning token from a [Token].
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

/// Creates a reference to data in an [OwningToken].
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

/// Metadata about a sequence of elements.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SeqMeta {
    pub size_hint: Option<usize>,
}

/// Metadata about a tuple of elements.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TupleMeta {
    pub size_hint: Option<usize>,
}

/// Metadata about a structure of named fields.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StructMeta<'a> {
    pub size_hint: Option<usize>,
    pub fields: Option<&'a [&'a str]>,
}

/// Metadata (owned variant) about a structure of named fields.
#[derive(Debug, Eq, PartialEq)]
pub struct OwningStructMeta {
    pub size_hint: Option<usize>,
    pub fields: Option<Vec<String>>,
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
