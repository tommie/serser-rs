//! Definitions of the tokens and metadata.
//!
//! In general, one piece of Rust data is one token. Structured data
//! uses start/end pairs, and may use special tokens in-between,
//! e.g. for a struct field name.

use std::convert::TryFrom;

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
        /// * [Enum](Token::Enum): First is a [Token::Variant], then elements of data.
        /// * [Seq](Token::Seq): Each element is one token.
        /// * [Tuple](Token::Tuple): Each element is one token.
        /// * [Struct](Token::Struct): Each field is one [Token::Field] and one value token, in that order.
        #[derive(Clone, Debug, PartialEq)]
        pub enum Token<'a> {
            $($id $($ref)?),*
        }

        /// The tokens and their data. The data is owned, making it
        /// useful for storage or sending to another thread. Prefer
        /// using [Token] to avoid duplicating data.
        #[derive(Clone, Debug, PartialEq)]
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
    Unit,
    Bool       => (bool)            => (bool),
    U8         => (u8)              => (u8),
    U16        => (u16)             => (u16),
    U32        => (u32)             => (u32),
    U64        => (u64)             => (u64),
    U128       => (u128)            => (u128),
    Usize      => (usize)           => (usize),
    I8         => (i8)              => (i8),
    I16        => (i16)             => (i16),
    I32        => (i32)             => (i32),
    I64        => (i64)             => (i64),
    I128       => (i128)            => (i128),
    Isize      => (isize)           => (isize),
    F32        => (f32)             => (f32),
    F64        => (f64)             => (f64),
    Char       => (char)            => (char),
    Str        => (&'a str)         => (String),

    Seq        => (SeqMeta)         => (SeqMeta),
    EndSeq,
    Tuple      => (TupleMeta)       => (TupleMeta),
    EndTuple,
    Struct     => (StructMeta<'a>)  => (OwningStructMeta),
    Field      => (&'a str)         => (String),
    EndStruct,
    Enum       => (EnumMeta<'a>)    => (OwningEnumMeta),
    Variant    => (EnumVariant<'a>) => (OwningEnumVariant),
    EndEnum,
}

// These are unsound because of f32/f64.
impl<'a> Eq for Token<'a> {}
impl Eq for OwningToken {}

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

    pub fn is_empty(&self) -> bool {
        self.0 == 0
    }

    pub fn contains(&self, tt: TokenType) -> bool {
        self.0 & (1 << tt as u64) != 0
    }

    pub fn iter(&self) -> TypeIter {
        TypeIter(*self, 0)
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

pub struct TypeIter(TokenTypes, u32);

impl Iterator for TypeIter {
    type Item = TokenType;

    fn next(&mut self) -> Option<Self::Item> {
        while self.1 < 64 {
            let tt = TokenType::try_from(self.1 as u64);
            self.1 += 1;
            if let Ok(tt) = tt {
                if self.0.contains(tt) {
                    return Some(tt);
                }
            }
        }

        None
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
                    Self::Enum(_) => true,
                    _ => false,
                }
            }

            /// Returns true if this token ends a nested value.
            pub fn is_end(&self) -> bool {
                match self {
                    Self::EndSeq => true,
                    Self::EndTuple => true,
                    Self::EndStruct => true,
                    Self::EndEnum => true,
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
            Token::Unit => Self::Unit,
            Token::Bool(v) => Self::Bool(v),
            Token::U8(v) => Self::U8(v),
            Token::U16(v) => Self::U16(v),
            Token::U32(v) => Self::U32(v),
            Token::U64(v) => Self::U64(v),
            Token::U128(v) => Self::U128(v),
            Token::Usize(v) => Self::Usize(v),
            Token::I8(v) => Self::I8(v),
            Token::I16(v) => Self::I16(v),
            Token::I32(v) => Self::I32(v),
            Token::I64(v) => Self::I64(v),
            Token::I128(v) => Self::I128(v),
            Token::Isize(v) => Self::Isize(v),
            Token::F32(v) => Self::F32(v),
            Token::F64(v) => Self::F64(v),
            Token::Char(v) => Self::Char(v),
            Token::Str(v) => Self::Str(v.to_owned()),

            Token::Seq(meta) => Self::Seq(meta.clone()),
            Token::EndSeq => Self::EndSeq,
            Token::Tuple(meta) => Self::Tuple(meta.clone()),
            Token::EndTuple => Self::EndTuple,
            Token::Struct(meta) => Self::Struct(meta.into()),
            Token::Field(name) => Self::Field((*name).to_owned()),
            Token::EndStruct => Self::EndStruct,
            Token::Enum(meta) => Self::Enum(meta.into()),
            Token::Variant(name) => Self::Variant(name.into()),
            Token::EndEnum => Self::EndEnum,
        }
    }
}

/// Creates a reference to data in an [OwningToken].
impl<'a> From<&'a OwningToken> for Token<'a> {
    fn from(v: &'a OwningToken) -> Self {
        match v {
            OwningToken::Unit => Self::Unit,
            OwningToken::Bool(v) => Self::Bool(*v),
            OwningToken::U8(v) => Self::U8(*v),
            OwningToken::U16(v) => Self::U16(*v),
            OwningToken::U32(v) => Self::U32(*v),
            OwningToken::U64(v) => Self::U64(*v),
            OwningToken::U128(v) => Self::U128(*v),
            OwningToken::Usize(v) => Self::Usize(*v),
            OwningToken::I8(v) => Self::I8(*v),
            OwningToken::I16(v) => Self::I16(*v),
            OwningToken::I32(v) => Self::I32(*v),
            OwningToken::I64(v) => Self::I64(*v),
            OwningToken::I128(v) => Self::I128(*v),
            OwningToken::Isize(v) => Self::Isize(*v),
            OwningToken::F32(v) => Self::F32(*v),
            OwningToken::F64(v) => Self::F64(*v),
            OwningToken::Char(v) => Self::Char(*v),
            OwningToken::Str(v) => Self::Str(v.as_str()),

            OwningToken::Seq(meta) => Self::Seq(meta.clone()),
            OwningToken::EndSeq => Self::EndSeq,
            OwningToken::Tuple(meta) => Self::Tuple(meta.clone()),
            OwningToken::EndTuple => Self::EndTuple,
            OwningToken::Struct(meta) => Self::Struct(meta.into()),
            OwningToken::Field(name) => Self::Field(name.as_str()),
            OwningToken::EndStruct => Self::EndStruct,
            OwningToken::Enum(meta) => Self::Enum(meta.into()),
            OwningToken::Variant(name) => Self::Variant(name.into()),
            OwningToken::EndEnum => Self::EndEnum,
        }
    }
}

/// A specific enum variant, whether represented as a string or
/// integer.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum EnumVariant<'a> {
    Str(&'a str),
    Usize(usize),
}

/// A specific enum variant (owned version,) whether represented as a
/// string or integer.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum OwningEnumVariant {
    Str(String),
    Usize(usize),
}

impl<'a> From<EnumVariant<'a>> for OwningEnumVariant {
    fn from(v: EnumVariant<'a>) -> Self {
        match v {
            EnumVariant::Str(s) => Self::Str(s.to_owned()),
            EnumVariant::Usize(i) => Self::Usize(i),
        }
    }
}

impl<'a> From<&'a EnumVariant<'a>> for OwningEnumVariant {
    fn from(v: &'a EnumVariant<'a>) -> Self {
        match *v {
            EnumVariant::Str(s) => Self::Str(s.to_owned()),
            EnumVariant::Usize(i) => Self::Usize(i),
        }
    }
}

impl<'a> From<&'a OwningEnumVariant> for EnumVariant<'a> {
    fn from(v: &'a OwningEnumVariant) -> Self {
        match v {
            OwningEnumVariant::Str(s) => Self::Str(s.as_str()),
            OwningEnumVariant::Usize(i) => Self::Usize(*i),
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
    pub fields: Option<&'a [&'a str]>,
}

/// Metadata (owned variant) about a structure of named fields.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct OwningStructMeta {
    pub fields: Option<Vec<String>>,
}

impl<'a> From<StructMeta<'a>> for OwningStructMeta {
    fn from(v: StructMeta<'a>) -> Self {
        Self {
            fields: v
                .fields
                .map(|fields| fields.into_iter().map(|k| (*k).to_owned()).collect()),
        }
    }
}

impl<'a> From<&'a OwningStructMeta> for StructMeta<'a> {
    fn from(_v: &'a OwningStructMeta) -> Self {
        Self {
            fields: None, // TODO
        }
    }
}

/// Metadata about a structure of named fields.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EnumMeta<'a> {
    pub variants: Option<&'a [EnumVariant<'a>]>,
}

/// Metadata (owned variant) about a structure of named fields.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct OwningEnumMeta {
    pub variants: Option<Vec<OwningEnumVariant>>,
}

impl<'a> From<EnumMeta<'a>> for OwningEnumMeta {
    fn from(v: EnumMeta<'a>) -> Self {
        Self {
            variants: v
                .variants
                .map(|variants| variants.into_iter().map(|name| name.into()).collect()),
        }
    }
}

impl<'a> From<&'a OwningEnumMeta> for EnumMeta<'a> {
    fn from(_v: &'a OwningEnumMeta) -> Self {
        Self {
            variants: None, // TODO
        }
    }
}
