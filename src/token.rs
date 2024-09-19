use crate::meta::*;

macro_rules! define_tokens {
    ($($id:ident $(=> $ref:tt => $own:tt)?),*$(,)?) => {
        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        pub enum TokenType {
            $($id),*
        }

        #[derive(Clone, Debug, Eq, PartialEq)]
        pub enum Token<'a> {
            $($id $($ref)?),*
        }

        #[derive(Debug, Eq, PartialEq)]
        pub enum OwningToken {
            $($id $($own)?),*
        }

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
            write!(f, "{:?}", TokenType::try_from(i).unwrap())?;
        }

        Ok(())
    }
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
