use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use base64::prelude::*;

use crate::token::*;
use crate::Error;
use crate::IntoTokens;
use crate::TokenError;
use crate::TokenSink;

/// The error returned from [json_from_tokens].
#[derive(Debug, Eq, PartialEq)]
pub enum WriteError {
    /// The string formatter (or I/O) failed.
    Fmt(fmt::Error),

    /// The token stream was invalid.
    Token(TokenError),
}

impl fmt::Display for WriteError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Fmt(err) => err.fmt(f),
            Self::Token(err) => err.fmt(f),
        }
    }
}

impl std::error::Error for WriteError {}
impl Error for WriteError {
    fn invalid_token(token: Token<'_>, expected: Option<TokenTypes>) -> Self {
        WriteError::Token(TokenError::InvalidToken(token.into(), expected))
    }

    fn unexpected_end(expected: Option<TokenTypes>) -> Self {
        WriteError::Token(TokenError::UnexpectedEnd(expected))
    }
}

/// Writes JSON to `writer` from data in an [IntoTokens].
///
/// The output JSON contains no whitespace.
pub fn json_from_tokens<I: IntoTokens, W: fmt::Write>(
    writer: W,
    into: I,
) -> Result<(), WriteError> {
    let mut sink = JsonTokenSink {
        writer: Rc::new(RefCell::new(writer)),
        state: JsonState::Plain,
    };
    into.into_tokens(&mut sink)?;

    match sink.state {
        JsonState::Done => Ok(()),
        _ => Err(WriteError::unexpected_end(sink.expect_tokens())),
    }
}

struct JsonTokenSink<W: fmt::Write> {
    writer: Rc<RefCell<W>>,
    state: JsonState,
}

impl<W: fmt::Write> JsonTokenSink<W> {
    /// Writes the comma or colon as appropriate before a
    /// token. Updates the state.
    fn write_sep(&mut self, token: &Token<'_>) -> Result<(), WriteError> {
        let mut w = self.writer.borrow_mut();

        match self.state {
            JsonState::Plain => self.state = JsonState::Done,
            JsonState::FirstArrayElement => self.state = JsonState::ArrayElement,
            JsonState::ArrayElement => w.write_str(",").map_err(|err| WriteError::Fmt(err))?,
            JsonState::FirstObjectKey => {
                match token {
                    Token::Field(_) => {}
                    _ => {
                        return Err(WriteError::invalid_token(
                            token.clone(),
                            Some(TokenTypes::new(TokenType::Field)),
                        ))
                    }
                }

                self.state = JsonState::ObjectValue;
            }
            JsonState::ObjectKey => {
                match token {
                    Token::Field(_) => {}
                    _ => {
                        return Err(WriteError::invalid_token(
                            token.clone(),
                            Some(TokenTypes::new(TokenType::Field)),
                        ))
                    }
                }

                w.write_str(",").map_err(|err| WriteError::Fmt(err))?;
                self.state = JsonState::ObjectValue;
            }
            JsonState::ObjectValue => {
                w.write_str(":").map_err(|err| WriteError::Fmt(err))?;
                self.state = JsonState::ObjectKey;
            }
            JsonState::EnumVariant => self.state = JsonState::FirstEnumElement,
            JsonState::FirstEnumElement => self.state = JsonState::EnumElement,
            JsonState::EnumElement => w.write_str(",").map_err(|err| WriteError::Fmt(err))?,
            JsonState::Done => unreachable!(),
        }

        Ok(())
    }

    fn write_bytes(w: &mut W, v: &[u8]) -> Result<(), fmt::Error> {
        w.write_char('"')?;
        w.write_str(BASE64_STANDARD.encode(v).as_str())?;
        w.write_char('"')?;

        Ok(())
    }

    fn write_str(w: &mut W, s: &str) -> Result<(), fmt::Error> {
        let mut start = 0;
        let mut end = 0;

        w.write_char('"')?;

        for c in s.chars() {
            if c == '\\' || c == '"' || c < ' ' {
                w.write_str(&s[start..end])?;

                w.write_char('\\')?;

                match c {
                    '\x07' => w.write_char('b')?,
                    '\x0C' => w.write_char('f')?,
                    '\n' => w.write_char('n')?,
                    '\r' => w.write_char('r')?,
                    '\t' => w.write_char('t')?,
                    c if c < ' ' => write!(w, "u{:04X}", c as u32)?,
                    c => w.write_char(c)?,
                }

                end += c.len_utf8();
                start = end;
            } else {
                end += c.len_utf8();
            }
        }

        if start < end {
            w.write_str(&s[start..end])?;
        }

        w.write_char('"')?;

        Ok(())
    }
}

impl<W: fmt::Write> TokenSink for JsonTokenSink<W> {
    type Error = WriteError;
    type Subsink<'c> = Self where Self: 'c;

    fn yield_start<'c, 'd>(&mut self, token: Token<'c>) -> Result<Self::Subsink<'d>, Self::Error>
    where
        Self: 'd,
    {
        self.write_sep(&token)?;

        let mut w = self.writer.borrow_mut();
        let state = match token {
            Token::Seq(_) => JsonState::FirstArrayElement,
            Token::Tuple(_) => JsonState::FirstArrayElement,
            Token::Struct(_) => JsonState::FirstObjectKey,
            Token::Enum(_) => JsonState::EnumVariant,
            _ => {
                return Err(WriteError::Token(TokenError::InvalidToken(
                    token.into(),
                    None,
                )))
            }
        };

        match state {
            JsonState::FirstArrayElement => w.write_str("["),
            JsonState::FirstObjectKey => w.write_str("{"),
            JsonState::EnumVariant => w.write_str("{"),
            _ => Ok(()),
        }
        .map_err(|err| WriteError::Fmt(err))?;

        Ok(Self {
            writer: self.writer.clone(),
            state,
        })
    }

    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error> {
        if token.is_end() {
            match self.state {
                JsonState::ObjectValue => {
                    return Err(WriteError::Token(TokenError::InvalidToken(
                        token.into(),
                        None,
                    )))
                }
                JsonState::EnumVariant => {
                    return Err(WriteError::Token(TokenError::InvalidToken(
                        token.into(),
                        Some(TokenTypes::new(TokenType::Variant)),
                    )))
                }
                _ => {}
            }

            self.state = JsonState::Done;
        } else {
            self.write_sep(&token)?;
        }

        let mut w = self.writer.borrow_mut();

        match token {
            Token::Unit => w.write_str("null"),
            Token::Bool(v) => write!(w, "{}", v),
            Token::U8(v) => write!(w, "{}", v),
            Token::U16(v) => write!(w, "{}", v),
            Token::U32(v) => write!(w, "{}", v),
            Token::U64(v) => write!(w, "{}", v),
            Token::U128(v) => write!(w, "{}", v),
            Token::Usize(v) => write!(w, "{}", v),
            Token::I8(v) => write!(w, "{}", v),
            Token::I16(v) => write!(w, "{}", v),
            Token::I32(v) => write!(w, "{}", v),
            Token::I64(v) => write!(w, "{}", v),
            Token::I128(v) => write!(w, "{}", v),
            Token::Isize(v) => write!(w, "{}", v),
            Token::F32(v) => write!(w, "{}", v),
            Token::F64(v) => write!(w, "{}", v),
            Token::Char(v) => Self::write_str(&mut w, v.to_string().as_str()),
            Token::Bytes(v) => Self::write_bytes(&mut w, v),
            Token::Str(s) => Self::write_str(&mut w, s),
            Token::Field(s) => Self::write_str(&mut w, s),
            Token::Variant(EnumVariant::Str(s)) => {
                Self::write_str(&mut w, s).map_err(|err| WriteError::Fmt(err))?;
                write!(w, ":[")
            }
            Token::Variant(EnumVariant::Usize(i)) => write!(w, "\"{}\":[", i),

            Token::Seq(_) => unreachable!(),
            Token::Tuple(_) => unreachable!(),
            Token::Struct(_) => unreachable!(),
            Token::Enum(_) => unreachable!(),

            Token::EndSeq => w.write_str("]"),
            Token::EndTuple => w.write_str("]"),
            Token::EndStruct => w.write_str("}"),
            Token::EndEnum => w.write_str("]}"),
        }
        .map_err(|err| WriteError::Fmt(err))?;

        Ok(false)
    }

    fn end<'b>(&mut self, _sink: Self::Subsink<'b>)
    where
        Self: 'b,
    {
    }

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        match self.state {
            JsonState::Plain => None,
            JsonState::FirstArrayElement => None,
            JsonState::ArrayElement => None,
            JsonState::FirstObjectKey => Some(TokenTypes::new(TokenType::Field)),
            JsonState::ObjectKey => Some(TokenTypes::new(TokenType::Field)),
            JsonState::ObjectValue => None,
            JsonState::EnumVariant => Some(TokenTypes::new(TokenType::Variant)),
            JsonState::FirstEnumElement => None,
            JsonState::EnumElement => None,
            JsonState::Done => Some(TokenTypes::EMPTY),
        }
    }
}

#[derive(Debug)]
enum JsonState {
    Plain,
    FirstArrayElement,
    ArrayElement,
    FirstObjectKey,
    ObjectKey,
    ObjectValue,
    EnumVariant,
    FirstEnumElement,
    EnumElement,
    Done,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::TokenVec;

    #[test]
    fn test_from_tokens_plain() {
        let cases = vec![
            (Token::Unit, "null"),
            (Token::Bool(true), "true"),
            (Token::U8(42), "42"),
            (Token::U16(42), "42"),
            (Token::U32(42), "42"),
            (Token::U64(42), "42"),
            (Token::U128(42), "42"),
            (Token::Usize(42), "42"),
            (Token::I8(42), "42"),
            (Token::I16(42), "42"),
            (Token::I32(42), "42"),
            (Token::I64(42), "42"),
            (Token::I128(42), "42"),
            (Token::Isize(42), "42"),
            (Token::Char('a'), "\"a\""),
            (Token::Bytes(b""), "\"\""),
            (Token::Bytes(b"\x00"), "\"AA==\""),
            (Token::Str("hello"), "\"hello\""),
            (Token::Str("\n"), "\"\\n\""),
            (Token::Str("❤️"), "\"❤️\""),
            (Token::Str("\x1F"), "\"\\u001F\""),
            (Token::Str("hello"), "\"hello\""),
        ];

        for (token, want) in cases {
            let mut got = String::new();
            json_from_tokens(&mut got, TokenVec::from_iter(vec![token].into_iter())).unwrap();
            assert_eq!(got, want);
        }
    }

    #[test]
    fn test_from_tokens_seq() {
        let cases = vec![
            (
                vec![Token::Seq(SeqMeta { size_hint: None }), Token::EndSeq],
                "[]",
            ),
            (
                vec![
                    Token::Seq(SeqMeta { size_hint: None }),
                    Token::Bool(true),
                    Token::EndSeq,
                ],
                "[true]",
            ),
            (
                vec![
                    Token::Seq(SeqMeta { size_hint: None }),
                    Token::Bool(true),
                    Token::Bool(false),
                    Token::EndSeq,
                ],
                "[true,false]",
            ),
            (
                vec![
                    Token::Seq(SeqMeta { size_hint: None }),
                    Token::Seq(SeqMeta { size_hint: None }),
                    Token::Bool(true),
                    Token::EndSeq,
                    Token::EndSeq,
                ],
                "[[true]]",
            ),
        ];

        for (tokens, want) in cases {
            let mut got = String::new();
            json_from_tokens(&mut got, TokenVec::from_iter(tokens.into_iter())).unwrap();
            assert_eq!(got, want);
        }
    }

    #[test]
    fn test_from_tokens_tuple() {
        let cases = vec![
            (
                vec![Token::Tuple(TupleMeta { size_hint: None }), Token::EndTuple],
                "[]",
            ),
            (
                vec![
                    Token::Tuple(TupleMeta { size_hint: None }),
                    Token::Bool(true),
                    Token::EndTuple,
                ],
                "[true]",
            ),
            (
                vec![
                    Token::Tuple(TupleMeta { size_hint: None }),
                    Token::Bool(true),
                    Token::Bool(false),
                    Token::EndTuple,
                ],
                "[true,false]",
            ),
            (
                vec![
                    Token::Tuple(TupleMeta { size_hint: None }),
                    Token::Tuple(TupleMeta { size_hint: None }),
                    Token::Bool(true),
                    Token::EndTuple,
                    Token::EndTuple,
                ],
                "[[true]]",
            ),
        ];

        for (tokens, want) in cases {
            let mut got = String::new();
            json_from_tokens(&mut got, TokenVec::from_iter(tokens.into_iter())).unwrap();
            assert_eq!(got, want);
        }
    }

    #[test]
    fn test_from_tokens_struct() {
        let cases = vec![
            (
                vec![Token::Struct(StructMeta { fields: None }), Token::EndStruct],
                "{}",
            ),
            (
                vec![
                    Token::Struct(StructMeta { fields: None }),
                    Token::Field("akey"),
                    Token::Bool(true),
                    Token::EndStruct,
                ],
                "{\"akey\":true}",
            ),
            (
                vec![
                    Token::Struct(StructMeta { fields: None }),
                    Token::Field("akey"),
                    Token::Bool(true),
                    Token::Field("bkey"),
                    Token::Bool(false),
                    Token::EndStruct,
                ],
                "{\"akey\":true,\"bkey\":false}",
            ),
            (
                vec![
                    Token::Struct(StructMeta { fields: None }),
                    Token::Field("akey"),
                    Token::Struct(StructMeta { fields: None }),
                    Token::Field("bkey"),
                    Token::Bool(false),
                    Token::EndStruct,
                    Token::EndStruct,
                ],
                "{\"akey\":{\"bkey\":false}}",
            ),
        ];

        for (tokens, want) in cases {
            let mut got = String::new();
            json_from_tokens(&mut got, TokenVec::from_iter(tokens.into_iter())).unwrap();
            assert_eq!(got, want);
        }
    }

    #[test]
    fn test_from_tokens_enum() {
        let cases = vec![
            (
                vec![
                    Token::Enum(EnumMeta { variants: None }),
                    Token::Variant(EnumVariant::Str("a")),
                    Token::EndEnum,
                ],
                "{\"a\":[]}",
            ),
            (
                vec![
                    Token::Enum(EnumMeta { variants: None }),
                    Token::Variant(EnumVariant::Usize(42)),
                    Token::EndEnum,
                ],
                "{\"42\":[]}",
            ),
            (
                vec![
                    Token::Enum(EnumMeta { variants: None }),
                    Token::Variant(EnumVariant::Str("a")),
                    Token::Bool(true),
                    Token::EndEnum,
                ],
                "{\"a\":[true]}",
            ),
            (
                vec![
                    Token::Enum(EnumMeta { variants: None }),
                    Token::Variant(EnumVariant::Str("a")),
                    Token::Bool(true),
                    Token::Bool(false),
                    Token::EndEnum,
                ],
                "{\"a\":[true,false]}",
            ),
            (
                vec![
                    Token::Enum(EnumMeta { variants: None }),
                    Token::Variant(EnumVariant::Str("a")),
                    Token::Enum(EnumMeta { variants: None }),
                    Token::Variant(EnumVariant::Str("b")),
                    Token::Bool(true),
                    Token::EndEnum,
                    Token::EndEnum,
                ],
                "{\"a\":[{\"b\":[true]}]}",
            ),
        ];

        for (tokens, want) in cases {
            let mut got = String::new();
            json_from_tokens(&mut got, TokenVec::from_iter(tokens.into_iter())).unwrap();
            assert_eq!(got, want);
        }
    }
}
