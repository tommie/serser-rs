use std::fmt;

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
        WriteError::Token(TokenError::invalid_token(token, expected))
    }

    fn invalid_field(field: &str) -> Self {
        WriteError::Token(TokenError::invalid_field(field))
    }

    fn missing_fields(fields: &[&str]) -> Self {
        WriteError::Token(TokenError::missing_fields(fields))
    }

    fn invalid_variant(variant: EnumVariant<'_>) -> Self {
        WriteError::Token(TokenError::invalid_variant(variant))
    }

    fn unexpected_end(expected: Option<TokenTypes>) -> Self {
        WriteError::Token(TokenError::unexpected_end(expected))
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
        writer,
        state: JsonState::Plain,
        states: Vec::new(),
    };
    into.into_tokens(&mut sink)?;

    match sink.state {
        JsonState::Done => Ok(()),
        _ => Err(WriteError::unexpected_end(sink.expect_tokens())),
    }
}

struct JsonTokenSink<W: fmt::Write> {
    writer: W,
    state: JsonState,
    states: Vec<JsonState>,
}

impl<W: fmt::Write> JsonTokenSink<W> {
    /// Writes the comma or colon as appropriate before a
    /// token. Updates the state.
    fn write_sep(&mut self, token: &Token<'_>) -> Result<(), WriteError> {
        match self.state {
            JsonState::Plain => self.state = JsonState::Done,
            JsonState::FirstArrayElement => self.state = JsonState::ArrayElement,
            JsonState::ArrayElement => self
                .writer
                .write_str(",")
                .map_err(|err| WriteError::Fmt(err))?,
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

                self.writer
                    .write_str(",")
                    .map_err(|err| WriteError::Fmt(err))?;
                self.state = JsonState::ObjectValue;
            }
            JsonState::ObjectValue => {
                self.writer
                    .write_str(":")
                    .map_err(|err| WriteError::Fmt(err))?;
                self.state = JsonState::ObjectKey;
            }
            JsonState::EnumVariant(kind) => self.state = JsonState::EnumStart(kind),
            JsonState::EnumStart(_) => {
                self.writer
                    .write_str(":")
                    .map_err(|err| WriteError::Fmt(err))?;
                self.state = JsonState::EnumEnd;
            }
            JsonState::EnumEnd => {}
            JsonState::Done => unreachable!(),
        }

        Ok(())
    }

    fn write_bytes(&mut self, v: &[u8]) -> Result<(), fmt::Error> {
        let w = &mut self.writer;

        w.write_char('"')?;
        w.write_str(BASE64_STANDARD.encode(v).as_str())?;
        w.write_char('"')?;

        Ok(())
    }

    fn write_str(&mut self, s: &str) -> Result<(), fmt::Error> {
        let mut start = 0;
        let mut end = 0;
        let w = &mut self.writer;

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

    fn yield_token(&mut self, token: Token<'_>) -> Result<bool, Self::Error> {
        if token.is_end() {
            match self.state {
                JsonState::FirstArrayElement | JsonState::ArrayElement => match token {
                    Token::EndSeq | Token::EndTuple => {}
                    _ => {
                        return Err(WriteError::invalid_token(
                            token.into(),
                            Some(TokenTypes::new(TokenType::EndTuple).with(TokenType::EndSeq)),
                        ))
                    }
                },
                JsonState::FirstObjectKey | JsonState::ObjectKey => match token {
                    Token::EndStruct => {}
                    _ => {
                        return Err(WriteError::invalid_token(
                            token.into(),
                            Some(TokenTypes::new(TokenType::EndStruct)),
                        ))
                    }
                },
                JsonState::EnumEnd => match token {
                    Token::EndEnum => {}
                    _ => {
                        return Err(WriteError::invalid_token(
                            token.into(),
                            Some(TokenTypes::new(TokenType::EndEnum)),
                        ))
                    }
                },

                JsonState::ObjectValue => {
                    return Err(WriteError::Token(TokenError::InvalidToken(
                        token.into(),
                        None,
                    )))
                }
                JsonState::EnumVariant(_) => {
                    return Err(WriteError::Token(TokenError::InvalidToken(
                        token.into(),
                        Some(TokenTypes::new(TokenType::Variant)),
                    )))
                }
                // Backfill empty enums.
                JsonState::EnumStart(EnumKind::Tuple) => self
                    .writer
                    .write_str(":[]")
                    .map_err(|err| WriteError::Fmt(err))?,
                JsonState::EnumStart(EnumKind::Struct) => self
                    .writer
                    .write_str(":{}")
                    .map_err(|err| WriteError::Fmt(err))?,
                _ => {}
            }

            self.state = JsonState::Done;
        } else {
            self.write_sep(&token)?;
        }

        let w = &mut self.writer;

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
            Token::Char(v) => self.write_str(v.to_string().as_str()),
            Token::Bytes(v) => self.write_bytes(v),
            Token::Str(s) => self.write_str(s),
            Token::Field(s) => self.write_str(s),
            Token::Variant(EnumVariant::Str(s)) => self.write_str(s),
            Token::Variant(EnumVariant::Usize(i)) => write!(w, "\"{}\"", i),

            Token::Seq(_) => {
                self.states.push(std::mem::replace(
                    &mut self.state,
                    JsonState::FirstArrayElement,
                ));
                w.write_str("[")
            }
            Token::Tuple(_) => {
                self.states.push(std::mem::replace(
                    &mut self.state,
                    JsonState::FirstArrayElement,
                ));
                w.write_str("[")
            }
            Token::Struct(_) => {
                self.states.push(std::mem::replace(
                    &mut self.state,
                    JsonState::FirstObjectKey,
                ));
                w.write_str("{")
            }
            Token::Enum(meta) => {
                self.states.push(std::mem::replace(
                    &mut self.state,
                    JsonState::EnumVariant(meta.kind.unwrap_or(EnumKind::Tuple)),
                ));
                w.write_str("{")
            }

            Token::EndSeq => {
                self.state = self
                    .states
                    .pop()
                    .ok_or(WriteError::invalid_token(token, Some(TokenTypes::EMPTY)))?;
                w.write_str("]")
            }
            Token::EndTuple => {
                self.state = self
                    .states
                    .pop()
                    .ok_or(WriteError::invalid_token(token, Some(TokenTypes::EMPTY)))?;
                w.write_str("]")
            }
            Token::EndStruct => {
                self.state = self
                    .states
                    .pop()
                    .ok_or(WriteError::invalid_token(token, Some(TokenTypes::EMPTY)))?;
                w.write_str("}")
            }
            Token::EndEnum => {
                self.state = self
                    .states
                    .pop()
                    .ok_or(WriteError::invalid_token(token, Some(TokenTypes::EMPTY)))?;
                w.write_str("}")
            }
        }
        .map_err(|err| WriteError::Fmt(err))?;

        Ok(true)
    }

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        match self.state {
            JsonState::Plain => None,
            JsonState::FirstArrayElement => None,
            JsonState::ArrayElement => None,
            JsonState::FirstObjectKey => Some(TokenTypes::new(TokenType::Field)),
            JsonState::ObjectKey => Some(TokenTypes::new(TokenType::Field)),
            JsonState::ObjectValue => None,
            JsonState::EnumVariant(_) => Some(TokenTypes::new(TokenType::Variant)),
            JsonState::EnumStart(EnumKind::Tuple) => Some(TokenTypes::new(TokenType::Tuple)),
            JsonState::EnumStart(EnumKind::Struct) => Some(TokenTypes::new(TokenType::Struct)),
            JsonState::EnumEnd => Some(TokenTypes::new(TokenType::EndEnum)),
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
    EnumVariant(EnumKind),
    EnumStart(EnumKind),
    EnumEnd,
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
    fn test_from_tokens_enum_tuple() {
        let cases = vec![
            (
                vec![
                    Token::Enum(EnumMeta {
                        variants: None,
                        kind: None,
                    }),
                    Token::Variant(EnumVariant::Str("a")),
                    Token::EndEnum,
                ],
                "{\"a\":[]}",
            ),
            (
                vec![
                    Token::Enum(EnumMeta {
                        variants: None,
                        kind: None,
                    }),
                    Token::Variant(EnumVariant::Usize(42)),
                    Token::EndEnum,
                ],
                "{\"42\":[]}",
            ),
            (
                vec![
                    Token::Enum(EnumMeta {
                        variants: None,
                        kind: None,
                    }),
                    Token::Variant(EnumVariant::Str("a")),
                    Token::Tuple(TupleMeta { size_hint: Some(1) }),
                    Token::Bool(true),
                    Token::EndTuple,
                    Token::EndEnum,
                ],
                "{\"a\":[true]}",
            ),
            (
                vec![
                    Token::Enum(EnumMeta {
                        variants: None,
                        kind: None,
                    }),
                    Token::Variant(EnumVariant::Str("a")),
                    Token::Tuple(TupleMeta { size_hint: Some(2) }),
                    Token::Bool(true),
                    Token::Bool(false),
                    Token::EndTuple,
                    Token::EndEnum,
                ],
                "{\"a\":[true,false]}",
            ),
            (
                vec![
                    Token::Enum(EnumMeta {
                        variants: None,
                        kind: None,
                    }),
                    Token::Variant(EnumVariant::Str("a")),
                    Token::Tuple(TupleMeta { size_hint: Some(1) }),
                    Token::Enum(EnumMeta {
                        variants: None,
                        kind: None,
                    }),
                    Token::Variant(EnumVariant::Str("b")),
                    Token::Tuple(TupleMeta { size_hint: Some(1) }),
                    Token::Bool(true),
                    Token::EndTuple,
                    Token::EndEnum,
                    Token::EndTuple,
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

    #[test]
    fn test_from_tokens_enum_struct() {
        let cases = vec![
            (
                vec![
                    Token::Enum(EnumMeta {
                        variants: None,
                        kind: None,
                    }),
                    Token::Variant(EnumVariant::Str("a")),
                    Token::Struct(StructMeta {
                        fields: Some(&["b"]),
                    }),
                    Token::Field("b"),
                    Token::Bool(true),
                    Token::EndStruct,
                    Token::EndEnum,
                ],
                r#"{"a":{"b":true}}"#,
            ),
            (
                vec![
                    Token::Enum(EnumMeta {
                        variants: None,
                        kind: None,
                    }),
                    Token::Variant(EnumVariant::Str("a")),
                    Token::Struct(StructMeta {
                        fields: Some(&["b", "c"]),
                    }),
                    Token::Field("b"),
                    Token::Bool(true),
                    Token::Field("c"),
                    Token::Bool(false),
                    Token::EndStruct,
                    Token::EndEnum,
                ],
                r#"{"a":{"b":true,"c":false}}"#,
            ),
            (
                vec![
                    Token::Enum(EnumMeta {
                        variants: None,
                        kind: None,
                    }),
                    Token::Variant(EnumVariant::Str("a")),
                    Token::Struct(StructMeta {
                        fields: Some(&["b"]),
                    }),
                    Token::Field("b"),
                    Token::Enum(EnumMeta {
                        variants: None,
                        kind: None,
                    }),
                    Token::Variant(EnumVariant::Str("c")),
                    Token::Struct(StructMeta {
                        fields: Some(&["d"]),
                    }),
                    Token::Field("d"),
                    Token::Bool(true),
                    Token::EndStruct,
                    Token::EndEnum,
                    Token::EndStruct,
                    Token::EndEnum,
                ],
                r#"{"a":{"b":{"c":{"d":true}}}}"#,
            ),
        ];

        for (tokens, want) in cases {
            let mut got = String::new();
            json_from_tokens(&mut got, TokenVec::from_iter(tokens.into_iter())).unwrap();
            assert_eq!(got, want);
        }
    }
}
