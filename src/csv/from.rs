use core::cmp::min;
use std::fmt;

use base64::prelude::*;

use crate::token::*;
use crate::Error;
use crate::IntoTokens;
use crate::TokenError;
use crate::TokenSink;
use super::options::*;

/// The error returned from [csv_from_tokens].
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

/// Writes CSV to `writer` from data in an [IntoTokens].
///
/// Enum types are not supported.
pub fn csv_from_tokens<'a, I: IntoTokens, W: fmt::Write>(
    options: &CsvOptions<'a>,
    writer: W,
    into: I,
) -> Result<(), WriteError> {
    let mut sink = CsvTokenSink {
        options,
        writer,
        level: 0,
        first: true,
    };
    into.into_tokens(&mut sink)?;

    match sink.level {
        0 => Ok(()),
        _ => Err(WriteError::unexpected_end(sink.expect_tokens())),
    }
}

struct CsvTokenSink<'a, W: fmt::Write> {
    options: &'a CsvOptions<'a>,
    writer: W,
    level: usize,
    first: bool,
}

impl<'a, W: fmt::Write> CsvTokenSink<'a, W> {
    fn write_sep(&mut self) -> Result<(), fmt::Error> {
        if !self.first {
            self.writer.write_char(
                self.options.separators[min(self.level, self.options.separators.len() - 1)],
            )
        } else {
            Ok(())
        }
    }

    fn write_bytes(&mut self, v: &[u8]) -> Result<(), fmt::Error> {
        self.writer.write_str(BASE64_STANDARD.encode(v).as_str())
    }

    fn write_str(&mut self, s: &str) -> Result<(), fmt::Error> {
        if !self.needs_quote(s) {
            return self.writer.write_str(s);
        }

        let quote = if let Some(c) = self.options.quote {
            c
        } else {
            return self.writer.write_str(s);
        };

        let mut start = 0;
        let mut end = 0;
        let w = &mut self.writer;

        w.write_char(quote)?;

        for c in s.chars() {
            if c == quote {
                w.write_str(&s[start..end])?;

                w.write_char(c)?;
                w.write_char(c)?;

                end += c.len_utf8();
                start = end;
            } else {
                end += c.len_utf8();
            }
        }

        if start < end {
            w.write_str(&s[start..end])?;
        }

        w.write_char(quote)?;

        Ok(())
    }

    fn needs_quote(&self, s: &str) -> bool {
        for c in s.chars() {
            if self.options.separators.contains(&c) || self.options.quote == Some(c) {
                return true;
            }
        }

        false
    }
}

impl<'a, W: fmt::Write> TokenSink for CsvTokenSink<'a, W> {
    type Error = WriteError;

    fn yield_token(&mut self, token: Token<'_>) -> Result<bool, Self::Error> {
        if token.is_end() {
            match self.level {
                0 => {
                    return Err(WriteError::invalid_token(
                        token.into(),
                        Some(TokenTypes::new(TokenType::EndTuple).with(TokenType::EndSeq)),
                    ))
                }
                _ => {
                    self.level -= 1;
                }
            }
        } else {
            match token {
                Token::Field(_) => {}
                _ => {
                    self.write_sep().map_err(|err| WriteError::Fmt(err))?;
                    self.first = false;
                }
            }
        }

        let w = &mut self.writer;

        match token {
            Token::Unit => Ok(()),
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

            Token::Seq(_) | Token::Tuple(_) | Token::Struct(_) => {
                self.level += 1;
                self.first = true;

                Ok(())
            }

            Token::Field(_) => Ok(()),
            Token::EndSeq | Token::EndTuple | Token::EndStruct => Ok(()),

            // Enums are not supported.
            Token::Enum(_) | Token::EndEnum | Token::Variant(_) => {
                return Err(WriteError::invalid_token(token.into(), None))
            }
        }
        .map_err(|err| WriteError::Fmt(err))?;

        Ok(true)
    }

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        match self.level {
            0 => Some(
                TokenTypes::new(TokenType::Seq)
                    .with(TokenType::Tuple)
                    .with(TokenType::Struct)
                    .with(TokenType::Enum),
            ),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::TokenVec;

    #[test]
    fn test_from_tokens_plain() {
        let cases = vec![
            (Token::Unit, ""),
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
            (Token::Char('a'), "a"),
            (Token::Bytes(b""), ""),
            (Token::Bytes(b"\x00"), "AA=="),
            (Token::Str("hello"), "hello"),
            (Token::Str("\n"), "\"\n\""),
            (Token::Str("❤️"), "❤️"),
            (Token::Str("\x1F"), "\x1F"),
            (Token::Str("hello"), "hello"),
        ];

        for (token, want) in cases {
            let mut got = String::new();
            csv_from_tokens(
                COMMA,
                &mut got,
                TokenVec::from_iter(vec![token].into_iter()),
            )
            .unwrap();
            assert_eq!(got, want);
        }
    }

    #[test]
    fn test_from_tokens_seq() {
        let cases = vec![
            (
                vec![Token::Seq(SeqMeta { size_hint: None }), Token::EndSeq],
                "",
            ),
            (
                vec![
                    Token::Seq(SeqMeta { size_hint: None }),
                    Token::Bool(true),
                    Token::EndSeq,
                ],
                "true",
            ),
            (
                vec![
                    Token::Seq(SeqMeta { size_hint: None }),
                    Token::Bool(true),
                    Token::EndSeq,
                    Token::Seq(SeqMeta { size_hint: None }),
                    Token::Bool(false),
                    Token::EndSeq,
                ],
                "true\nfalse",
            ),
            (
                vec![
                    Token::Seq(SeqMeta { size_hint: None }),
                    Token::Bool(true),
                    Token::Bool(false),
                    Token::EndSeq,
                ],
                "true,false",
            ),
            (
                vec![
                    Token::Seq(SeqMeta { size_hint: None }),
                    Token::Seq(SeqMeta { size_hint: None }),
                    Token::Bool(true),
                    Token::EndSeq,
                    Token::EndSeq,
                ],
                "true",
            ),
        ];

        for (tokens, want) in cases {
            let mut got = String::new();
            csv_from_tokens(COMMA, &mut got, TokenVec::from_iter(tokens.into_iter())).unwrap();
            assert_eq!(got, want);
        }
    }

    #[test]
    fn test_from_tokens_tuple() {
        let cases = vec![
            (
                vec![Token::Tuple(TupleMeta { size_hint: None }), Token::EndTuple],
                "",
            ),
            (
                vec![
                    Token::Tuple(TupleMeta { size_hint: None }),
                    Token::Bool(true),
                    Token::EndTuple,
                ],
                "true",
            ),
            (
                vec![
                    Token::Tuple(TupleMeta { size_hint: None }),
                    Token::Bool(true),
                    Token::EndTuple,
                    Token::Tuple(TupleMeta { size_hint: None }),
                    Token::Bool(false),
                    Token::EndTuple,
                ],
                "true\nfalse",
            ),
            (
                vec![
                    Token::Tuple(TupleMeta { size_hint: None }),
                    Token::Bool(true),
                    Token::Bool(false),
                    Token::EndTuple,
                ],
                "true,false",
            ),
            (
                vec![
                    Token::Tuple(TupleMeta { size_hint: None }),
                    Token::Tuple(TupleMeta { size_hint: None }),
                    Token::Bool(true),
                    Token::EndTuple,
                    Token::EndTuple,
                ],
                "true",
            ),
        ];

        for (tokens, want) in cases {
            let mut got = String::new();
            csv_from_tokens(COMMA, &mut got, TokenVec::from_iter(tokens.into_iter())).unwrap();
            assert_eq!(got, want);
        }
    }

    #[test]
    fn test_from_tokens_struct() {
        let cases = vec![
            (
                vec![Token::Struct(StructMeta { fields: None }), Token::EndStruct],
                "",
            ),
            (
                vec![
                    Token::Struct(StructMeta { fields: None }),
                    Token::Field("akey"),
                    Token::Bool(true),
                    Token::EndStruct,
                ],
                "true",
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
                "true,false",
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
                "false",
            ),
        ];

        for (tokens, want) in cases {
            let mut got = String::new();
            csv_from_tokens(COMMA, &mut got, TokenVec::from_iter(tokens.into_iter())).unwrap();
            assert_eq!(got, want);
        }
    }
}
