use std::io;
use std::str::FromStr;

use base64::prelude::*;

use crate::token::*;
use crate::utf8read::CharRead;
use crate::utf8read::UTF8Read;
use crate::FromTokenSink;
use crate::FromTokens;
use crate::TokenSink;

/// The error returned from [json_into_tokens].
#[derive(Debug)]
#[non_exhaustive]
pub enum ParseError<E: crate::error::Error> {
    /// An error returned by the text reader.
    Read(io::Error),

    /// An error returned by the [TokenSink].
    Sink(E),

    /// The JSON data ended prematurely.
    UnexpectedEnd,

    /// The JSON data contained an unexpected character.
    UnexpectedChar(char),

    /// Something that looked like a number couldn't be parsed as one.
    ParseFloat(std::num::ParseFloatError),

    /// A bytes string could not be decoded.
    Base64(base64::DecodeError),
}

/// Parses a JSON string into a token sink.
pub fn json_into<F: FromTokens, R: io::Read>(
    r: R,
) -> Result<F, ParseError<<<F as FromTokenSink>::Sink as TokenSink>::Error>> {
    let mut sink = F::new_sink();
    json_into_tokens(&mut sink, r)?;

    F::from_sink(sink).ok_or(ParseError::UnexpectedEnd)
}

/// Parses a JSON string into a token sink.
pub fn json_into_tokens<S: TokenSink, R: io::Read>(
    sink: &mut S,
    r: R,
) -> Result<(), ParseError<S::Error>> {
    parse_any(sink, &mut UTF8Read::new(r))
}

fn parse_any<'b, S: TokenSink, R: CharRead>(
    sink: &mut S,
    r: &mut R,
) -> Result<(), ParseError<S::Error>> {
    loop {
        match read_char_nws(r)? {
            Some('{') if expects_enum(sink) => {
                sink.yield_token(Token::Enum(EnumMeta {
                    variants: None,
                    kind: None,
                }))
                .map_err(|err| ParseError::Sink(err))?;

                parse_in_enum_object(sink, r)?;
                sink.yield_token(Token::EndEnum)
                    .map_err(|err| ParseError::Sink(err))?;

                return Ok(());
            }
            Some('{') => {
                sink.yield_token(Token::Struct(StructMeta { fields: None }))
                    .map_err(|err| ParseError::Sink(err))?;

                parse_in_object(sink, r)?;
                sink.yield_token(Token::EndStruct)
                    .map_err(|err| ParseError::Sink(err))?;

                return Ok(());
            }
            Some('[') => {
                let (start, end) = tokens_for_array(sink.expect_tokens());
                sink.yield_token(start)
                    .map_err(|err| ParseError::Sink(err))?;

                parse_in_array(sink, r)?;
                sink.yield_token(end).map_err(|err| ParseError::Sink(err))?;

                return Ok(());
            }
            Some('"') => return parse_in_string(sink, r, TokenType::Str),
            Some(c) if c == '-' || c.is_digit(10) => return parse_number(sink, r, c),
            Some('f') => return parse_in_false(sink, r),
            Some('t') => return parse_in_true(sink, r),
            Some('n') => return parse_in_null(sink, r),
            Some(c) => return Err(ParseError::UnexpectedChar(c)),
            None => return Ok(()),
        }
    }
}

fn tokens_for_array<'b>(expected: Option<TokenTypes>) -> (Token<'b>, Token<'b>) {
    if let Some(tts) = expected {
        if !tts.contains(TokenType::Seq) {
            for tt in tts.iter() {
                match tt {
                    TokenType::Tuple => {
                        return (Token::Tuple(TupleMeta { size_hint: None }), Token::EndTuple)
                    }
                    _ => {}
                }
            }
        }
    }

    (Token::Seq(SeqMeta { size_hint: None }), Token::EndSeq)
}

fn expects_enum<S: TokenSink>(sink: &mut S) -> bool {
    if let Some(tts) = sink.expect_tokens() {
        tts.contains(TokenType::Enum)
    } else {
        false
    }
}

fn parse_in_enum_object<'b, S: TokenSink, R: CharRead>(
    sink: &mut S,
    r: &mut R,
) -> Result<(), ParseError<S::Error>> {
    loop {
        match read_char_nws(r)? {
            Some('"') => {
                break;
            }
            Some(c) => return Err(ParseError::UnexpectedChar(c)),
            None => return Err(ParseError::UnexpectedEnd),
        }
    }

    parse_in_string(sink, r, TokenType::Variant)?;

    loop {
        match read_char_nws(r)? {
            Some(':') => {
                break;
            }
            Some(c) => return Err(ParseError::UnexpectedChar(c)),
            None => return Err(ParseError::UnexpectedEnd),
        }
    }

    loop {
        match read_char_nws(r)? {
            Some('[') => {
                sink.yield_token(Token::Tuple(TupleMeta { size_hint: None }))
                    .map_err(|err| ParseError::Sink(err))?;

                parse_in_array(sink, r)?;

                sink.yield_token(Token::EndTuple)
                    .map_err(|err| ParseError::Sink(err))?;

                break;
            }
            Some('{') => {
                sink.yield_token(Token::Struct(StructMeta { fields: None }))
                    .map_err(|err| ParseError::Sink(err))?;

                parse_in_object(sink, r)?;

                sink.yield_token(Token::EndStruct)
                    .map_err(|err| ParseError::Sink(err))?;

                break;
            }
            Some(c) => return Err(ParseError::UnexpectedChar(c)),
            None => return Err(ParseError::UnexpectedEnd),
        }
    }

    loop {
        match read_char_nws(r)? {
            Some('}') => {
                return Ok(());
            }
            Some(c) => return Err(ParseError::UnexpectedChar(c)),
            None => return Err(ParseError::UnexpectedEnd),
        }
    }
}

fn parse_in_object<'b, S: TokenSink, R: CharRead>(
    sink: &mut S,
    r: &mut R,
) -> Result<(), ParseError<S::Error>> {
    loop {
        loop {
            match read_char_nws(r)? {
                Some('}') => {
                    return Ok(());
                }
                Some('"') => {
                    break;
                }
                Some(c) => return Err(ParseError::UnexpectedChar(c)),
                None => return Err(ParseError::UnexpectedEnd),
            }
        }

        parse_in_string(sink, r, TokenType::Field)?;

        loop {
            match read_char_nws(r)? {
                Some(':') => {
                    break;
                }
                Some(c) => return Err(ParseError::UnexpectedChar(c)),
                None => return Err(ParseError::UnexpectedEnd),
            }
        }

        parse_any(sink, r)?;

        loop {
            match read_char_nws(r)? {
                Some(',') => {
                    break;
                }
                Some('}') => {
                    return Ok(());
                }
                Some(c) => return Err(ParseError::UnexpectedChar(c)),
                None => return Err(ParseError::UnexpectedEnd),
            }
        }
    }
}

fn parse_in_array<'b, S: TokenSink, R: CharRead>(
    sink: &mut S,
    r: &mut R,
) -> Result<(), ParseError<S::Error>> {
    loop {
        loop {
            match read_char_nws(r)? {
                Some(']') => {
                    return Ok(());
                }
                Some(c) => {
                    push_char(r, c)?;
                    break;
                }
                None => return Err(ParseError::UnexpectedEnd),
            }
        }

        parse_any(sink, r)?;

        loop {
            match read_char_nws(r)? {
                Some(',') => {
                    break;
                }
                Some(']') => {
                    return Ok(());
                }
                Some(c) => return Err(ParseError::UnexpectedChar(c)),
                None => return Err(ParseError::UnexpectedEnd),
            }
        }
    }
}

fn parse_in_string<'b, S: TokenSink, R: CharRead>(
    sink: &mut S,
    r: &mut R,
    default_type: TokenType,
) -> Result<(), ParseError<S::Error>> {
    let mut esc_n = 0; // -1 means single character.
    let mut esc_char = 0;
    let mut out = String::new();

    loop {
        match read_char(r)? {
            Some(c) if esc_n == -1 => {
                esc_n = 0;
                match c {
                    '"' => out.push(c),
                    '\\' => out.push(c),
                    '/' => out.push(c),
                    'b' => out.push('\x07'),
                    'f' => out.push('\x0C'),
                    'n' => out.push('\n'),
                    'r' => out.push('\r'),
                    't' => out.push('\t'),
                    'u' => esc_n = 4,
                    _ => return Err(ParseError::UnexpectedChar(c)),
                }
            }
            Some(c) if esc_n > 0 => {
                esc_n -= 1;
                esc_char <<= 4;
                esc_char |= c.to_digit(0x10).ok_or(ParseError::UnexpectedChar(c))?;

                if esc_n == 0 {
                    let ch = char::from_u32(esc_char).ok_or(ParseError::UnexpectedChar(c))?;
                    out.push(ch);
                    esc_char = 0;
                }
            }
            Some('"') => {
                match str_token_type(out.as_str(), sink.expect_tokens(), default_type) {
                    TokenType::Char => sink.yield_token(Token::Char(out.chars().next().unwrap())),
                    TokenType::Str => sink.yield_token(Token::Str(out.as_str())),
                    TokenType::Field => sink.yield_token(Token::Field(out.as_str())),
                    TokenType::Variant => {
                        sink.yield_token(Token::Variant(EnumVariant::Str(out.as_str())))
                    }
                    TokenType::Bytes => sink.yield_token(Token::Bytes(
                        BASE64_STANDARD
                            .decode(out.as_bytes())
                            .map_err(|err| ParseError::Base64(err))?
                            .as_slice(),
                    )),
                    _ => unreachable!(),
                }
                .map_err(|err| ParseError::Sink(err))?;

                return Ok(());
            }
            Some('\\') => {
                esc_n = -1;
            }
            Some(c) => {
                out.push(c);
            }
            None => return Err(ParseError::UnexpectedEnd),
        }
    }
}

fn str_token_type(s: &str, expected: Option<TokenTypes>, default_type: TokenType) -> TokenType {
    if let Some(tts) = expected {
        if tts.contains(default_type) {
            default_type
        } else if tts.contains(TokenType::Char) && s.len() == 1 {
            TokenType::Char
        } else if tts.contains(TokenType::Str) {
            TokenType::Str
        } else if tts.contains(TokenType::Field) {
            TokenType::Field
        } else if tts.contains(TokenType::Variant) {
            TokenType::Variant
        } else if tts.contains(TokenType::Bytes) {
            TokenType::Bytes
        } else {
            default_type
        }
    } else {
        default_type
    }
}

fn parse_number<'b, S: TokenSink, R: CharRead>(
    sink: &mut S,
    r: &mut R,
    first_char: char,
) -> Result<(), ParseError<S::Error>> {
    let mut buf = String::with_capacity(16);
    buf.push(first_char);

    loop {
        let c = read_char(r)?;
        match c {
            Some(c) if c.is_digit(10) => {}
            Some('.' | 'e' | 'E' | '+' | '-') => {}
            Some(c) => {
                push_char(r, c)?;
                break;
            }
            None => break,
        }

        buf.push(c.unwrap());
    }

    let token = sink.expect_tokens();

    sink.yield_token(as_number_token(buf.as_str(), token)?)
        .map_err(|err| ParseError::Sink(err))?;

    Ok(())
}

fn as_number_token<'b, E: crate::error::Error>(
    s: &'b str,
    expected: Option<TokenTypes>,
) -> Result<Token<'b>, ParseError<E>> {
    let v = f64::from_str(s).map_err(|err| ParseError::ParseFloat(err))?;

    if let Some(tts) = expected {
        if !tts.contains(TokenType::F64) {
            for tt in tts.iter() {
                match tt {
                    TokenType::U8 => return Ok(Token::U8(v as u8)),
                    TokenType::U16 => return Ok(Token::U16(v as u16)),
                    TokenType::U32 => return Ok(Token::U32(v as u32)),
                    TokenType::U64 => return Ok(Token::U64(v as u64)),
                    TokenType::U128 => return Ok(Token::U128(v as u128)),
                    TokenType::Usize => return Ok(Token::Usize(v as usize)),
                    TokenType::I8 => return Ok(Token::I8(v as i8)),
                    TokenType::I16 => return Ok(Token::I16(v as i16)),
                    TokenType::I32 => return Ok(Token::I32(v as i32)),
                    TokenType::I64 => return Ok(Token::I64(v as i64)),
                    TokenType::I128 => return Ok(Token::I128(v as i128)),
                    TokenType::Isize => return Ok(Token::Isize(v as isize)),
                    TokenType::F32 => return Ok(Token::F32(v as f32)),
                    TokenType::Str => return Ok(Token::Str(s)),
                    _ => {}
                }
            }
        }
    }

    Ok(Token::F64(v))
}

fn parse_in_false<'b, S: TokenSink, R: CharRead>(
    sink: &mut S,
    r: &mut R,
) -> Result<(), ParseError<S::Error>> {
    read_match(r, "alse")?;

    sink.yield_token(Token::Bool(false))
        .map_err(|err| ParseError::Sink(err))?;

    Ok(())
}

fn parse_in_true<'b, S: TokenSink, R: CharRead>(
    sink: &mut S,
    r: &mut R,
) -> Result<(), ParseError<S::Error>> {
    read_match(r, "rue")?;

    sink.yield_token(Token::Bool(true))
        .map_err(|err| ParseError::Sink(err))?;

    Ok(())
}

fn parse_in_null<'b, S: TokenSink, R: CharRead>(
    sink: &mut S,
    r: &mut R,
) -> Result<(), ParseError<S::Error>> {
    read_match(r, "ull")?;

    let tt = if let Some(tts) = sink.expect_tokens() {
        if tts.contains(TokenType::Seq) {
            TokenType::Seq
        } else if tts.contains(TokenType::Tuple) {
            TokenType::Tuple
        } else if tts.contains(TokenType::Struct) {
            TokenType::Struct
        } else {
            TokenType::Unit
        }
    } else {
        TokenType::Unit
    };

    match tt {
        TokenType::Unit => {
            sink.yield_token(Token::Unit)
                .map_err(|err| ParseError::Sink(err))?;
        }
        TokenType::Seq => {
            sink.yield_token(Token::Seq(SeqMeta { size_hint: None }))
                .map_err(|err| ParseError::Sink(err))?;
            sink.yield_token(Token::EndSeq)
                .map_err(|err| ParseError::Sink(err))?;
        }
        TokenType::Struct => {
            sink.yield_token(Token::Struct(StructMeta { fields: None }))
                .map_err(|err| ParseError::Sink(err))?;
            sink.yield_token(Token::EndStruct)
                .map_err(|err| ParseError::Sink(err))?;
        }
        TokenType::Tuple => {
            sink.yield_token(Token::Tuple(TupleMeta { size_hint: None }))
                .map_err(|err| ParseError::Sink(err))?;
            sink.yield_token(Token::EndTuple)
                .map_err(|err| ParseError::Sink(err))?;
        }
        _ => unreachable!(),
    }

    Ok(())
}

fn read_char_nws<R: CharRead, E: crate::error::Error>(
    r: &mut R,
) -> Result<Option<char>, ParseError<E>> {
    loop {
        match read_char(r)? {
            Some(' ') => {}
            Some('\t') => {}
            Some('\n') => {}
            Some('\r') => {}
            Some(c) => return Ok(Some(c)),
            None => return Ok(None),
        }
    }
}

fn read_char<R: CharRead, E: crate::error::Error>(
    r: &mut R,
) -> Result<Option<char>, ParseError<E>> {
    r.read_char().map_err(|err| ParseError::Read(err))
}

fn read_match<R: CharRead, E: crate::error::Error>(
    r: &mut R,
    s: &str,
) -> Result<(), ParseError<E>> {
    r.read_match(s).map_err(|err| ParseError::Read(err))
}

fn push_char<R: CharRead, E: crate::error::Error>(r: &mut R, c: char) -> Result<(), ParseError<E>> {
    r.push_char(c).map_err(|err| ParseError::Read(err))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test::ExpectingTokenSink;
    use crate::*;

    #[test]
    fn test_json_into_tokens_empty() {
        let cases = vec!["", " "];

        for json in cases {
            let mut got = TokenVec::new();
            json_into_tokens(&mut got, json.as_bytes()).unwrap();
            assert_eq!(got.into_vec(), vec![]);
        }
    }

    #[test]
    fn test_json_into_tokens_null() {
        let cases = vec!["null", " null"];

        for json in cases {
            let mut got = TokenVec::new();
            json_into_tokens(&mut got, json.as_bytes()).unwrap();
            assert_eq!(got.into_vec(), vec![OwningToken::Unit]);
        }
    }

    #[test]
    fn test_json_into_tokens_bool() {
        let cases = vec![("false", false), (" false", false), ("true", true)];

        for (json, want) in cases {
            let mut got = TokenVec::new();
            json_into_tokens(&mut got, json.as_bytes()).unwrap();
            assert_eq!(got.into_vec(), vec![OwningToken::Bool(want)]);
        }
    }

    #[test]
    fn test_json_into_tokens_f64() {
        let cases = vec![
            ("0", 0.0),
            ("-0", 0.0),
            ("-1", -1.0),
            ("0.25", 0.25),
            ("1000", 1000.0),
            ("1e3", 1000.0),
            ("1E3", 1000.0),
            ("1e+3", 1000.0),
            ("5e-1", 0.5),
        ];

        for (json, want) in cases {
            let mut got = TokenVec::new();
            json_into_tokens(&mut got, json.as_bytes()).unwrap();
            assert_eq!(got.into_vec(), vec![OwningToken::F64(want)]);
        }
    }

    #[test]
    fn test_json_into_tokens_char() {
        let cases = vec![(r#""a""#, 'a')];

        for (json, want) in cases {
            let mut got = TokenVec::new();
            let mut expsink =
                ExpectingTokenSink::new(&mut got, |_| Some(TokenTypes::new(TokenType::Char)));
            json_into_tokens(&mut expsink, json.as_bytes()).unwrap();
            assert_eq!(got.into_vec(), vec![OwningToken::Char(want)]);
        }
    }

    #[test]
    fn test_json_into_tokens_str() {
        let cases = vec![
            (r#""""#, ""),
            (r#""a""#, "a"),
            (r#""abc""#, "abc"),
            (r#""❤️""#, "❤️"),
            (r#""\\""#, "\\"),
            (r#""\/""#, "/"),
            (r#""\t""#, "\t"),
            (r#""\u000A""#, "\n"),
            (r#""\u000a""#, "\n"),
        ];

        for (json, want) in cases {
            let mut got = TokenVec::new();
            json_into_tokens(&mut got, json.as_bytes()).unwrap();
            assert_eq!(got.into_vec(), vec![OwningToken::Str(want.to_owned())]);
        }
    }

    #[test]
    fn test_json_into_tokens_bytes() {
        let cases = vec![(r#""""#, b"".as_slice()), (r#""AA==""#, b"\x00".as_slice())];

        for (json, want) in cases {
            let mut got = TokenVec::new();
            let mut expsink =
                ExpectingTokenSink::new(&mut got, |_| Some(TokenTypes::new(TokenType::Bytes)));
            json_into_tokens(&mut expsink, json.as_bytes()).unwrap();
            assert_eq!(got.into_vec(), vec![OwningToken::Bytes(want.to_owned())]);
        }
    }

    #[test]
    fn test_json_into_tokens_array() {
        let start = || OwningToken::Seq(SeqMeta { size_hint: None });
        let end = || OwningToken::EndSeq;
        let cases = vec![
            ("[]", vec![start(), end()]),
            (" [ ]", vec![start(), end()]),
            ("[false]", vec![start(), OwningToken::Bool(false), end()]),
            ("[ false ]", vec![start(), OwningToken::Bool(false), end()]),
            (
                "[false,true]",
                vec![
                    start(),
                    OwningToken::Bool(false),
                    OwningToken::Bool(true),
                    end(),
                ],
            ),
            (
                "[false , true]",
                vec![
                    start(),
                    OwningToken::Bool(false),
                    OwningToken::Bool(true),
                    end(),
                ],
            ),
            ("[[]]", vec![start(), start(), end(), end()]),
            (
                "[[false],true]",
                vec![
                    start(),
                    start(),
                    OwningToken::Bool(false),
                    end(),
                    OwningToken::Bool(true),
                    end(),
                ],
            ),
            (
                "[[],[]]",
                vec![start(), start(), end(), start(), end(), end()],
            ),
            (
                "[false,null,42]",
                vec![
                    start(),
                    OwningToken::Bool(false),
                    OwningToken::Unit,
                    OwningToken::F64(42.0),
                    end(),
                ],
            ),
        ];

        for (json, want) in cases {
            let mut got = TokenVec::new();
            json_into_tokens(&mut got, json.as_bytes()).unwrap();
            assert_eq!(got.into_vec(), want);
        }
    }

    #[test]
    fn test_json_into_tokens_object() {
        let start = || OwningToken::Struct(OwningStructMeta { fields: None });
        let end = || OwningToken::EndStruct;
        let cases = vec![
            ("{}", vec![start(), end()]),
            (" { }", vec![start(), end()]),
            (
                r#"{"a":true}"#,
                vec![
                    start(),
                    OwningToken::Field("a".to_owned()),
                    OwningToken::Bool(true),
                    end(),
                ],
            ),
            (
                r#"{ "a" : true }"#,
                vec![
                    start(),
                    OwningToken::Field("a".to_owned()),
                    OwningToken::Bool(true),
                    end(),
                ],
            ),
            (
                r#"{"a":true,"b":false}"#,
                vec![
                    start(),
                    OwningToken::Field("a".to_owned()),
                    OwningToken::Bool(true),
                    OwningToken::Field("b".to_owned()),
                    OwningToken::Bool(false),
                    end(),
                ],
            ),
        ];

        for (json, want) in cases {
            let mut got = TokenVec::new();
            json_into_tokens(&mut got, json.as_bytes()).unwrap();
            assert_eq!(got.into_vec(), want);
        }
    }

    #[test]
    fn test_json_into_tokens_enum_tuple() {
        let start = || {
            OwningToken::Enum(OwningEnumMeta {
                variants: None,
                kind: None,
            })
        };
        let end = || OwningToken::EndEnum;
        let cases = vec![
            (
                r#"{"a":[]}"#,
                vec![
                    start(),
                    OwningToken::Variant(OwningEnumVariant::Str("a".to_owned())),
                    OwningToken::Tuple(TupleMeta { size_hint: None }),
                    OwningToken::EndTuple,
                    end(),
                ],
            ),
            (
                r#"{ "a" : [ ] }"#,
                vec![
                    start(),
                    OwningToken::Variant(OwningEnumVariant::Str("a".to_owned())),
                    OwningToken::Tuple(TupleMeta { size_hint: None }),
                    OwningToken::EndTuple,
                    end(),
                ],
            ),
            (
                r#"{"a":[true]}"#,
                vec![
                    start(),
                    OwningToken::Variant(OwningEnumVariant::Str("a".to_owned())),
                    OwningToken::Tuple(TupleMeta { size_hint: None }),
                    OwningToken::Bool(true),
                    OwningToken::EndTuple,
                    end(),
                ],
            ),
            (
                r#"{"a":[true,false]}"#,
                vec![
                    start(),
                    OwningToken::Variant(OwningEnumVariant::Str("a".to_owned())),
                    OwningToken::Tuple(TupleMeta { size_hint: None }),
                    OwningToken::Bool(true),
                    OwningToken::Bool(false),
                    OwningToken::EndTuple,
                    end(),
                ],
            ),
        ];

        for (json, want) in cases {
            let mut got = TokenVec::new();
            let mut expsink =
                ExpectingTokenSink::new(&mut got, |_| Some(TokenTypes::new(TokenType::Enum)));
            json_into_tokens(&mut expsink, json.as_bytes()).unwrap();
            assert_eq!(got.into_vec(), want);
        }
    }

    #[test]
    fn test_json_into_tokens_enum_struct() {
        let start = || {
            OwningToken::Enum(OwningEnumMeta {
                variants: None,
                kind: None,
            })
        };
        let end = || OwningToken::EndEnum;
        let cases = vec![
            (
                r#"{"a":{}}"#,
                vec![
                    start(),
                    OwningToken::Variant(OwningEnumVariant::Str("a".to_owned())),
                    OwningToken::Struct(OwningStructMeta { fields: None }),
                    OwningToken::EndStruct,
                    end(),
                ],
            ),
            (
                r#"{ "a" : { } }"#,
                vec![
                    start(),
                    OwningToken::Variant(OwningEnumVariant::Str("a".to_owned())),
                    OwningToken::Struct(OwningStructMeta { fields: None }),
                    OwningToken::EndStruct,
                    end(),
                ],
            ),
            (
                r#"{"a":{"b":true}}"#,
                vec![
                    start(),
                    OwningToken::Variant(OwningEnumVariant::Str("a".to_owned())),
                    OwningToken::Struct(OwningStructMeta { fields: None }),
                    OwningToken::Field("b".to_owned()),
                    OwningToken::Bool(true),
                    OwningToken::EndStruct,
                    end(),
                ],
            ),
            (
                r#"{"a":{"b":true,"c":false}}"#,
                vec![
                    start(),
                    OwningToken::Variant(OwningEnumVariant::Str("a".to_owned())),
                    OwningToken::Struct(OwningStructMeta { fields: None }),
                    OwningToken::Field("b".to_owned()),
                    OwningToken::Bool(true),
                    OwningToken::Field("c".to_owned()),
                    OwningToken::Bool(false),
                    OwningToken::EndStruct,
                    end(),
                ],
            ),
        ];

        for (json, want) in cases {
            let mut got = TokenVec::new();
            let mut expsink =
                ExpectingTokenSink::new(&mut got, |_| Some(TokenTypes::new(TokenType::Enum)));
            json_into_tokens(&mut expsink, json.as_bytes()).unwrap();
            assert_eq!(got.into_vec(), want);
        }
    }
}
