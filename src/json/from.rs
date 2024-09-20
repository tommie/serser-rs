use std::str::FromStr;

use crate::token::*;
use crate::TokenSink;

/// The error returned from [json_into_tokens].
#[derive(Debug, Eq, PartialEq)]
pub enum ParseError<E: crate::error::Error> {
    /// An error returned by the [TokenSink].
    Sink(E),

    /// The JSON data ended prematurely.
    UnexpectedEnd,

    /// The JSON data contained an unexpected character.
    UnexpectedChar(char),

    /// Something that looked like a number couldn't be parsed as one.
    ParseFloat(std::num::ParseFloatError),
}

/// Parses a JSON string into a token sink.
pub fn json_into_tokens<S: TokenSink>(sink: &mut S, json: &str) -> Result<(), ParseError<S::Error>> {
    parse_any(sink, json)?;

    Ok(())
}

fn parse_any<'b, S: TokenSink>(
    sink: &mut S,
    mut json: &'b str,
) -> Result<&'b str, ParseError<S::Error>> {
    loop {
        match json.chars().next() {
            Some(' ') => {}
            Some('\t') => {}
            Some('\n') => {}
            Some('\r') => {}
            Some('{') => {
                let mut subsink = sink
                    .yield_start(Token::Struct(StructMeta { fields: None }))
                    .map_err(|err| ParseError::Sink(err))?;

                json = parse_in_object(&mut subsink, &json[1..])?;
                subsink
                    .yield_token(Token::EndStruct)
                    .map_err(|err| ParseError::Sink(err))?;
                sink.end(subsink);

                return Ok(json);
            }
            Some('[') => {
                let (start, end) = tokens_for_array(sink.expect_tokens());
                let mut subsink = sink.yield_start(start).map_err(|err| ParseError::Sink(err))?;

                json = parse_in_array(&mut subsink, &json[1..])?;
                subsink.yield_token(end).map_err(|err| ParseError::Sink(err))?;
                sink.end(subsink);

                return Ok(json);
            }
            Some('"') => return parse_in_string(sink, &json[1..], |s| Token::Str(s)),
            Some(c) if c == '-' || c.is_digit(10) => return parse_number(sink, json),
            Some('f') => return parse_in_false(sink, &json[1..]),
            Some('t') => return parse_in_true(sink, &json[1..]),
            Some('n') => return parse_in_null(sink, &json[1..]),
            Some(c) => return Err(ParseError::UnexpectedChar(c)),
            None => return Ok(json),
        }

        json = &json[1..];
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

fn parse_in_object<'b, S: TokenSink>(
    sink: &mut S,
    mut json: &'b str,
) -> Result<&'b str, ParseError<S::Error>> {
    loop {
        loop {
            match json.chars().next() {
                None => return Err(ParseError::UnexpectedEnd),
                Some(' ') => {}
                Some('\t') => {}
                Some('\n') => {}
                Some('\r') => {}
                Some('}') => {
                    json = &json[1..];
                    return Ok(json);
                }
                Some('"') => {
                    json = &json[1..];
                    break;
                }
                Some(c) => return Err(ParseError::UnexpectedChar(c)),
            }

            json = &json[1..];
        }

        json = parse_in_string(sink, json, |s| Token::Field(s))?;

        loop {
            match json.chars().next() {
                None => return Err(ParseError::UnexpectedEnd),
                Some(' ') => {}
                Some('\t') => {}
                Some('\n') => {}
                Some('\r') => {}
                Some(':') => {
                    json = &json[1..];
                    break;
                }
                Some(c) => return Err(ParseError::UnexpectedChar(c)),
            }

            json = &json[1..];
        }

        json = parse_any(sink, json)?;

        loop {
            match json.chars().next() {
                None => return Err(ParseError::UnexpectedEnd),
                Some(' ') => {}
                Some('\t') => {}
                Some('\n') => {}
                Some('\r') => {}
                Some(',') => {
                    json = &json[1..];
                    break;
                }
                Some('}') => {
                    json = &json[1..];
                    return Ok(json);
                }
                Some(c) => return Err(ParseError::UnexpectedChar(c)),
            }

            json = &json[1..];
        }
    }
}

fn parse_in_array<'b, S: TokenSink>(
    sink: &mut S,
    mut json: &'b str,
) -> Result<&'b str, ParseError<S::Error>> {
    loop {
        loop {
            match json.chars().next() {
                None => return Err(ParseError::UnexpectedEnd),
                Some(' ') => {}
                Some('\t') => {}
                Some('\n') => {}
                Some('\r') => {}
                Some(']') => {
                    json = &json[1..];
                    return Ok(json);
                }
                Some(_) => break,
            }

            json = &json[1..];
        }

        json = parse_any(sink, json)?;

        loop {
            match json.chars().next() {
                None => return Err(ParseError::UnexpectedEnd),
                Some(' ') => {}
                Some('\t') => {}
                Some('\n') => {}
                Some('\r') => {}
                Some(',') => {
                    json = &json[1..];
                    break;
                }
                Some(']') => {
                    json = &json[1..];
                    return Ok(json);
                }
                Some(c) => return Err(ParseError::UnexpectedChar(c)),
            }

            json = &json[1..];
        }
    }
}

fn parse_in_string<'b, S: TokenSink>(
    sink: &mut S,
    json: &'b str,
    to_token: impl for<'c> FnOnce(&'c str) -> Token<'c>,
) -> Result<&'b str, ParseError<S::Error>> {
    let mut n = 0;
    let mut esc_n = 0; // -1 means single character.
    let mut esc_char = 0;
    let mut out = String::new();

    for c in json.chars() {
        match c {
            c if esc_n == -1 => {
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
            c if esc_n > 0 => {
                esc_n -= 1;
                esc_char <<= 4;
                esc_char |= c.to_digit(0x10).ok_or(ParseError::UnexpectedChar(c))?;

                if esc_n == 0 {
                    let ch = char::from_u32(esc_char).ok_or(ParseError::UnexpectedChar(c))?;
                    out.push(ch);
                    esc_char = 0;
                }
            }
            '"' => {
                n += c.len_utf8();
                let token = as_str_token(out.as_str(), sink.expect_tokens(), to_token);
                sink.yield_token(token).map_err(|err| ParseError::Sink(err))?;

                return Ok(&json[n..]);
            }
            '\\' => {
                esc_n = -1;
            }
            c => {
                out.push(c);
            }
        }

        n += c.len_utf8();
    }

    Err(ParseError::UnexpectedEnd)
}

fn as_str_token<'b>(
    v: &'b str,
    expected: Option<TokenTypes>,
    to_token: impl for<'c> FnOnce(&'c str) -> Token<'c>,
) -> Token<'b> {
    if let Some(tts) = expected {
        if !tts.contains(TokenType::F64) {
            for tt in tts.iter() {
                match tt {
                    TokenType::Str => return Token::Str(v),
                    TokenType::Field => return Token::Field(v),
                    _ => {}
                }
            }
        }
    }

    to_token(v)
}

fn parse_number<'b, S: TokenSink>(sink: &mut S, json: &'b str) -> Result<&'b str, ParseError<S::Error>> {
    let mut n = 0;
    let mut chars = json.chars();

    match chars.next().unwrap() {
        c if c.is_digit(10) => n += c.len_utf8(),
        '-' => n += 1,
        c => return Err(ParseError::UnexpectedChar(c)),
    }

    for c in chars {
        match c {
            c if c.is_digit(10) => n += c.len_utf8(),
            '.' => n += 1,
            'e' => n += 1,
            'E' => n += 1,
            '+' => n += 1,
            '-' => n += 1,
            _ => break,
        }
    }

    let token = sink.expect_tokens();

    sink.yield_token(as_number_token(&json[..n], token)?)
        .map_err(|err| ParseError::Sink(err))?;

    Ok(&json[n..])
}

fn as_number_token<'b, E: crate::error::Error>(s: &'b str, expected: Option<TokenTypes>) -> Result<Token<'b>, ParseError<E>> {
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

fn parse_in_false<'b, S: TokenSink>(
    sink: &mut S,
    json: &'b str,
) -> Result<&'b str, ParseError<S::Error>> {
    if json.starts_with("alse") {
        sink.yield_token(Token::Bool(false))
            .map_err(|err| ParseError::Sink(err))?;

        Ok(&json[4..])
    } else if let Some(c) = json.chars().next() {
        Err(ParseError::UnexpectedChar(c))
    } else {
        Err(ParseError::UnexpectedEnd)
    }
}

fn parse_in_true<'b, S: TokenSink>(
    sink: &mut S,
    json: &'b str,
) -> Result<&'b str, ParseError<S::Error>> {
    if json.starts_with("rue") {
        sink.yield_token(Token::Bool(true))
            .map_err(|err| ParseError::Sink(err))?;

        Ok(&json[3..])
    } else if let Some(c) = json.chars().next() {
        Err(ParseError::UnexpectedChar(c))
    } else {
        Err(ParseError::UnexpectedEnd)
    }
}

fn parse_in_null<'b, S: TokenSink>(
    sink: &mut S,
    json: &'b str,
) -> Result<&'b str, ParseError<S::Error>> {
    if json.starts_with("ull") {
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
                let subsink = sink
                    .yield_start(Token::Seq(SeqMeta { size_hint: None }))
                    .map_err(|err| ParseError::Sink(err))?;
                sink.end(subsink);
            }
            TokenType::Struct => {
                let subsink = sink
                    .yield_start(Token::Struct(StructMeta { fields: None }))
                    .map_err(|err| ParseError::Sink(err))?;
                sink.end(subsink);
            }
            TokenType::Tuple => {
                let subsink = sink
                    .yield_start(Token::Tuple(TupleMeta { size_hint: None }))
                    .map_err(|err| ParseError::Sink(err))?;
                sink.end(subsink);
            }
            _ => unreachable!(),
        }

        Ok(&json[3..])
    } else if let Some(c) = json.chars().next() {
        Err(ParseError::UnexpectedChar(c))
    } else {
        Err(ParseError::UnexpectedEnd)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::*;

    #[test]
    fn test_json_into_tokens_empty() {
        let cases = vec!["", " "];

        for json in cases {
            let mut got = TokenVec::new();
            json_into_tokens(&mut got, json).unwrap();
            assert_eq!(got.into_vec(), vec![]);
        }
    }

    #[test]
    fn test_json_into_tokens_null() {
        let cases = vec!["null", " null"];

        for json in cases {
            let mut got = TokenVec::new();
            json_into_tokens(&mut got, json).unwrap();
            assert_eq!(got.into_vec(), vec![OwningToken::Unit]);
        }
    }

    #[test]
    fn test_json_into_tokens_bool() {
        let cases = vec![("false", false), (" false", false), ("true", true)];

        for (json, want) in cases {
            let mut got = TokenVec::new();
            json_into_tokens(&mut got, json).unwrap();
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
            json_into_tokens(&mut got, json).unwrap();
            assert_eq!(got.into_vec(), vec![OwningToken::F64(want)]);
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
            json_into_tokens(&mut got, json).unwrap();
            assert_eq!(got.into_vec(), vec![OwningToken::Str(want.to_owned())]);
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
            json_into_tokens(&mut got, json).unwrap();
            assert_eq!(got.into_vec(), want);
        }
    }

    #[test]
    fn test_json_into_tokens_object() {
        let start = || {
            OwningToken::Struct(OwningStructMeta { fields: None })
        };
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
            json_into_tokens(&mut got, json).unwrap();
            assert_eq!(got.into_vec(), want);
        }
    }
}
