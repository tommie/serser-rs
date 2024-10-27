use std::str::FromStr as _;

use base64::prelude::*;

use crate::token::*;
use crate::TokenSink;

/// The error returned from [str_to_token].
#[derive(Debug)]
#[non_exhaustive]
pub enum StrConvertError<E: crate::error::Error> {
    /// An error returned by the [TokenSink].
    Sink(E),

    /// Something that looked like a number couldn't be parsed as one.
    ParseFloat(std::num::ParseFloatError),

    /// A number that was expected to be an integer couldn't be parsed as one.
    ParseInt(std::num::ParseIntError),

    /// A bytes string could not be decoded.
    Base64(base64::DecodeError),
}

/// Negotiates token type with the sink, and converts the string to
/// numbers, bytes or meta tokens as appropriate.
///
/// Numbers use the standard [FromStr] functions.
/// Bytes are decoded using the standard Base64 alphabet, ignoring padding.
pub(crate) fn yield_str_token<S: TokenSink>(
    sink: &mut S,
    s: &str,
    expected: Option<TokenTypes>,
    default_type: TokenType,
) -> Result<bool, StrConvertError<S::Error>> {
    if expected.is_none() || expected.unwrap().contains(default_type) {
        return sink
            .yield_token(str_token(s, default_type))
            .map_err(StrConvertError::Sink);
    }

    let exp = expected.unwrap();

    // In priority order, we prefer integers since large integers cannot be parsed as f64.
    if exp.contains_any(
        TokenTypes::new(TokenType::U128)
            .with(TokenType::U64)
            .with(TokenType::U32)
            .with(TokenType::U16)
            .with(TokenType::U8)
            .with(TokenType::Usize),
    ) {
        if exp.contains(TokenType::U128) {
            sink.yield_token(Token::U128(
                u128::from_str(s).map_err(StrConvertError::ParseInt)?,
            ))
        } else if exp.contains(TokenType::U64) {
            sink.yield_token(Token::U64(
                u64::from_str(s).map_err(StrConvertError::ParseInt)?,
            ))
        } else if exp.contains(TokenType::U32) {
            sink.yield_token(Token::U32(
                u32::from_str(s).map_err(StrConvertError::ParseInt)?,
            ))
        } else if exp.contains(TokenType::U16) {
            sink.yield_token(Token::U16(
                u16::from_str(s).map_err(StrConvertError::ParseInt)?,
            ))
        } else if exp.contains(TokenType::U8) {
            sink.yield_token(Token::U8(
                u8::from_str(s).map_err(StrConvertError::ParseInt)?,
            ))
        } else if exp.contains(TokenType::Usize) {
            sink.yield_token(Token::Usize(
                usize::from_str(s).map_err(StrConvertError::ParseInt)?,
            ))
        } else {
            unreachable!();
        }
    } else if exp.contains_any(
        TokenTypes::new(TokenType::I128)
            .with(TokenType::I64)
            .with(TokenType::I32)
            .with(TokenType::I16)
            .with(TokenType::I8)
            .with(TokenType::Isize),
    ) {
        if exp.contains(TokenType::I128) {
            sink.yield_token(Token::I128(
                i128::from_str(s).map_err(StrConvertError::ParseInt)?,
            ))
        } else if exp.contains(TokenType::I64) {
            sink.yield_token(Token::I64(
                i64::from_str(s).map_err(StrConvertError::ParseInt)?,
            ))
        } else if exp.contains(TokenType::I32) {
            sink.yield_token(Token::I32(
                i32::from_str(s).map_err(StrConvertError::ParseInt)?,
            ))
        } else if exp.contains(TokenType::I16) {
            sink.yield_token(Token::I16(
                i16::from_str(s).map_err(StrConvertError::ParseInt)?,
            ))
        } else if exp.contains(TokenType::I8) {
            sink.yield_token(Token::I8(
                i8::from_str(s).map_err(StrConvertError::ParseInt)?,
            ))
        } else if exp.contains(TokenType::Isize) {
            sink.yield_token(Token::Isize(
                isize::from_str(s).map_err(StrConvertError::ParseInt)?,
            ))
        } else {
            unreachable!();
        }
    } else if exp.contains_any(TokenTypes::new(TokenType::F64).with(TokenType::F32)) {
        if exp.contains(TokenType::F64) {
            sink.yield_token(Token::F64(
                f64::from_str(s).map_err(StrConvertError::ParseFloat)?,
            ))
        } else if exp.contains(TokenType::F32) {
            sink.yield_token(Token::F32(
                f32::from_str(s).map_err(StrConvertError::ParseFloat)?,
            ))
        } else {
            unreachable!();
        }
    } else if exp.contains(TokenType::Bytes) {
        sink.yield_token(Token::Bytes(
            BASE64_STANDARD
                .decode(s.as_bytes())
                .map_err(StrConvertError::Base64)?
                .as_slice(),
        ))
    } else {
        sink.yield_token(str_token(s, default_type))
    }
    .map_err(StrConvertError::Sink)
}

fn str_token(s: &str, tt: TokenType) -> Token {
    match tt {
        TokenType::Char if s.len() > 0 => Token::Char(s.chars().next().unwrap()),
        TokenType::Char | TokenType::Str => Token::Str(s),
        TokenType::Field => Token::Field(s),
        TokenType::Variant => Token::Variant(EnumVariant::Str(s)),
        _ => unreachable!("not a simple string token type"),
    }
}
