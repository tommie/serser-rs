use std::io;

use super::options::*;
use crate::convert_str::yield_str_token;
use crate::convert_str::StrConvertError;
use crate::token::*;
use crate::utf8read::CharRead;
use crate::utf8read::UTF8Read;
use crate::FromTokenSink;
use crate::FromTokens;
use crate::TokenSink;

/// The error returned from [csv_into_tokens].
#[derive(Debug)]
#[non_exhaustive]
pub enum ParseError<E: crate::error::Error> {
    /// An error returned by the text reader.
    Read(io::Error),

    /// An error returned by the [TokenSink].
    Sink(E),

    /// An error returned by a conversion from string.
    StrConvert(StrConvertError<E>),

    /// The CSV data ended prematurely.
    UnexpectedEnd,
}

/// Parses a CSV string into a token sink.
pub fn csv_into<'a, F: FromTokens, R: io::Read>(
    options: &'a CsvOptions<'a>,
    r: R,
) -> Result<F, ParseError<<<F as FromTokenSink>::Sink as TokenSink>::Error>> {
    let mut sink = F::new_sink();
    csv_into_tokens(&mut sink, options, r)?;

    F::from_sink(sink).ok_or(ParseError::UnexpectedEnd)
}

/// Parses a CSV string into a token sink.
pub fn csv_into_tokens<'a, S: TokenSink, R: io::Read>(
    sink: &mut S,
    options: &'a CsvOptions<'a>,
    r: R,
) -> Result<(), ParseError<S::Error>> {
    parse_value(sink, options, &mut UTF8Read::new(r))
}

fn parse_value<'a, S: TokenSink, R: CharRead>(
    sink: &mut S,
    options: &'a CsvOptions<'a>,
    r: &mut R,
) -> Result<(), ParseError<S::Error>> {
    #[derive(Eq, PartialEq)]
    enum InQuote {
        No,
        Yes,
        Quote,
    }

    let num_levels = options.separators.len();
    let mut s = String::new();
    let mut level: usize = 0;
    let mut in_quote = InQuote::No;

    loop {
        let c = if let Some(c) = read_char(r)? {
            c
        } else {
            if level > 0 {
                yield_str(sink, s.as_str())?;
                yield_for_end(sink, level, 0)?;
            }

            return Ok(());
        };

        if options.quote == Some(c) {
            match in_quote {
                InQuote::No => {
                    in_quote = InQuote::Yes;
                    yield_for_start(sink, level, num_levels - 1)?;
                    level = num_levels - 1;
                    continue;
                }
                InQuote::Yes => {
                    in_quote = InQuote::Quote;
                    continue;
                }
                InQuote::Quote => {
                    in_quote = InQuote::Yes;
                }
            }
        } else if in_quote == InQuote::Quote {
            in_quote = InQuote::No;
        }

        if in_quote == InQuote::No {
            if let Some(sep) = options.separators.iter().position(|x| x == &c) {
                yield_str(sink, s.as_str())?;
                s.clear();
                yield_for_end(sink, level, sep)?;
                level = sep;
                continue;
            }
        }

        s.push(c);
        yield_for_start(sink, level, num_levels - 1)?;
        level = num_levels - 1;
    }
}

fn read_char<R: CharRead, E: crate::error::Error>(
    r: &mut R,
) -> Result<Option<char>, ParseError<E>> {
    r.read_char().map_err(ParseError::Read)
}

fn yield_str<S: TokenSink>(sink: &mut S, s: &str) -> Result<bool, ParseError<S::Error>> {
    let expected = sink.expect_tokens();
    yield_str_token(sink, s, expected, TokenType::Str).map_err(ParseError::StrConvert)
}

fn yield_for_start<S: TokenSink>(
    sink: &mut S,
    mut level: usize,
    num_levels: usize,
) -> Result<(), ParseError<S::Error>> {
    while level < num_levels {
        sink.yield_token(Token::Tuple(TupleMeta { size_hint: None }))
            .map_err(ParseError::Sink)?;
        level += 1;
    }

    Ok(())
}

fn yield_for_end<S: TokenSink>(
    sink: &mut S,
    mut level: usize,
    sep: usize,
) -> Result<(), ParseError<S::Error>> {
    while level > sep {
        sink.yield_token(Token::EndTuple)
            .map_err(ParseError::Sink)?;
        level -= 1;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test::ExpectingTokenSink;
    use crate::*;

    #[test]
    fn test_csv_into_tokens_empty() {
        let mut got = TokenVec::new();
        csv_into_tokens(&mut got, COMMA, "".as_bytes()).unwrap();
        assert_eq!(got.into_vec(), vec![]);
    }

    #[test]
    fn test_csv_into_tokens_str() {
        let cases = vec![
            (r#""""#, ""),
            (r#"a"#, "a"),
            (r#"abc"#, "abc"),
            (r#""a""#, "a"),
            (r#""abc""#, "abc"),
            (r#"❤️"#, "❤️"),
            (r#""""""#, "\""),
            ("\"\n\"", "\n"),
        ];

        for (csv, want) in cases {
            let mut got = TokenVec::new();
            csv_into_tokens(&mut got, COMMA, csv.as_bytes()).unwrap();
            assert_eq!(
                got.into_vec(),
                vec![
                    OwningToken::Tuple(TupleMeta { size_hint: None }),
                    OwningToken::Str(want.to_owned()),
                    OwningToken::EndTuple,
                ]
            );
        }
    }

    #[test]
    fn test_csv_into_tokens_bytes() {
        let cases = vec![(r#""""#, b"".as_slice()), (r#""AA==""#, b"\x00".as_slice())];

        for (csv, want) in cases {
            let mut got = TokenVec::new();
            let mut expsink =
                ExpectingTokenSink::new(&mut got, |_| Some(TokenTypes::new(TokenType::Bytes)));
            csv_into_tokens(&mut expsink, COMMA, csv.as_bytes()).unwrap();
            assert_eq!(
                got.into_vec(),
                vec![
                    OwningToken::Tuple(TupleMeta { size_hint: None }),
                    OwningToken::Bytes(want.to_owned()),
                    OwningToken::EndTuple,
                ]
            );
        }
    }

    #[test]
    fn test_csv_into_tokens_tuple() {
        let start = || OwningToken::Tuple(TupleMeta { size_hint: None });
        let end = || OwningToken::EndTuple;
        let cases = vec![
            ("a", vec![start(), OwningToken::Str("a".to_string()), end()]),
            (
                "a,b",
                vec![
                    start(),
                    OwningToken::Str("a".to_string()),
                    OwningToken::Str("b".to_string()),
                    end(),
                ],
            ),
            (
                "a,b,",
                vec![
                    start(),
                    OwningToken::Str("a".to_string()),
                    OwningToken::Str("b".to_string()),
                    OwningToken::Str("".to_string()),
                    end(),
                ],
            ),
            (
                "a\nb",
                vec![
                    start(),
                    OwningToken::Str("a".to_string()),
                    end(),
                    start(),
                    OwningToken::Str("b".to_string()),
                    end(),
                ],
            ),
            (
                "a\nb\n",
                vec![
                    start(),
                    OwningToken::Str("a".to_string()),
                    end(),
                    start(),
                    OwningToken::Str("b".to_string()),
                    end(),
                ],
            ),
        ];

        for (csv, want) in cases {
            let mut got = TokenVec::new();
            csv_into_tokens(&mut got, COMMA, csv.as_bytes()).unwrap();
            assert_eq!(got.into_vec(), want);
        }
    }
}
