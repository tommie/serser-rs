use std::fmt;

use crate::error::*;
use crate::token::*;
use crate::TokenSink;

/// The error type used in [PrintingTokenSink].
#[derive(Debug)]
pub enum PrintError<DE: Error> {
    /// The downstream sink returned an error.
    Downstream(DE),

    /// The [writer](std::io::Write) returned an error.
    Print(std::io::Error),
}

impl<DE: Error> Error for PrintError<DE> {
    fn invalid_token(token: Token<'_>, expected: Option<TokenTypes>) -> Self {
        Self::Downstream(DE::invalid_token(token, expected))
    }

    fn invalid_field(field: &str) -> Self {
        Self::Downstream(DE::invalid_field(field))
    }

    fn missing_fields(fields: &[&str]) -> Self {
        Self::Downstream(DE::missing_fields(fields))
    }

    fn invalid_variant(variant: EnumVariant<'_>) -> Self {
        Self::Downstream(DE::invalid_variant(variant))
    }

    fn unexpected_end(expected: Option<TokenTypes>) -> Self {
        Self::Downstream(DE::unexpected_end(expected))
    }
}

impl<DE: Error> fmt::Display for PrintError<DE> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Downstream(err) => fmt::Display::fmt(err, f),
            Self::Print(err) => fmt::Display::fmt(err, f),
        }
    }
}

impl<DE: Error> std::error::Error for PrintError<DE> {}

/// A token sink that prints a description of the tokens to a writer,
/// and forwards them to another sink. For debugging.
///
/// Each token is output on one line. Start and end tokens adjust
/// indentation.
///
/// ## Example
///
/// ```
/// # use crate::serser::*;
/// # use crate::serser::test::*;
/// let mut got = TokenVec::new();
/// let mut output = Vec::new();
/// vec![42u32]
///     .into_tokens(&mut PrintingTokenSink::new(&mut got, &mut output, "T: "))
///     .unwrap();
/// assert_eq!(
///     String::from_utf8(output).unwrap(),
///     concat!(
///         "T: Seq(SeqMeta { size_hint: Some(1) })\n",
///         "T:   U32(42)\n",
///         "T: EndSeq\n")
/// );
/// ```
pub struct PrintingTokenSink<'a: 'b, 'b, S: TokenSink, W: std::io::Write> {
    sink: &'b mut S,
    prefix: &'a str,
    writer: W,
    indent: usize,
}

impl<'a: 'b, 'b, S: TokenSink, W: std::io::Write> PrintingTokenSink<'a, 'b, S, W> {
    /// Creates a new printing sink. The prefix is used for each
    /// output line, before the indentation.
    pub fn new(sink: &'b mut S, writer: W, prefix: &'a str) -> Self {
        Self {
            sink,
            prefix,
            writer,
            indent: 0,
        }
    }

    fn print_token(&mut self, token: &Token<'_>, indent: usize) -> std::io::Result<()> {
        let prefix = self.prefix;

        writeln!(
            self.writer,
            "{}{:indent$}{:?}",
            prefix,
            "",
            token,
            indent = 2 * indent
        )?;

        Ok(())
    }
}

impl<'a: 'b, 'b, S: TokenSink, W: std::io::Write> TokenSink for PrintingTokenSink<'a, 'b, S, W> {
    type Error = PrintError<S::Error>;

    fn yield_token(&mut self, token: Token<'_>) -> Result<bool, Self::Error> {
        if token.is_end() {
            self.indent -= 1;
        }

        self.print_token(&token, self.indent)
            .map_err(|err| PrintError::Print(err))?;

        if token.is_start() {
            self.indent += 1;
        }

        self.sink
            .yield_token(token)
            .map_err(|err| PrintError::Downstream(err))
    }

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        self.sink.expect_tokens()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::into::*;
    use crate::vec::*;

    #[test]
    fn test_printing_token_sink() {
        let mut got = TokenVec::new();
        let mut output = Vec::new();
        vec![42u32]
            .into_tokens(&mut PrintingTokenSink::new(&mut got, &mut output, "T: "))
            .unwrap();
        assert_eq!(
            String::from_utf8(output).unwrap(),
            "T: Seq(SeqMeta { size_hint: Some(1) })\nT:   U32(42)\nT: EndSeq\n"
        );
    }
}
