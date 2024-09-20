use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

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
pub struct PrintingTokenSink<'a, S: TokenSink, W: std::io::Write> {
    inner: Rc<RefCell<PrintingInner<'a, W>>>,
    sink: &'a mut S,
    indent: usize,
}

/// The inner aspects of the printer, shared between all subsinks.
struct PrintingInner<'a, W: std::io::Write> {
    prefix: &'a str,
    writer: W,
}

impl<'a, S: TokenSink, W: std::io::Write> PrintingTokenSink<'a, S, W> {
    /// Creates a new printing sink. The prefix is used for each
    /// output line, before the indentation.
    pub fn new(sink: &'a mut S, writer: W, prefix: &'a str) -> Self {
        Self {
            inner: Rc::new(RefCell::new(PrintingInner { prefix, writer })),
            sink,
            indent: 0,
        }
    }
}

impl<'a, W: std::io::Write> PrintingInner<'a, W> {
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

impl<'a, S: TokenSink, W: std::io::Write> TokenSink for PrintingTokenSink<'a, S, W> {
    type Error = PrintError<S::Error>;
    type Subsink<'c> = PrintingTokenSubsink<'a, S::Subsink<'c>, W> where Self: 'c;

    fn yield_start<'b, 'c>(&mut self, token: Token<'b>) -> Result<Self::Subsink<'c>, Self::Error>
    where
        Self: 'c,
    {
        let mut inner = self.inner.borrow_mut();

        inner
            .print_token(&token, self.indent - if token.is_end() { 1 } else { 0 })
            .map_err(|err| PrintError::Print(err))?;

        let sink = self
            .sink
            .yield_start(token)
            .map_err(|err| PrintError::Downstream(err))?;

        Ok(PrintingTokenSubsink {
            inner: self.inner.clone(),
            sink,
            indent: self.indent + 1,
        })
    }

    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error> {
        let mut inner = self.inner.borrow_mut();

        inner
            .print_token(&token, self.indent - if token.is_end() { 1 } else { 0 })
            .map_err(|err| PrintError::Print(err))?;

        self.sink
            .yield_token(token)
            .map_err(|err| PrintError::Downstream(err))
    }

    fn end<'b>(&mut self, _sink: Self::Subsink<'b>)
    where
        Self: 'b,
    {
    }

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        self.sink.expect_tokens()
    }
}

pub struct PrintingTokenSubsink<'a, S: TokenSink, W: std::io::Write> {
    inner: Rc<RefCell<PrintingInner<'a, W>>>,
    sink: S,
    indent: usize,
}

impl<'a, S: TokenSink, W: std::io::Write> TokenSink for PrintingTokenSubsink<'a, S, W> {
    type Error = PrintError<S::Error>;
    type Subsink<'c> = PrintingTokenSubsink<'a, S::Subsink<'c>, W> where Self: 'c;

    fn yield_start<'b, 'c>(&mut self, token: Token<'b>) -> Result<Self::Subsink<'c>, Self::Error>
    where
        Self: 'c,
    {
        let mut inner = self.inner.borrow_mut();

        inner
            .print_token(&token, self.indent - if token.is_end() { 1 } else { 0 })
            .map_err(|err| PrintError::Print(err))?;

        let sink = self
            .sink
            .yield_start(token)
            .map_err(|err| PrintError::Downstream(err))?;

        Ok(PrintingTokenSubsink {
            inner: self.inner.clone(),
            sink,
            indent: self.indent + 1,
        })
    }

    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error> {
        let mut inner = self.inner.borrow_mut();

        inner
            .print_token(&token, self.indent - if token.is_end() { 1 } else { 0 })
            .map_err(|err| PrintError::Print(err))?;

        self.sink
            .yield_token(token)
            .map_err(|err| PrintError::Downstream(err))
    }

    fn end<'b>(&mut self, _sink: Self::Subsink<'b>)
    where
        Self: 'b,
    {
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
