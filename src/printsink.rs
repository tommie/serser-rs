use std::cell::RefCell;
use std::rc::Rc;

use crate::error::*;
use crate::token::*;
use crate::TokenSink;

#[derive(Debug)]
pub enum PrintError<DE: Error> {
    Downstream(DE),
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

pub struct PrintingTokenSink<'a, S: TokenSink, W: std::io::Write>(
    Rc<RefCell<PrintingInner<'a, S, W>>>,
    usize,
);
struct PrintingInner<'a, S: TokenSink, W: std::io::Write> {
    sink: &'a mut S,
    prefix: &'a str,
    writer: W,
}

impl<'a, S: TokenSink, W: std::io::Write> PrintingTokenSink<'a, S, W> {
    pub fn new(sink: &'a mut S, writer: W, prefix: &'a str) -> Self {
        Self(
            Rc::new(RefCell::new(PrintingInner {
                sink,
                prefix,
                writer,
            })),
            0,
        )
    }
}

impl<'a, S: TokenSink, W: std::io::Write> PrintingInner<'a, S, W> {
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
    type Subsink<'c> = PrintingTokenSink<'a, S, W> where Self: 'c;

    fn yield_start<'b, 'c>(&mut self, token: Token<'b>) -> Result<Self::Subsink<'c>, Self::Error>
    where
        Self: 'c,
    {
        self.yield_token(token)?;

        Ok(PrintingTokenSink(self.0.clone(), self.1 + 1))
    }

    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error> {
        let mut inner = self.0.borrow_mut();

        inner
            .print_token(&token, self.1 - if token.is_end() { 1 } else { 0 })
            .map_err(|err| PrintError::Print(err))?;

        inner
            .sink
            .yield_token(token)
            .map_err(|err| PrintError::Downstream(err))
    }

    fn end<'b>(&mut self, _sink: Self::Subsink<'b>)
    where
        Self: 'b,
    {
    }

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        self.0.borrow_mut().sink.expect_tokens()
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
