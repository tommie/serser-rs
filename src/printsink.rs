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

pub struct PrintingTokenSink<'a, S: TokenSink, W: std::io::Write>(&'a mut S, &'a str, W, usize);

impl<'a, S: TokenSink, W: std::io::Write> PrintingTokenSink<'a, S, W> {
    pub fn new(sink: &'a mut S, w: W, prefix: &'a str) -> Self {
        Self(sink, prefix, w, 0)
    }

    fn print_token(&mut self, token: &Token<'_>) -> std::io::Result<()> {
        if token.is_end() {
            self.3 -= 1;
        }

        writeln!(
            self.2,
            "{}{:indent$}{:?}",
            self.1,
            "",
            token,
            indent = 2 * self.3
        )?;

        if token.is_start() {
            self.3 += 1;
        }

        Ok(())
    }
}

impl<'a, S: TokenSink, W: std::io::Write> TokenSink for PrintingTokenSink<'a, S, W> {
    type Error = PrintError<S::Error>;

    fn yield_token<'b>(&mut self, token: Token<'b>) -> Result<bool, Self::Error> {
        self.print_token(&token)
            .map_err(|err| PrintError::Print(err))?;

        self.0
            .yield_token(token)
            .map_err(|err| PrintError::Downstream(err))
    }

    fn expect_tokens(&mut self) -> Option<TokenTypes> {
        self.0.expect_tokens()
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
