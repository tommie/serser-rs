use std::cmp::min;
use std::io;

pub(crate) trait CharRead {
    fn read_char(&mut self) -> Result<Option<char>, io::Error>;

    /// Like read_char, but assumes there is no character pushed back.
    fn read_char_nopop(&mut self) -> Result<Option<char>, io::Error>;

    fn read_match(&mut self, s: &str) -> Result<(), io::Error>;
    fn push_char(&mut self, c: char) -> Result<(), io::Error>;
    fn can_pop_char(&self) -> bool;
}

/// Reads UTF-8 characters.
///
/// Provides a one character push-back buffer.
pub(crate) struct UTF8Read<R: io::Read> {
    r: R,
    back: Option<char>,
}

impl<R: io::Read> UTF8Read<R> {
    pub fn new(r: R) -> Self {
        Self { r, back: None }
    }
}

impl<R: io::Read> CharRead for UTF8Read<R> {
    fn read_char(&mut self) -> Result<Option<char>, io::Error> {
        if let Some(c) = self.back.take() {
            Ok(Some(c))
        } else {
            self.read_char_nopop()
        }
    }

    fn read_char_nopop(&mut self) -> Result<Option<char>, io::Error> {
        let mut buf: [u8; 4] = [0; 4];

        if self.r.read(&mut buf[..1])? < 1 {
            return Ok(None);
        }

        if buf[0] < 0x80 {
            return Ok(Some(buf[0] as char));
        }

        let (mut c, n) = if buf[0] < 0xE0 {
            if buf[0] < 0xC0 {
                return Err(io::Error::new(io::ErrorKind::InvalidData, "bad UTF-8"));
            }

            ((buf[0] & 0x1F) as u32, 1)
        } else if buf[0] < 0xF0 {
            ((buf[0] & 0x0F) as u32, 2)
        } else if buf[0] < 0xF8 {
            ((buf[0] & 0x07) as u32, 3)
        } else {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "bad UTF-8"));
        };

        self.r.read_exact(&mut buf[..n])?;

        for i in 0..n {
            if buf[i] & 0xC0 != 0x80 {
                return Err(io::Error::new(io::ErrorKind::InvalidData, "bad UTF-8"));
            }

            c <<= 6;
            c |= (buf[i] & 0x3F) as u32;
        }

        Ok(Some(unsafe { char::from_u32_unchecked(c) }))
    }

    fn read_match(&mut self, s: &str) -> Result<(), io::Error> {
        const N: usize = 16;

        let mut buf: [u8; N] = [0; N];
        let mut sb = s.as_bytes();

        while !sb.is_empty() {
            let n = min(N, sb.len());
            let b = &mut buf[..n];
            self.r.read_exact(b)?;

            if &sb[..n] != b {
                return Err(io::Error::new(io::ErrorKind::InvalidData, "failed match"));
            }

            sb = &sb[n..];
        }

        Ok(())
    }

    fn push_char(&mut self, c: char) -> Result<(), io::Error> {
        if self.back.is_some() {
            Err(io::Error::from(io::ErrorKind::WriteZero))
        } else {
            self.back = Some(c);
            Ok(())
        }
    }

    fn can_pop_char(&self) -> bool {
        self.back.is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn utf8_read_char_empty() {
        let mut ur = UTF8Read::new("".as_bytes());
        assert_eq!(ur.read_char().unwrap(), None);
    }

    #[test]
    fn utf8_read_char_last_byte() {
        let mut ur = UTF8Read::new("a".as_bytes());
        assert_eq!(ur.read_char().unwrap(), Some('a'));
    }

    #[test]
    fn utf8_read_char_byte() {
        let mut ur = UTF8Read::new("ab".as_bytes());
        assert_eq!(ur.read_char().unwrap(), Some('a'));
    }

    #[test]
    fn utf8_read_char_last_unicode2() {
        let mut ur = UTF8Read::new("\u{07FF}".as_bytes());
        assert_eq!(ur.read_char().unwrap(), Some('\u{07FF}'));
    }

    #[test]
    fn utf8_read_char_last_unicode3() {
        let mut ur = UTF8Read::new("\u{FFFF}".as_bytes());
        assert_eq!(ur.read_char().unwrap(), Some('\u{FFFF}'));
    }

    #[test]
    fn utf8_read_char_last_unicode4() {
        let mut ur = UTF8Read::new("\u{10FFFF}".as_bytes());
        assert_eq!(ur.read_char().unwrap(), Some('\u{10FFFF}'));
    }

    #[test]
    fn utf8_read_char_unicode4() {
        let mut ur = UTF8Read::new("\u{10FFFF}b".as_bytes());
        assert_eq!(ur.read_char().unwrap(), Some('\u{10FFFF}'));
    }

    #[test]
    fn utf8_read_match_short() {
        let mut ur = UTF8Read::new("Hello".as_bytes());
        ur.read_match("Hello").unwrap();
    }

    #[test]
    fn utf8_read_match_long() {
        let mut ur = UTF8Read::new("It's a small world after all".as_bytes());
        ur.read_match("It's a small world after all").unwrap();
    }

    #[test]
    fn utf8_read_match_short_fail() {
        let mut ur = UTF8Read::new("Hello".as_bytes());
        assert_eq!(ur.read_match("World").unwrap_err().kind(), io::ErrorKind::InvalidData);
    }

    #[test]
    fn utf8_read_match_long_fail() {
        let mut ur = UTF8Read::new("It's a small world after all".as_bytes());
        assert_eq!(ur.read_match("It's a small world after?").unwrap_err().kind(), io::ErrorKind::InvalidData);
    }

    #[test]
    fn utf8_push_read() {
        let mut ur = UTF8Read::new("".as_bytes());
        ur.push_char('a').unwrap();
        assert_eq!(ur.read_char().unwrap(), Some('a'));
    }
}
