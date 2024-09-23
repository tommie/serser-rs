use std::io;

pub(crate) trait CharRead {
    fn read_char(&mut self) -> Result<Option<char>, io::Error>;
    fn read_match(&mut self, s: &str) -> Result<(), io::Error>;
    fn push_char(&mut self, c: char) -> Result<(), io::Error>;
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
            return Ok(Some(c));
        }

        let mut buf: [u8; 3] = [0; 3];

        if self.r.read(&mut buf[..1])? < 1 {
            return Ok(None);
        }

        let (mut c, n) = if buf[0] & 0x80 == 0 {
            return Ok(Some(buf[0] as char));
        } else if buf[0] & 0xE0 == 0xC0 {
            ((buf[0] & 0x1F) as u32, 1)
        } else if buf[0] & 0xF0 == 0xE0 {
            ((buf[0] & 0x0F) as u32, 2)
        } else if buf[0] & 0xF8 == 0xF0 {
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
        let mut buf = Vec::<u8>::with_capacity(s.len());
        buf.resize(s.len(), 0);

        self.r.read_exact(buf.as_mut_slice())?;

        if s.as_bytes() != buf.as_slice() {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "failed match"));
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
}
