use super::{Error, ParseResult};

pub trait Input: Sized {
    type Token: Copy;

    fn peek(&self) -> Option<Self::Token>;
    fn pop(&mut self) -> Option<Self::Token>;

    fn foreach<F>(&self, F)
    where
        F: FnMut(Self::Token);

    fn ok<O>(self, result: O) -> ParseResult<Self, O> {
        ParseResult::Ok((self, result))
    }

    fn err<O>(self, error: Error) -> ParseResult<Self, O> {
        ParseResult::Err((self, error))
    }
}

impl<'a> Input for &'a str {
    type Token = char;

    fn peek(&self) -> Option<Self::Token> {
        self.chars().next()
    }

    fn pop(&mut self) -> Option<Self::Token> {
        let mut iter = self.char_indices();
        iter.next().map(|(_, c)| {
            match iter.next() {
                Some((n, _)) => *self = &self[n..],
                None => *self = &self[..0],
            }

            c
        })
    }

    fn foreach<F>(&self, f: F)
    where
        F: FnMut(Self::Token),
    {
        self.chars().for_each(f);
    }
}
