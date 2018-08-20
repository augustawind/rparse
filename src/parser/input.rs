use super::{Error, ParseResult};

pub trait Input: Sized {
    type Item: Copy;

    fn peek(&self) -> Option<Self::Item>;
    fn pop(&mut self) -> Option<Self::Item>;

    fn foreach<F>(&self, F)
    where
        F: FnMut(Self::Item);

    fn ok<O>(self, result: O) -> ParseResult<Self, O> {
        (Ok(result), self)
    }

    fn err<O>(self, error: Error<Self>) -> ParseResult<Self, O> {
        (Err(error), self)
    }
}

impl<'a> Input for &'a str {
    type Item = char;

    fn peek(&self) -> Option<Self::Item> {
        self.chars().next()
    }

    fn pop(&mut self) -> Option<Self::Item> {
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
        F: FnMut(Self::Item),
    {
        self.chars().for_each(f);
    }
}
