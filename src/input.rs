//! Types that represent parsable input.

use std::fmt::Debug;

use error::{Error, ParseResult};

pub struct Tokens<'a, I: Input>(Box<Iterator<Item = I::Item> + 'a>);

impl<'a, I: Input> Tokens<'a, I> {
    fn new<T: Iterator<Item = I::Item> + 'a>(t: T) -> Self {
        Tokens(Box::new(t))
    }
}

impl<'a, I: Input> Iterator for Tokens<'a, I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

pub trait Input: Sized + Debug + Clone {
    type Item: Copy + PartialEq + Debug;

    fn peek(&self) -> Option<Self::Item>;
    fn pop(&mut self) -> Option<Self::Item>;

    fn tokens(&self) -> Tokens<Self>;

    fn backup(&self) -> Self {
        self.clone()
    }

    fn restore(&mut self, backup: Self) {
        *self = backup;
    }

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

    fn tokens(&self) -> Tokens<Self> {
        Tokens::new(self.chars())
    }
}

impl Input for String {
    type Item = char;

    fn peek(&self) -> Option<Self::Item> {
        self.chars().next()
    }

    fn pop(&mut self) -> Option<Self::Item> {
        if self.is_empty() {
            None
        } else {
            Some(self.remove(0))
        }
    }

    fn tokens(&self) -> Tokens<Self> {
        Tokens::new(self.chars())
    }
}
