//! Types that represent parsable input.

use std::fmt::Debug;

use error::{Error, ParseResult};

pub struct Tokens<'a, T>(Box<Iterator<Item = T> + 'a>);

impl<'a, T> Tokens<'a, T> {
    fn new<I: Iterator<Item = T> + 'a>(iter: I) -> Self {
        Tokens(Box::new(iter))
    }
}

impl<'a, T> Iterator for Tokens<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

pub trait Input: Sized + Debug + Clone {
    type Item: Copy + PartialEq + Debug;
    // type Position: Position<Self::Item>;

    fn peek(&self) -> Option<Self::Item>;
    fn pop(&mut self) -> Option<Self::Item>;
    fn tokens(&self) -> Tokens<Self::Item>;

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

pub trait Position<T>: Debug + Default + Clone {
    type Position: Clone + Ord;

    fn position(&self) -> Self::Position;
    fn update(&mut self, item: &T);
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct IndexPosition(usize);

impl Default for IndexPosition {
    fn default() -> Self {
        IndexPosition(0)
    }
}

impl<T> Position<T> for IndexPosition {
    type Position = usize;

    fn position(&self) -> Self::Position {
        self.0
    }

    fn update(&mut self, _: &T) {
        self.0 += 1;
    }
}

impl From<usize> for IndexPosition {
    fn from(x: usize) -> Self {
        IndexPosition(x)
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct LinePosition {
    pub line: u32,
    pub column: u32,
}

impl Default for LinePosition {
    fn default() -> Self {
        LinePosition { line: 0, column: 0 }
    }
}

impl Position<char> for LinePosition {
    type Position = Self;

    fn position(&self) -> Self::Position {
        self.clone()
    }

    fn update(&mut self, item: &char) {
        if let &'\n' = item {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
    }
}

impl From<(u32, u32)> for LinePosition {
    fn from((line, column): (u32, u32)) -> Self {
        LinePosition { line, column }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct State<I: Input, X: Position<I::Item>> {
    pub input: I,
    pub position: X,
}

impl<I: Input, X: Position<I::Item>> State<I, X> {
    pub fn new<Y: Into<X>>(input: I, position: Y) -> Self {
        State {
            input,
            position: position.into(),
        }
    }
}

impl<I: Input, X: Position<I::Item>> From<I> for State<I, X> {
    fn from(input: I) -> Self {
        State {
            input,
            position: Default::default(),
        }
    }
}

impl<I: Input, X: Position<I::Item>, F: Into<X>> From<(I, F)> for State<I, X> {
    fn from((input, pos): (I, F)) -> Self {
        State {
            input,
            position: pos.into(),
        }
    }
}

impl<I: Input, X: Position<I::Item>> Input for State<I, X> {
    type Item = I::Item;

    fn peek(&self) -> Option<Self::Item> {
        self.input.peek()
    }

    fn pop(&mut self) -> Option<Self::Item> {
        self.input.pop().map(|item| {
            self.position.update(&item);
            item
        })
    }

    fn tokens(&self) -> Tokens<Self::Item> {
        self.input.tokens()
    }
}

pub type SourceCode = State<&'static str, LinePosition>;

pub type IndexedInput<I> = State<I, IndexPosition>;

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

    fn tokens(&self) -> Tokens<Self::Item> {
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

    fn tokens(&self) -> Tokens<Self::Item> {
        Tokens::new(self.chars())
    }
}
