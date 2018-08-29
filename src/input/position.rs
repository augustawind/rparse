//! Traits and implementations representing positions in an `Input` stream.

use std::fmt::Debug;

/// The Position trait defines types that keep track of the cursor position while parsing an
/// `Input` stream.
pub trait Position<T>: Debug + Default + Clone {
    type Position: Clone + Ord;

    fn position(&self) -> Self::Position;
    fn update(&mut self, item: &T);
}

/// IndexPosition is a `Position` which is represented as an index.
/// This is useful for binary data or any kind of virtual input stream.
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

/// LinePosition is a `Position` which is represented as a line number and column number.
/// This is primarily useful for parsing text files or anything that has multiple lines.
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
