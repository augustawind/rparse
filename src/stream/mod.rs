//! Traits and implementations defining parsable input streams.

mod impls;
pub mod position;
pub mod state;

use std::fmt::Debug;

pub use self::position::{IndexPosition, LinePosition, NullPosition, Position};
pub use self::state::State;
use error::{Error, Errors, ParseResult};

/// SourceCode is a type alias for str `Stream` positioned by rows and columns.
pub type SourceCode = State<&'static str, LinePosition>;

/// IndexedStream is a type alias for `Stream` positioned by its index.
pub type IndexedStream<S> = State<S, IndexPosition>;

/// Tokens is an iterator over the tokens of some `Stream`.
/// It is returned by the `tokens` method of `Stream`.
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

pub trait StreamRange: Stream + PartialEq + Clone + Debug {
    fn len(&self) -> usize;
}

/// The Stream trait represents data that can be consumed by a `Parser`.
pub trait Stream: Sized + Clone + Debug {
    /// The underlying Stream type.
    type Stream: Stream<Item = Self::Item>;

    /// The Position type used to track the current parsing position.
    type Position: Position<Self::Stream>;

    /// The type of a single token.
    type Item: Copy + PartialEq + Debug;

    /// The type of a range of tokens.
    type Range: StreamRange<Item = Self::Item>;

    /// Returns the next token in the stream without consuming it.
    /// If there are no more tokens, returns `None`.
    fn peek(&self) -> Option<Self::Item>;

    /// Removes and returns the next token in the stream.
    fn pop(&mut self) -> Option<Self::Item>;

    /// Returns an iterator over the whole stream as tokens.
    /// Does not consume any input.
    fn tokens(&self) -> Tokens<Self::Item>;

    /// Consumes and returns a continuous range of the stream.
    fn range(&mut self, idx: usize) -> Option<Self::Range>;

    /// Return the current position in the stream.
    fn position(&self) -> &Self::Position;

    fn nth_position(&self, n: usize) -> Self::Position {
        let mut pos = self.position().clone();
        self.tokens().take(n).for_each(|t| pos.update(&t));
        pos
    }

    /// Return a snapshot of the current stream.
    /// The returned snapshot can be restored with `restore()`.
    fn backup(&self) -> Self {
        self.clone()
    }

    /// Reset the stream to the given state.
    /// This method is intended for use with the `backup()` method.
    fn restore(&mut self, backup: Self) {
        *self = backup;
    }

    /// Return the given parse output as a `ParseResult`, using `Self` as the `Stream` type.
    fn ok<O>(self, result: O) -> ParseResult<Self, O> {
        (Ok(result), self)
    }

    fn empty_err(&self) -> Errors<Self> {
        Errors::new(self.position().clone())
    }

    /// Return the given parse error as a `ParseResult`, using `Self` as the `Stream` type.
    fn err<O>(self, error: Error<Self>) -> ParseResult<Self, O> {
        (
            Err(Errors::from_error(self.position().clone(), error)),
            self,
        )
    }

    /// Return multiple parse errors as a `ParseResult`, using `Self` as the `Stream` type.
    fn errs<O>(self, errors: Errors<Self>) -> ParseResult<Self, O> {
        (Err(errors), self)
    }
}
