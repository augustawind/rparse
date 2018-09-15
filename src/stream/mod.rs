//! Traits and implementations defining parsable input streams.

pub mod position;
mod primitives;
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

/// The Stream trait represents data that can be consumed by a `Parser`.
pub trait Stream: Sized + Debug + Clone {
    /// The type of a single token.
    type Item: Copy + PartialEq + Debug + ToStream<Self> + ToStream<Self::Range>;

    /// The type of a series of tokens.
    type Range: RangeStream<Item = Self::Item>;

    /// The Position type used to track the current parsing position.
    type Position: Position<Self::Item>;

    fn from_token(token: Self::Item) -> Self;

    fn from_range(range: Self::Range) -> Self;

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
    fn position(&self) -> Self::Position;

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

    fn empty_err(&self) -> Errors<Self, Self::Position> {
        Errors::new(self.position())
    }

    /// Return the given parse error as a `ParseResult`, using `Self` as the `Stream` type.
    fn err<O>(self, error: Error<Self::Range>) -> ParseResult<Self, O> {
        (Err(Errors::from_error(self.position(), error)), self)
    }

    /// Return multiple parse errors as a `ParseResult`, using `Self` as the `Stream` type.
    fn errs<O>(self, errors: Errors<Self, Self::Position>) -> ParseResult<Self, O> {
        (Err(errors), self)
    }
}

/// The RangeStream trait is a `Stream` that can be used as a continuous range of tokens.
/// It supports equality comparisons and a `len` method, but does not track its parsing position.
pub trait RangeStream: Stream<Position = NullPosition, Range = Self> + PartialEq {
    fn len(&self) -> usize;
}

/// The ToStream trait provides an interface that converts its implementor to a `Stream`.
pub trait ToStream<S: Stream> {
    fn to_stream(self) -> S;
}

/// The FromStream trait is the inverse of `ToStream`. It is implemented automatically for
/// `Stream` types whose `ToStream` converts to another `Stream` type.
pub trait FromStream<S: Stream> {
    fn from_stream(stream: S) -> Self;
}

impl<S: Stream + ToStream<T>, T: Stream> FromStream<S> for T {
    fn from_stream(stream: S) -> Self {
        stream.to_stream()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_to_stream_str_for_str() {
        assert_eq!(<&str as ToStream<&str>>::to_stream("yee"), "yee");
        assert_eq!(<(&str)>::from_stream("yee"), "yee");
    }

    #[test]
    fn test_to_stream_str_for_char() {
        assert_eq!(<char as ToStream<&str>>::to_stream('x'), "x");
    }

    #[test]
    fn test_to_stream_string_for_string() {
        assert_eq!(
            <String as ToStream<String>>::to_stream("yee".to_string()),
            String::from("yee")
        );
        assert_eq!(String::from_stream("yee".to_string()), String::from("yee"));
    }

    #[test]
    fn test_to_stream_string_for_char() {
        assert_eq!(
            <char as ToStream<String>>::to_stream('x'),
            String::from("x")
        );
    }

    #[test]
    fn test_to_stream_string_for_str() {
        assert_eq!(
            <&str as ToStream<String>>::to_stream("yee"),
            String::from("yee")
        );
        assert_eq!(String::from_stream("yee"), String::from("yee"));
    }
}
