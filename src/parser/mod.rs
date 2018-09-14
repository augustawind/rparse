//! A collection of various parsers and combinators.
//!
//! Defines the `Parser` trait.

#[cfg(test)]
#[macro_use]
mod test_utils;

#[macro_use]
pub mod choice;
#[macro_use]
pub mod combinator;
pub mod function;
pub mod range;
pub mod token;

use std::fmt::Display;
use std::iter::FromIterator;
use std::str;

use self::choice::{and, or, And, Or};
use self::combinator::{append, then, Append, Then};
use self::function::{bind, from_str, map, Bind, FromStr, Map, StrLike};
use error::ParseResult;
use stream::{Stream, ToStream};

pub trait Parser {
    type Stream: Stream;
    type Output;

    fn parse_stream(&mut self, Self::Stream) -> ParseResult<Self::Stream, Self::Output>;

    fn parse(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output>
    where
        Self: Sized,
    {
        let backup = stream.backup();
        let mut result = self.parse_stream(stream);
        if let (Err(_), ref mut stream) = result {
            stream.restore(backup);
        }
        result
    }

    fn map<F, O>(self, f: F) -> Map<Self, F>
    where
        Self: Sized,
        F: Fn(Self::Output) -> O,
    {
        map(self, f)
    }

    fn bind<F, O>(self, f: F) -> Bind<Self, F>
    where
        Self: Sized,
        F: Fn(Self::Output, Self::Stream) -> O,
    {
        bind(self, f)
    }

    fn from_str<O>(self) -> FromStr<Self, O>
    where
        Self: Sized,
        Self::Output: StrLike,
        O: str::FromStr,
        O::Err: Display,
    {
        from_str(self)
    }

    fn and<P>(self, other: P) -> And<Self, P>
    where
        Self: Sized,
        P: Parser<Stream = Self::Stream, Output = Self::Output>,
    {
        and(self, other)
    }

    fn or<P>(self, other: P) -> Or<Self, P>
    where
        Self: Sized,
        P: Parser<Stream = Self::Stream, Output = Self::Output>,
    {
        or(self, other)
    }

    fn then<P, O>(self, other: P) -> Then<Self, P, O>
    where
        Self: Sized,
        P: Parser<Stream = Self::Stream, Output = Self::Output>,
        O: FromIterator<Self::Output>,
    {
        then(self, other)
    }

    fn append<P, O>(self, other: P) -> Append<Self, P, Self::Output>
    where
        Self: Sized,
        Self::Output: Extend<O>,
        P: Parser<Stream = Self::Stream, Output = O>,
    {
        append(self, other)
    }

    /// Convert this Parser's output to some `Stream` type.
    fn s<S>(self) -> Map<Self, fn(Self::Output) -> S>
    where
        Self: Sized,
        Self::Output: ToStream<S>,
        S: Stream,
    {
        map(self, |output: Self::Output| output.to_stream())
    }
}

impl<'a, S: Stream, O> Parser for FnMut(S) -> ParseResult<S, O> + 'a {
    type Stream = S;
    type Output = O;

    fn parse_stream(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        self(stream)
    }
}

impl<S: Stream, O> Parser for fn(S) -> ParseResult<S, O> {
    type Stream = S;
    type Output = O;

    fn parse_stream(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        self(stream)
    }
}

pub fn parser<S: Stream, O>(f: fn(S) -> ParseResult<S, O>) -> fn(S) -> ParseResult<S, O> {
    f
}

#[cfg(test)]
mod test {

    use super::*;
    use error::Error;
    use stream::{IndexedStream, Position};

    fn vowel<S>() -> impl Parser<Stream = S, Output = char>
    where
        S: Stream<Item = char>,
        S::Position: Position<char>,
        char: ToStream<S> + ToStream<S::Range>,
    {
        parser(|mut stream: S| match stream.pop() {
            Some(t) => match t {
                'a' | 'e' | 'i' | 'o' | 'u' => stream.ok(t),
                _ => stream.err(Error::unexpected_token(t)),
            },
            _ => stream.err(Error::EOF),
        })
    }

    #[test]
    fn test_parser() {
        test_parser!(IndexedStream<&str> | vowel(), {
            "a" => (Ok('a'), "", 1);
            "ooh" => (Ok('o'), "oh", 1);
        });
        test_parser_errors!(IndexedStream<&str> | vowel(), {
            "" => at 0; vec![Error::EOF];
            "d" => at 1; vec![Error::unexpected_token('d')];
            "du" => at 1; vec![Error::unexpected_token('d')];
        });
    }
}
