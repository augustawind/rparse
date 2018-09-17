//! A collection of various parsers and combinators.
//!
//! Defines the `Parser` trait.

#[macro_use]
mod test_utils;

#[macro_use]
pub mod choice;
#[macro_use]
pub mod seq;
pub mod function;
pub mod range;
pub mod token;

use std::fmt::Display;
use std::iter::FromIterator;
use std::str;

use self::choice::{and, optional, or, And, Optional, Or};
use self::function::{
    bind, collect, flatten, from_str, iter, map, wrap, Bind, Collect, Flatten, FromStr, Iter, Map,
    StrLike, Wrap,
};
use self::seq::{append, extend, then, Append, Extend, Then};
use error::ParseResult;
use stream::Stream;

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

    fn optional(self) -> Optional<Self>
    where
        Self: Sized,
    {
        optional(self)
    }

    fn map<F, O>(self, f: F) -> Map<Self, F>
    where
        Self: Sized,
        F: Fn(Self::Output) -> O,
    {
        map(self, f)
    }

    fn iter<I>(self) -> Iter<Self, I>
    where
        Self: Sized + Parser<Output = I>,
        I: IntoIterator,
    {
        iter(self)
    }

    fn collect<O>(self) -> Collect<Self, O>
    where
        Self: Sized,
        Self::Output: IntoIterator,
        O: FromIterator<<Self::Output as IntoIterator>::Item>,
    {
        collect(self)
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

    fn then<P>(self, other: P) -> Then<Self, P>
    where
        Self: Sized,
        P: Parser<Stream = Self::Stream, Output = Self::Output>,
    {
        then(self, other)
    }

    fn append<P, O>(self, other: P) -> Append<Self, P>
    where
        Self: Sized + Parser<Output = Vec<O>>,
        P: Parser<Stream = Self::Stream, Output = O>,
    {
        append(self, other)
    }

    fn extend<P, O>(self, other: P) -> Extend<Self, P>
    where
        Self: Sized + Parser<Output = Vec<O>>,
        P: Parser<Stream = Self::Stream, Output = Vec<O>>,
    {
        extend(self, other)
    }

    fn flatten<O>(self) -> Flatten<Self, O>
    where
        Self: Sized + Parser<Output = Vec<Vec<O>>>,
    {
        flatten(self)
    }

    fn wrap(self) -> Wrap<Self>
    where
        Self: Sized,
    {
        wrap(self)
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
    use std::fmt::Debug;

    use super::*;
    use error::Error;
    use stream::{IndexedStream, Position};

    fn vowel<S>() -> impl Parser<Stream = S, Output = char>
    where
        S: Stream<Item = char>,
        S::Position: Position<char>,
    {
        parser(|mut stream: S| match stream.pop() {
            Some(t) => match t {
                'a' | 'e' | 'i' | 'o' | 'u' => stream.ok(t),
                _ => stream.err(Error::unexpected_token(t)),
            },
            None => stream.err(Error::EOF),
        })
    }

    #[test]
    fn test_parser_from_closure() {
        test_parser!(IndexedStream<&str> | vowel(), {
            "a" => (Ok('a'), ("", 1));
            "ooh" => (Ok('o'), ("oh", 1));
        }, {
            "" => (0, vec![Error::EOF]);
            "d" => (1, vec![Error::unexpected_token('d')]);
            "du" => (1, vec![Error::unexpected_token('d')]);
        });
    }

    fn newline<S, O>(mut stream: S) -> ParseResult<S, O>
    where
        S: Stream<Item = O>,
        S::Position: Position<O>,
        O: Copy + PartialEq + Debug + Into<char>,
    {
        match stream.pop().ok_or_else(|| Error::EOF).and_then(|t| {
            if t.into() == '\n' {
                Ok(t)
            } else {
                Err(Error::unexpected_token(t))
            }
        }) {
            Ok(ok) => stream.ok(ok),
            Err(err) => stream.err(err),
        }
    }

    #[test]
    fn test_parser_from_fn() {
        test_parser!(IndexedStream<&[u8]> | parser(newline), {
            "\n".as_bytes() => (Ok(b'\n'), ("".as_bytes(), 1));
            "\nx".as_bytes() => (Ok(b'\n'), ("x".as_bytes(), 1));
        }, {
            "".as_bytes() => (0, vec![Error::EOF]);
            "x\n".as_bytes() => (1, vec![Error::unexpected_token(b'x')]);
        });
    }
}
