//! A collection of various parsers and combinators.
//!
//! Defines the `Parser` trait.

#[macro_use]
mod test_utils;

#[macro_use]
pub mod choice;
pub mod function;
pub mod range;
pub mod seq;
pub mod token;
pub mod tuple;

use std::fmt::Display;
use std::iter::FromIterator;
use std::str;

use self::choice::{and, optional, or, xor, And, Optional, Or, Xor};
use self::function::{
    bind, collect, expect, flatten, from_str, iter, map, wrap, Bind, Collect, Expect, Flatten,
    FromStr, Iter, Map, Wrap,
};
use self::seq::{append, extend, then, Append, Extend, Then};
use error::{Errors, Info, ParseResult};
use stream::Stream;
use traits::StrLike;

pub trait Parser {
    type Stream: Stream;
    type Output;

    fn parse_lazy(&mut self, Self::Stream) -> ParseResult<Self::Stream, Self::Output>;

    fn parse_partial(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let mut result = self.parse_lazy(stream);
        if let Err(ref mut errors) = result.0 {
            self.add_expected_error(errors);
        }
        result
    }

    fn parse(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output>
    where
        Self: Sized,
    {
        let backup = stream.backup();
        let mut result = self.parse_lazy(stream);
        if let (Err(ref mut errors), ref mut stream) = result {
            stream.restore(backup);
            self.add_expected_error(errors);
        }
        result
    }

    fn add_expected_error(&self, _: &mut Errors<Self::Stream>) {}

    fn expect<I>(self, i: I) -> Expect<Self>
    where
        Self: Sized,
        I: Into<Info<Self::Stream>>,
    {
        expect(self, i)
    }

    fn optional<O>(self) -> Optional<Self, O>
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
        P: Parser<Stream = Self::Stream>,
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

    fn xor<P>(self, other: P) -> Xor<Self, P>
    where
        Self: Sized,
        P: Parser<Stream = Self::Stream, Output = Self::Output>,
    {
        xor(self, other)
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

impl<'a, S: Stream, O> Parser for dyn FnMut(S) -> ParseResult<S, O> + 'a {
    type Stream = S;
    type Output = O;

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        self(stream)
    }
}

impl<S: Stream, O> Parser for fn(S) -> ParseResult<S, O> {
    type Stream = S;
    type Output = O;

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
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
        S::Position: Position<S::Stream>,
    {
        parser(|mut stream: S| match stream.pop() {
            Some(t) => match t {
                'a' | 'e' | 'i' | 'o' | 'u' => stream.ok(t),
                _ => stream.err(Error::unexpected_token(t)),
            },
            None => stream.err(Error::unexpected_eoi()),
        })
    }

    #[test]
    fn test_parser_from_closure() {
        test_parser!(IndexedStream<&str> | vowel(), {
            "a" => (Ok('a'), ("", 1));
            "ooh" => (Ok('o'), ("oh", 1));
        }, {
            "" => (0, vec![Error::unexpected_eoi()]);
            "d" => (1, vec![Error::unexpected_token('d')]);
            "du" => (1, vec![Error::unexpected_token('d')]);
        });
    }

    fn newline<S>(mut stream: S) -> ParseResult<S, S::Item>
    where
        S: Stream,
        S::Position: Position<S::Stream>,
    {
        match stream
            .pop()
            .ok_or_else(|| Error::unexpected_eoi())
            .and_then(|t| {
                if t == b'\n'.into() {
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
            "".as_bytes() => (0, vec![Error::unexpected_eoi()]);
            "x\n".as_bytes() => (1, vec![Error::unexpected_token(b'x')]);
        });
    }
}
