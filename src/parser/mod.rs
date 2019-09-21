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

use self::choice::{and, optional, or, required, skip, And, Optional, Or, Required, Skip};
use self::function::{
    bind, collect, expect, flatten, from_str, iter, map, wrap, Bind, Collect, Expect, Flatten,
    FromStr, Iter, Map, Wrap,
};
use self::seq::{append, extend, then, Append, Extend, Then};
use error::{Error, Errors, Info, ParseResult};
use stream::{Stream, StreamRange};
use traits::StrLike;

pub trait Parser {
    type Stream: Stream;
    type Output;

    fn parse_lazy(&mut self, Self::Stream) -> ParseResult<Self::Stream, Self::Output>;

    fn parse_partial(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let mut result = self.parse_lazy(stream);
        if let Err((ref mut errors, _)) = result {
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
        if let Err((ref mut errors, ref mut stream)) = result {
            stream.restore(backup);
            self.add_expected_error(errors);
        }
        result
    }

    fn must_parse(
        &mut self,
        stream: Self::Stream,
    ) -> Result<(Self::Output, Self::Stream), (Errors<Self::Stream>, Self::Stream)>
    where
        Self: Sized,
    {
        let backup = stream.backup();
        match self.parse_lazy(stream) {
            Ok((Some(output), stream)) => Ok((output, stream)),
            Ok((None, mut stream)) => {
                &mut stream.restore(backup);
                let mut errors = stream.empty_err();
                self.add_expected_error(&mut errors);
                Err((errors, stream))
            }
            Err((mut errors, mut stream)) => {
                &mut stream.restore(backup);
                self.add_expected_error(&mut errors);
                Err((errors, stream))
            }
        }
    }

    fn expected_error(&self) -> Option<Error<Self::Stream>> {
        None
    }

    fn add_expected_error(&self, errors: &mut Errors<Self::Stream>) {
        if let Some(error) = self.expected_error() {
            errors.add_error(error);
        }
    }

    fn expect<I>(self, i: I) -> Expect<Self>
    where
        Self: Sized,
        I: Into<Info<Self::Stream>>,
    {
        expect(self, i)
    }

    fn skip(self) -> Skip<Self>
    where
        Self: Sized,
    {
        skip(self)
    }

    fn optional(self) -> Optional<Self>
    where
        Self: Sized,
    {
        optional(self)
    }

    fn required(self) -> Required<Self>
    where
        Self: Sized,
    {
        required(self)
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
        F: Fn(Option<Self::Output>, Self::Stream) -> O,
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

    fn as_string(self) -> Map<Self, fn(Self::Output) -> String>
    where
        Self: Sized,
        Self::Output: StreamRange,
    {
        map(self, StreamRange::to_string)
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
        test_parser!(IndexedStream<&str> => char | vowel(), {
            "a" => ok('a', ("", 1)),
            "ooh" => ok('o', ("oh", 1)),
            "" => err(0, vec![Error::unexpected_eoi()]),
            "d" => err(1, vec![Error::unexpected_token('d')]),
            "du" => err(1, vec![Error::unexpected_token('d')]),
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
        test_parser!(IndexedStream<&[u8]> => u8 | parser(newline), {
            "\n".as_bytes() => ok(b'\n', ("".as_bytes(), 1)),
            "\nx".as_bytes() => ok(b'\n', ("x".as_bytes(), 1)),
            "".as_bytes() => err(0, vec![Error::unexpected_eoi()]),
            "x\n".as_bytes() => err(1, vec![Error::unexpected_token(b'x')]),
        });
    }
}
