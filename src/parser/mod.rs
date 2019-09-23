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
use error::{Error, Errors, ParseResult};
use stream::{Stream, StreamRange};
use traits::StrLike;

pub trait Parser {
    type Stream: Stream;
    type Output;

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        self.parse_partial(stream)
    }

    fn parse_partial(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let mut result = self.parse_lazy(stream);
        if let Err((ref mut errors, _)) = result {
            self.add_expected_error(errors);
        }
        result
    }

    fn try_parse(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output>
    where
        Self: Sized,
    {
        let backup = stream.backup();
        let mut result = self.parse_lazy(stream);
        if let Err((_, ref mut stream)) = result {
            stream.restore(backup);
        }
        result
    }

    fn parse(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output>
    where
        Self: Sized,
    {
        let backup = stream.backup();
        let mut result = self.parse_partial(stream);
        if let Err((_, ref mut stream)) = result {
            stream.restore(backup);
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
                let mut errors = stream.new_errors();
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

    fn expected_errors(&self) -> Vec<Error<Self::Stream>> {
        Vec::new()
    }

    fn add_expected_error(&self, errors: &mut Errors<Self::Stream>) {
        errors.add_errors(self.expected_errors());
    }

    /// Wrap `self` with a custom error. If parsing fails, the parser's expected errors will be
    /// replaced with [`Expected(error)`].
    ///
    /// [`Expected(error)`]: Error::Expected
    fn expect<E>(self, error: E) -> Expect<Self>
    where
        Self: Sized,
        E: Into<Error<Self::Stream>>,
    {
        expect(self, error)
    }

    /// Equivalent to [`skip(self)`].
    ///
    /// [`skip(self)`]: skip
    fn skip<O>(self) -> Skip<Self, O>
    where
        Self: Sized,
    {
        skip(self)
    }

    /// Equivalent to [`optional(self)`].
    ///
    /// [`optional(self)`]: optional
    fn optional(self) -> Optional<Self>
    where
        Self: Sized,
    {
        optional(self)
    }

    /// Equivalent to [`required(self)`].
    ///
    /// [`required(self)`]: required
    fn required(self) -> Required<Self>
    where
        Self: Sized,
    {
        required(self)
    }

    /// Parses with `self` and if it succeeds with `Some(value)`, apply `f` to the result.
    fn map<F, O>(self, f: F) -> Map<Self, F>
    where
        Self: Sized,
        F: Fn(Self::Output) -> O,
    {
        map(self, f)
    }

    /// Parses with `self` and transforms the result [into an iterator].
    ///
    /// [into an iterator]: IntoIterator::into_iter
    fn iter<I>(self) -> Iter<Self, I>
    where
        Self: Sized + Parser<Output = I>,
        I: IntoIterator,
    {
        iter(self)
    }

    /// Parses with `self` and transforms the result [into a new collection].
    ///
    /// [into a new collection]: std::iter::Iterator::collect
    fn collect<O>(self) -> Collect<Self, O>
    where
        Self: Sized,
        Self::Output: IntoIterator,
        O: FromIterator<<Self::Output as IntoIterator>::Item>,
    {
        collect(self)
    }

    /// Parses with `self` and applies `f` to the result.
    ///
    /// Unlike [`map`], `f` returns a [`ParseResult`] and is called on _any_ successful
    /// parse, even it returned [`None`].
    fn bind<F, O>(self, f: F) -> Bind<Self, F>
    where
        Self: Sized,
        F: Fn(Option<Self::Output>, Self::Stream) -> ParseResult<Self::Stream, O>,
    {
        bind(self, f)
    }

    /// Parses with `self` and transforms the result using [`str::FromStr`].
    fn from_str<O>(self) -> FromStr<Self, O>
    where
        Self: Sized,
        Self::Output: StrLike,
        O: str::FromStr,
        O::Err: Display,
    {
        from_str(self)
    }

    /// Parses with `self` and transforms the result into a [`String`].
    fn as_string(self) -> Map<Self, fn(Self::Output) -> String>
    where
        Self: Sized,
        Self::Output: StreamRange,
    {
        map(self, StreamRange::to_string)
    }

    /// Parses with `self` followed by `p`. Succeeds if both parsers succeed, otherwise fails.
    /// Returns the result of `p` on success.
    fn and<P>(self, p: P) -> And<Self, P>
    where
        Self: Sized,
        P: Parser<Stream = Self::Stream>,
    {
        and(self, p)
    }

    /// Attempts to parse with `self`. If it fails, it attempts to parse the same input with `p`.
    /// Returns the first successful result, or fails if both parsers fail.
    fn or<P>(self, p: P) -> Or<Self, P>
    where
        Self: Sized,
        P: Parser<Stream = Self::Stream, Output = Self::Output>,
    {
        or(self, p)
    }

    /// Parses with `self` followed by `p`, returning the results in a [`Vec`]. Succeeds if both
    /// parsers return `Some(value)`, otherwise fails. Both parsers must have the same `Output`
    /// type.
    ///
    /// ```
    /// # use rparse::Parser;
    /// # use rparse::parser::range::range;
    /// # use rparse::parser::token::ascii::*;
    /// let mut p = range("Hello, ").then(range("World!"));
    /// assert_eq!(p.parse("Hello, World!"), Ok((Some(vec!["Hello, ", "World!"]), "")));
    /// ```
    fn then<P>(self, p: P) -> Then<Self, P>
    where
        Self: Sized,
        P: Parser<Stream = Self::Stream, Output = Self::Output>,
    {
        then(self, p)
    }

    /// Parses with `self` followed by `p`, returning the results in a [`Vec`]. Succeeds as long
    /// as `self` returns `Some(value)`, otherwise fails. If `p` returns `Some(value)` it's
    /// appended to the output of `self`, otherwise it's ignored and only `self`'s output is
    /// returned.
    ///
    /// `self` must return `Vec<O>`, where `O` is `p`'s output type. This can be used for chaining
    /// item parsers to a composite parser:
    ///
    /// ```
    /// # use rparse::Parser;
    /// # use rparse::parser::seq::many;
    /// # use rparse::parser::token::ascii::*;
    /// let mut p = many(whitespace()).append(letter()).append(digit());
    /// assert_eq!(p.parse("\n\tT2!"), Ok((Some(vec!['\n', '\t', 'T', '2']), "!")));
    /// ```
    ///
    /// If `self` doesn't return a `Vec`, you can use [`Parser::then`] to start the chain:
    ///
    /// ```
    /// # #[macro_use] extern crate rparse;
    /// # use rparse::Parser;
    /// # use rparse::parser::token::ascii::*;
    /// # use rparse::parser::token::token;
    /// let mut p = token(b'\x27')
    ///     .then(token(b'['))
    ///     .append(digit())
    ///     .append(choice![token(b'A'), token(b'B'), token(b'C'), token(b'D')]);
    /// assert_eq!(p.parse("\x27[5B"), Ok((Some(vec!['\x27', '[', '5', 'B']), "")));
    /// ```
    ///
    /// Another way to chain parsers like this is with the [`seq!`] macro. This approach
    /// can be clearer and simpler than using the `then`/`append` method:
    ///
    /// ```
    /// # #[macro_use] extern crate rparse;
    /// # use rparse::Parser;
    /// # use rparse::parser::range::range;
    /// # fn main() {
    /// let mut p = seq![
    ///     range("HTTP"),
    ///     range("/").skip(),
    ///     range("1.1").or(range("2")),
    /// ];
    /// assert_eq!(p.parse("HTTP/2\r"), Ok((Some(vec!["HTTP", "2"]), "\r")));
    /// # }
    fn append<P, O>(self, p: P) -> Append<Self, P>
    where
        Self: Sized + Parser<Output = Vec<O>>,
        P: Parser<Stream = Self::Stream, Output = O>,
    {
        append(self, p)
    }

    fn extend<P, O>(self, p: P) -> Extend<Self, P>
    where
        Self: Sized + Parser<Output = Vec<O>>,
        P: Parser<Stream = Self::Stream, Output = Vec<O>>,
    {
        extend(self, p)
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
