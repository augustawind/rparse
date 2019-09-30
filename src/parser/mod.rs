//! A collection of various parsers and combinators.
//!
//! Defines the `Parser` trait.

#[macro_use]
mod test_utils;

#[macro_use]
pub mod choice;
pub mod function;
pub mod range;
pub mod repeat;
pub mod seq;
pub mod token;
pub mod tuple;

use std::fmt::Display;
use std::iter::FromIterator;
use std::str;

use self::choice::{and, optional, or, skip, with, And, Optional, Or, Skip, With};
use self::function::{
    bind, collect, expect, flatten, from_str, iter, map, wrap, Bind, Collect, Expect, Flatten,
    FromStr, Iter, Map, Wrap,
};
use self::seq::{append, extend, then, Append, Extend, Then};
use self::token::{negate, Negate};
use error::{Error, Errors, ParseResult};
use stream::{RangeStream, Stream};
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

    /// Parses `stream` and reverts `stream` if parsing fails, so that parsing may continue
    /// from its pre-failure state.
    fn try_parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output>
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

    fn by_ref(&mut self) -> &mut Self {
        self
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

    /// Reverses the parse behavior of `self`. Fails if `self` succeeds, succeeds if `self` fails.
    fn negate<S>(self) -> Negate<Self>
    where
        Self: Sized + Parser<Stream = S, Output = S::Item>,
        S: Stream,
    {
        negate(self)
    }

    /// Parses with `self` and then `p`, but discards the result of `p` and returns the result of
    /// `self`. Fails if any of the parsers fail.
    fn skip<P>(self, p: P) -> Skip<Self, P>
    where
        Self: Sized,
    {
        skip(self, p)
    }

    /// Parses with `self` and then `p`, but discards the result of `self` and returns the result
    /// of `p`. Fails if any of the parsers fail.
    fn with<P>(self, p: P) -> With<Self, P>
    where
        Self: Sized,
    {
        with(self, p)
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

    fn opt(self) -> Optional<Self>
    where
        Self: Sized,
    {
        self.optional()
    }

    /// Parses with `self` and if it succeeds with `Some(value)`, apply `f` to the result.
    ///
    /// If `f` needs to be able to fail, use [`Parser::bind`] instead.
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

    /// Parses with `self` and if it succeeds with `Some(value)`, apply `f` to the result.
    ///
    /// Unlike [`map`], `bind` takes a function which returns a [`ParseResult`], so it can be used
    /// to signify failure.
    fn bind<F, O>(self, f: F) -> Bind<Self, F>
    where
        Self: Sized,
        F: Fn(Self::Output, Self::Stream) -> ParseResult<Self::Stream, O>,
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

    /// Parses with `self` and converts the result into a [`String`].
    ///
    /// Can only be used if `Self::Output` is a [`RangeStream`], and will panic if `Self::Output`
    /// is not valid UTF-8.
    fn as_string(self) -> Map<Self, fn(Self::Output) -> String>
    where
        Self: Sized,
        Self::Output: RangeStream,
    {
        map(self, RangeStream::to_string)
    }

    fn s(self) -> Map<Self, fn(Self::Output) -> String>
    where
        Self: Sized,
        Self::Output: RangeStream,
    {
        self.as_string()
    }

    /// Parses with `self` and collects the result into a [`String`].
    ///
    /// Can only be used if `Self::Output` implements [`IntoIterator`] and its items implement
    /// `Into<Char>`.
    fn collect_string(self) -> Map<Self, fn(Self::Output) -> String>
    where
        Self: Sized,
        Self::Output: IntoIterator,
        <<Self as Parser>::Output as IntoIterator>::Item: Into<char>,
    {
        map(self, |s| s.into_iter().map(Into::into).collect())
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
    /// parsers succeed. Both parsers must have the same `Output` type.
    ///
    /// ```
    /// # use rparse::Parser;
    /// # use rparse::parser::range::range;
    /// # use rparse::parser::token::ascii::*;
    /// let mut p = range("Hello, ").then(range("World!"));
    /// assert_eq!(p.parse("Hello, World!"), Ok((Some(vec!["Hello, ", "World!"]), "")));
    /// ```
    fn then<I, P>(self, p: P) -> Then<I, Self, P>
    where
        Self: Sized,
        P: Parser<Stream = Self::Stream, Output = Self::Output>,
        I: std::iter::FromIterator<Self::Output>,
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
    /// # use rparse::parser::repeat::many;
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
    /// # use rparse::parser::token::ascii::*;
    /// # use rparse::parser::token::token;
    /// # fn main() {
    /// let mut p = seq![
    ///     token(b'\x27'),
    ///     token(b'['),
    ///     digit(),
    ///     choice![token(b'A'), token(b'B'), token(b'C'), token(b'D')],
    /// ];
    /// assert_eq!(p.parse("\x27[5B"), Ok((Some(vec!['\x27', '[', '5', 'B']), "")));
    /// # }
    fn append<O, P>(self, p: P) -> Append<Self, P>
    where
        Self: Sized + Parser<Output = Vec<O>>,
        P: Parser<Stream = Self::Stream, Output = O>,
    {
        append(self, p)
    }

    fn extend<O, P>(self, p: P) -> Extend<Self, P>
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

    fn wrap<O>(self) -> Wrap<O, Self>
    where
        Self: Sized,
        O: std::iter::Extend<Self::Output> + Default,
    {
        wrap(self)
    }

    fn w<O>(self) -> Wrap<O, Self>
    where
        Self: Sized,
        O: std::iter::Extend<Self::Output> + Default,
    {
        wrap(self)
    }
}

impl<P: Parser> Parser for &mut P {
    type Stream = P::Stream;
    type Output = P::Output;

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        (**self).parse_lazy(stream)
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
            None => stream.err(Error::eoi()),
        })
    }

    #[test]
    fn test_parser_from_closure() {
        test_parser!(IndexedStream<&str> => char | vowel(), {
            "a" => ok('a', ("", 1)),
            "ooh" => ok('o', ("oh", 1)),
            "" => err(0, vec![Error::eoi()]),
            "d" => err(1, vec![Error::unexpected_token('d')]),
            "du" => err(1, vec![Error::unexpected_token('d')]),
        });
    }

    fn newline<S>(mut stream: S) -> ParseResult<S, S::Item>
    where
        S: Stream,
        S::Position: Position<S::Stream>,
    {
        match stream.pop().ok_or_else(|| Error::eoi()).and_then(|t| {
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
            "".as_bytes() => err(0, vec![Error::eoi()]),
            "x\n".as_bytes() => err(1, vec![Error::unexpected_token(b'x')]),
        });
    }
}
