//! A collection of various parsers and combinators.
//!
//! Defines the `Parser` trait.

#[macro_use]
mod test_utils;

#[macro_use]
pub mod choice;
pub mod function;
pub mod item;
pub mod range;
pub mod repeat;
pub mod seq;
pub mod tuple;

use std::fmt::Display;
use std::iter::FromIterator;
use std::str;

use self::choice::{and, optional, or, skip, with, And, Optional, Or, Skip, With};
use self::function::{
    bind, collect, expect, flatten, from_str, iter, map, wrap, Bind, Collect, Expect, Flatten,
    FromStr, Iter, Map, Wrap,
};
use self::item::{negate, Negate};
use self::seq::{append, extend, then, Append, Extend, Then};
use error::{Error, Expected, ParseResult};
use stream::{RangeStream, Stream};
use traits::StrLike;

pub trait Parser {
    type Stream: Stream;
    type Output;

    /// Parses `stream`. Doesn't revert `stream` or add expected errors if parsing fails.
    ///
    /// At minimum, implementors must implement this function or [`Parser::parse_partial`] since
    /// their default definitions each reference each other. Where possible, it is preferred to
    /// implement `parse_lazy` so that callers can postpone adding expected errors until parsing
    /// is complete.
    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        self.parse_partial(stream)
    }

    /// Parses `stream` and adds expected errors if parsing fails.
    ///
    /// At minimum, implementors must implement this function or [`Parser::parse_partial`] since
    /// their default definitions each reference each other.
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

    /// Parses `stream`. If parsing fails, reverts `stream` and adds expected errors.
    ///
    /// This is typically used to initiate parsing from the top-level parser. It is provided as the
    /// main entrypoint into parsing.
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

    /// Like [`Parser::parse`], but fails if the output is [`Option::None`] and returns the output
    /// not wrapped in an [`Option`].
    fn must_parse(
        &mut self,
        stream: Self::Stream,
    ) -> Result<(Self::Output, Self::Stream), (Error<Self::Stream>, Self::Stream)>
    where
        Self: Sized,
    {
        let backup = stream.backup();
        match self.parse_lazy(stream) {
            Ok((Some(output), stream)) => Ok((output, stream)),
            Ok((None, mut stream)) => {
                &mut stream.restore(backup);
                let mut error = stream.new_error();
                self.add_expected_error(&mut error);
                Err((error, stream))
            }
            Err((mut error, mut stream)) => {
                &mut stream.restore(backup);
                self.add_expected_error(&mut error);
                Err((error, stream))
            }
        }
    }

    /// Returns a [`Vec`] of the expected errors that should be added if parsing fails.
    ///
    /// By default this returns an empty [`Vec`].
    fn expected_error(&self) -> Option<Expected<Self::Stream>> {
        None
    }

    /// Adds this parsers expected errors to `errors`.
    ///
    /// In most cases, this should be left as the default and [`Parser::expected_errors`]
    /// should be defined instead.
    fn add_expected_error(&self, error: &mut Error<Self::Stream>) {
        if let Some(expected) = self.expected_error() {
            error.add_expected(expected);
        }
    }

    /// Returns a mutable referance to this parser.
    ///
    /// Equivalent to `&mut p`; `by_ref` removes the need for wrapping parenthesis in some cases.
    fn by_ref(&mut self) -> &mut Self {
        self
    }

    /// Wrap `self` with a custom error. If parsing fails, the parser's expected errors will be
    /// replaced with [`Expected(error)`].
    ///
    /// [`Expected(error)`]: Error::Expected
    fn expect<E>(self, expected: E) -> Expect<Self>
    where
        Self: Sized,
        E: Into<Expected<Self::Stream>>,
    {
        expect(self, expected)
    }

    /// Reverses the parse behavior of `self`. Fails if `self` succeeds, succeeds if `self` fails.
    ///
    /// Only works for item parsers.
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
    /// # use rparse::parser::item::ascii::*;
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
    /// # use rparse::parser::item::ascii::*;
    /// let mut p = many(whitespace()).append(letter()).append(digit());
    /// assert_eq!(p.parse("\n\tT2!"), Ok((Some(vec!['\n', '\t', 'T', '2']), "!")));
    /// ```
    ///
    /// If `self` doesn't return a `Vec`, you can use [`Parser::then`] to start the chain:
    ///
    /// ```
    /// # #[macro_use] extern crate rparse;
    /// # use rparse::Parser;
    /// # use rparse::parser::item::ascii::*;
    /// # use rparse::parser::item::item;
    /// let mut p = item(b'\x27')
    ///     .then(item(b'['))
    ///     .append(digit())
    ///     .append(choice![item(b'A'), item(b'B'), item(b'C'), item(b'D')]);
    /// assert_eq!(p.parse("\x27[5B"), Ok((Some(vec!['\x27', '[', '5', 'B']), "")));
    /// ```
    ///
    /// Another way to chain parsers like this is with the [`seq!`] macro. This approach
    /// can be clearer and simpler than using the `then`/`append` method:
    ///
    /// ```
    /// # #[macro_use] extern crate rparse;
    /// # use rparse::Parser;
    /// # use rparse::parser::item::ascii::*;
    /// # use rparse::parser::item::item;
    /// # fn main() {
    /// let mut p = seq![
    ///     item(b'\x27'),
    ///     item(b'['),
    ///     digit(),
    ///     choice![item(b'A'), item(b'B'), item(b'C'), item(b'D')],
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

    fn parse_partial(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        (**self).parse_partial(stream)
    }

    fn try_parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output>
    where
        Self: Sized,
    {
        (**self).try_parse_lazy(stream)
    }

    fn parse(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output>
    where
        Self: Sized,
    {
        (**self).parse(stream)
    }

    fn must_parse(
        &mut self,
        stream: Self::Stream,
    ) -> Result<(Self::Output, Self::Stream), (Error<Self::Stream>, Self::Stream)>
    where
        Self: Sized,
    {
        (**self).must_parse(stream)
    }

    fn expected_error(&self) -> Option<Expected<Self::Stream>> {
        (**self).expected_error()
    }

    fn add_expected_error(&self, error: &mut Error<Self::Stream>) {
        (**self).add_expected_error(error);
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
        parser(|mut stream: S| {
            let start = stream.position().clone();
            match stream.pop() {
                Some(t) => match t {
                    'a' | 'e' | 'i' | 'o' | 'u' => stream.ok(t),
                    _ => stream.err_at(start, Error::item(t)),
                },
                None => stream.err_at(start, Error::eoi()),
            }
        })
    }

    #[test]
    fn test_parser_from_closure() {
        test_parser!(IndexedStream<&str> => char | vowel(), {
            "a" => ok('a', ("", 1)),
            "ooh" => ok('o', ("oh", 1)),
            "" => err(Error::eoi().at(0)),
            "d" => err(Error::item('d').at(0)),
            "du" => err(Error::item('d').at(0)),
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
                Err(Error::item(t))
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
            "".as_bytes() => err(Error::eoi().at(0)),
            "x\n".as_bytes() => err(Error::item(b'x').at(1)),
        });
    }
}
