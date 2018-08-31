//! Parsers that parse a single token.

use std::fmt::Debug;
use std::marker::PhantomData;

use error::{Error, ParseResult};
use parser::{parser, Parser};
use stream::{Position, Stream};

pub fn any<S, O>() -> fn(S) -> ParseResult<S, O>
where
    S: Stream<Item = O>,
    S::Position: Position<O>,
    O: Copy + PartialEq + Debug,
{
    parser(|mut stream| match stream.pop() {
        Some(t) => stream.ok(t),
        _ => stream.err(Error::EOF),
    })
}

pub struct Token<S: Stream> {
    token: S::Item,
}

impl<S: Stream> Parser for Token<S>
where
    S::Item: PartialEq,
{
    type Stream = S;
    type Output = S::Item;

    fn parse_stream(
        &mut self,
        mut stream: Self::Stream,
    ) -> ParseResult<Self::Stream, Self::Output> {
        match stream.peek() {
            Some(t) if t == self.token => {
                stream.pop();
                stream.ok(t)
            }
            Some(_) => stream.err(Error::expected_token(self.token)),
            _ => stream.err(Error::EOF),
        }
    }
}

pub fn token<S: Stream>(token: S::Item) -> Token<S> {
    Token { token }
}

pub struct Cond<S: Stream, F>
where
    F: Fn(&S::Item) -> bool,
{
    f: F,
    _marker: PhantomData<S>,
}

impl<S: Stream, F> Parser for Cond<S, F>
where
    F: Fn(&S::Item) -> bool,
{
    type Stream = S;
    type Output = S::Item;

    fn parse_stream(
        &mut self,
        mut stream: Self::Stream,
    ) -> ParseResult<Self::Stream, Self::Output> {
        match stream.peek() {
            Some(ref t) if (self.f)(t) => {
                stream.pop();
                stream.ok(*t)
            }
            Some(t) => stream.err(Error::unexpected_token(t)),
            _ => stream.err(Error::EOF),
        }
    }
}

pub fn cond<S: Stream, F>(f: F) -> Cond<S, F>
where
    F: Fn(&S::Item) -> bool,
{
    Cond {
        f,
        _marker: PhantomData,
    }
}

macro_rules! def_char_parser {
    ($(#[$attr:meta])* $name:ident, $f:ident) => {
        $(#[$attr])*
        pub fn $name<S, T>() -> Cond<S, fn(&S::Item) -> bool>
        where
            S: Stream<Item = T>,
            T: Copy + PartialEq + Debug + Into<char>,
            S::Position: Position<T>,
        {
            Cond {
                f: |&c: &T| c.into().$f(),
                _marker: PhantomData,
            }
        }
    };
}

pub mod ascii {
    //! Parsers for ASCII characters.

    use super::*;

    def_char_parser!(
        /// Parses an ASCII letter.
        letter,
        is_ascii_alphabetic
    );
    def_char_parser!(
        /// Parses a digit according to [`std::char::is_ascii_digit`].
        ///
        /// [`std::char::is_ascii_digit`]: https://doc.rust-lang.org/std/primitive.char.html#method.is_ascii_digit
        digit,
        is_ascii_digit
    );
    def_char_parser!(
        /// Parses an ASCII letter or digit.
        alpha_num,
        is_ascii_alphanumeric
    );
    def_char_parser!(
        /// Parses an ASCII whitespace character.
        whitespace,
        is_ascii_whitespace
    );
    def_char_parser!(
        /// Parses an ASCII punctuation character.
        punctuation,
        is_ascii_punctuation
    );

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_letter() {
            test_parser!(&str | letter(), {
                "a" => (Ok('a'), ""),
            }, {
                "1" => (|err| err == &Error::unexpected_token('1')),
            });
        }

        #[test]
        fn test_digit() {
            test_parser!(&str | digit(), {
                "1" => (Ok('1'), ""),
            }, {
                "a" => (|err| err == &Error::unexpected_token('a')),
            });
        }

        #[test]
        fn test_alpha_num() {
            test_parser!(&str | alpha_num(), {
                "a" => (Ok('a'), ""),
                "1" => (Ok('1'), ""),
            }, {
                "." => (|err| err == &Error::unexpected_token('.')),
            });
        }
    }
}

pub mod unicode {
    //! Parsers for unicode characters.

    use super::*;

    def_char_parser!(
        /// Parses a Unicode alphabetic character.
        letter,
        is_alphabetic
    );
    def_char_parser!(
        /// Parses a Unicode numeric character.
        numeric,
        is_numeric
    );
    def_char_parser!(
        /// Parses a Unicode alphabetic or numeric character.
        alpha_num,
        is_alphanumeric
    );
    def_char_parser!(
        /// Parses a Unicode whitespace character.
        whitespace,
        is_whitespace
    );

    #[cfg(test)]
    mod test {
        use super::*;
        use error::{Error::*, Info::*};
        use stream::IndexedStream;

        #[test]
        fn test_letter() {
            test_parser!(&str | letter(), {
                "京34a" => (Ok('京'), "34a"),
                "a京34" => (Ok('a'), "京34"),
            });
            test_parser_errors!(IndexedStream<&str> | letter(), {
                "3京4a" => at 0; (|e| is_match!(&Unexpected(Token('3')) = e)),
            });
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use stream::IndexedStream;

    #[test]
    fn test_any() {
        test_parser!(IndexedStream<&str> | any(), {
            "hello, world." => (Ok('h'), "ello, world.", 1)
        });
    }

    #[test]
    fn test_token() {
        test_parser!(&str | token('c'), {
            "cat" => (Ok('c'), "at"),
        }, {
            "ace" => (|err| err == &Error::expected_token('c')),
        });
    }

    #[test]
    fn test_cond() {
        let mut parser = cond(|&c: &char| c.is_numeric());
        test_parser!(&str | parser, {
            "123abc" => (Ok('1'), "23abc"),
        });
        test_parser_errors!(&str | parser, {
            "abc123" => (|err| err == &Error::unexpected_token('a')),
        });
    }
}
