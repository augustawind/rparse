//! Parsers that parse a single token.

use std::fmt::Debug;
use std::marker::PhantomData;

use error::{Error, ParseResult};
use input::Input;
use parser::{parser, Parser};

pub fn any<I, O>() -> fn(I) -> ParseResult<I, O>
where
    I: Input<Item = O>,
    O: Copy + PartialEq + Debug,
{
    parser(|mut input| match input.pop() {
        Some(t) => input.ok(t),
        _ => input.err(Error::EOF),
    })
}

#[derive(Copy, Clone)]
pub struct Token<I: Input> {
    token: I::Item,
}

impl<I: Input> Parser for Token<I>
where
    I::Item: PartialEq,
{
    type Input = I;
    type Output = I::Item;

    fn parse_input(&mut self, mut input: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        match input.peek() {
            Some(t) if t == self.token => {
                input.pop();
                input.ok(t)
            }
            Some(_) => input.err(Error::expected_token(self.token)),
            _ => input.err(Error::EOF),
        }
    }
}

pub fn token<I: Input>(token: I::Item) -> Token<I> {
    Token { token }
}

pub struct Cond<I: Input, F>
where
    F: Fn(&I::Item) -> bool,
{
    f: F,
    _marker: PhantomData<I>,
}

impl<I: Input, F> Parser for Cond<I, F>
where
    F: Fn(&I::Item) -> bool,
{
    type Input = I;
    type Output = I::Item;

    fn parse_input(&mut self, mut input: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        match input.peek() {
            Some(ref t) if (self.f)(t) => {
                input.pop();
                input.ok(*t)
            }
            Some(t) => input.err(Error::unexpected_token(t)),
            _ => input.err(Error::EOF),
        }
    }
}

pub fn cond<I: Input, F>(f: F) -> Cond<I, F>
where
    F: Fn(&I::Item) -> bool,
{
    Cond {
        f,
        _marker: PhantomData,
    }
}

macro_rules! def_char_parser {
    ($(#[$attr:meta])* $name:ident, $f:ident) => {
        $(#[$attr])*
        pub fn $name<I, T>() -> Cond<I, fn(&I::Item) -> bool>
        where
            I: Input<Item = T>,
            T: Copy + PartialEq + Debug + Into<char>,
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
                "1" => (Err(Error::unexpected_token('1')), "1"),
            });
        }

        #[test]
        fn test_digit() {
            test_parser!(&str | digit(), {
                "1" => (Ok('1'), ""),
                "a" => (Err(Error::unexpected_token('a')), "a"),
            });
        }

        #[test]
        fn test_alpha_num() {
            test_parser!(&str | alpha_num(), {
                "a" => (Ok('a'), ""),
                "1" => (Ok('1'), ""),
                "." => (Err(Error::unexpected_token('.')), "."),
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

        #[test]
        fn test_letter() {
            assert_eq!(letter().parse("京34a"), (Ok('京'), "34a"));
            assert_eq!(letter().parse("a京34"), (Ok('a'), "京34"));
            assert_parse_err!(letter().parse("3京4a"), "3京4a");
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use input::IndexedInput;

    #[test]
    fn test_any() {
        test_parser!(IndexedInput<&str> | any(), {
            "hello, world." => (Ok('h'), "ello, world.", 1)
        });
    }

    #[test]
    fn test_token() {
        test_parser!(&str | token('c'), {
            "cat" => (Ok('c'), "at"),
            "ace" => (Err(Error::expected_token('c')), "ace"),
        });
    }

    #[test]
    fn test_cond() {
        test_parser!(&str | cond(|&c: &char| c.is_numeric()), {
            "123abc" => (Ok('1'), "23abc"),
            "abc123" => (Err(Error::unexpected_token('a')), "abc123"),
        });
    }
}
