//! Parsers that parse a single token.

use std::marker::PhantomData;

use error::{Error, ParseResult};
use parser::{parser, Parser};
use stream::{Position, Stream, StreamItem, StreamRange};

// TODO: refactor this to use a struct impl (e.g. `struct Any<S, O>`)
pub fn any<S>() -> impl Parser<Stream = S, Output = S::Item>
where
    S: Stream,
    S::Position: Position<S::Stream>,
{
    parser(|mut stream: S| match stream.pop() {
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
            result => {
                let mut errors = stream.empty_err();
                errors.add_error(match result {
                    Some(t) => Error::unexpected_token(t),
                    None => Error::EOF,
                });
                errors.add_error(Error::expected_token(self.token));
                stream.errs(errors)
            }
        }
    }
}

pub fn token<S: Stream>(token: u8) -> Token<S> {
    Token {
        token: S::Range::item_from_byte(token),
    }
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

#[cfg(test)]
macro_rules! def_token_parser_tests {
    ($name:ident, $p:expr, [ $($t_ok:expr),+ ] , [ $($t_err:expr),+ ] ) => {
        #[test]
        fn $name() {
            $(
                test_parser!(IndexedStream<&str> => char | $p(), {
                    concat!($t_ok) => ok(Ok($t_ok), ("", 1)),
                });
                test_parser!(IndexedStream<&[u8]> => u8 | $p(), {
                    concat!($t_ok).as_bytes() => ok(Ok($t_ok as u8), ("".as_bytes(), 1)),
                });
            )+
            $(
                test_parser!(IndexedStream<&str> => char | $p(), {
                    "" => err(0, vec![Error::EOF]),
                    concat!($t_err) => err(0, vec![Error::unexpected_token($t_err)]),
                });
                test_parser!(IndexedStream<&[u8]> => u8 | $p(), {
                    "".as_bytes() => err(0, vec![Error::EOF]),
                    concat!($t_err).as_bytes() => err(0, vec![Error::unexpected_token($t_err as u8)]),
                });
            )+
        }
    };
}

pub mod ascii {
    //! Parsers for ASCII characters.

    use super::*;

    macro_rules! def_ascii_parser {
        ($(#[$attr:meta])* $name:ident, $f:ident) => {
            $(#[$attr])*
            pub fn $name<S>() -> Cond<S, fn(&S::Item) -> bool>
            where
                S: Stream,
                S::Position: Position<S::Stream>,
            {
                Cond {
                    f: <S::Item as StreamItem>::$f,
                    _marker: PhantomData,
                }
            }
        };
    }

    def_ascii_parser!(
        /// Parses any ASCII character.
        ascii,
        is_ascii
    );
    def_ascii_parser!(
        /// Parses an ASCII letter.
        letter,
        is_ascii_alphabetic
    );
    def_ascii_parser!(
        /// Parses an ASCII letter or digit.
        alpha_num,
        is_ascii_alphanumeric
    );
    def_ascii_parser!(
        /// Parses a digit according to [`std::char::is_ascii_digit`].
        ///
        /// [`std::char::is_ascii_digit`]: https://doc.rust-lang.org/std/primitive.char.html#method.is_ascii_digit
        digit,
        is_ascii_digit
    );
    def_ascii_parser!(
        /// Parses a hexadecimal digit.
        hexdigit,
        is_ascii_hexdigit
    );
    def_ascii_parser!(
        /// Parses an ASCII punctuation character.
        punctuation,
        is_ascii_punctuation
    );
    def_ascii_parser!(
        /// Parses an ASCII graphic character.
        graphic,
        is_ascii_graphic
    );
    def_ascii_parser!(
        /// Parses an ASCII whitespace character.
        whitespace,
        is_ascii_whitespace
    );
    def_ascii_parser!(
        /// Parses an ASCII control character.
        control,
        is_ascii_control
    );
    def_ascii_parser!(
        /// Parses a lowercase ASCII letter.
        lowercase,
        is_ascii_lowercase
    );
    def_ascii_parser!(
        /// Parses an uppercase ASCII letter.
        uppercase,
        is_ascii_uppercase
    );

    #[cfg(test)]
    mod test {
        use super::*;
        use parser::Parser;
        use stream::IndexedStream;

        // def_token_parser_tests!(test_ascii, ascii, '5', 'û');
        def_token_parser_tests!(test_letter, letter, ['z', 'Z'], ['_', '\n', '9']);
        def_token_parser_tests!(test_alpha_num, alpha_num, ['3', 'h', 'H'], ['!', '\n']);
        def_token_parser_tests!(test_digit, digit, ['1', '0', '9'], ['a', 'f', '?']);
    }
}

pub mod unicode {
    //! Parsers for unicode characters.

    use super::*;

    macro_rules! def_unicode_parser {
        ($(#[$attr:meta])* $name:ident, $f:ident) => {
            $(#[$attr])*
            pub fn $name<S>() -> Cond<S, fn(&S::Item) -> bool>
            where
                S: Stream<Item = char>,
                S::Position: Position<S::Stream>,
            {
                Cond {
                    f: |&c| <char>::$f(c),
                    _marker: PhantomData,
                }
            }
        };
    }

    def_unicode_parser!(
        /// Parses a Unicode alphabetic character.
        letter,
        is_alphabetic
    );
    def_unicode_parser!(
        /// Parses a Unicode alphabetic or numeric character.
        alpha_num,
        is_alphanumeric
    );
    def_unicode_parser!(
        /// Parses a Unicode numeric character.
        numeric,
        is_numeric
    );
    def_unicode_parser!(
        /// Parses a Unicode control character.
        control,
        is_control
    );
    def_unicode_parser!(
        /// Parses a Unicode whitespace character.
        whitespace,
        is_whitespace
    );
    def_unicode_parser!(
        /// Parses a lowercase Unicode alphabetic character.
        lowercase,
        is_lowercase
    );
    def_unicode_parser!(
        /// Parses an uppercase Unicode alphabetic character.
        uppercase,
        is_uppercase
    );

    #[cfg(test)]
    mod test {
        use super::*;
        use error::{Error::*, Info::*};
        use stream::IndexedStream;

        #[test]
        fn test_letter() {
            test_parser!(IndexedStream<&str> | letter(), {
                "京34a" => (Ok('京'), ("34a", 1));
                "a京34" => (Ok('a'), ("京34", 1));
            }, {
                "3京4a" => (0, vec![Unexpected(Token('3'))]);
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
            "hello, world." => (Ok('h'), ("ello, world.", 1));
        });
    }

    #[test]
    fn test_token() {
        test_parser!(&str | token(b'c'), {
            "cat" => (Ok('c'), "at");
        }, {
            "ace" => vec![Error::unexpected_token('a'), Error::expected_token('c')];
        });
    }

    #[test]
    fn test_cond() {
        let mut parser = cond(|&c: &char| c.is_numeric());
        test_parser!(&str | parser, {
            "123abc" => (Ok('1'), "23abc");
        }, {
            "abc123" => vec![Error::unexpected_token('a')];
        });
    }
}
