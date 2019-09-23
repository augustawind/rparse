//! Parsers that parse a single token.

use std::marker::PhantomData;

use error::{Error, Info, ParseResult};
use parser::function::Expect;
use parser::Parser;
use stream::{Position, Stream, StreamItem};

pub struct Any<S: Stream>(PhantomData<S>);

impl<S: Stream> Parser for Any<S> {
    type Stream = S;
    type Output = S::Item;

    fn parse_lazy(&mut self, mut stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match stream.pop() {
            Some(t) => stream.ok(t),
            None => stream.err(Error::unexpected_eoi()),
        }
    }

    fn expected_errors(&self) -> Vec<Error<Self::Stream>> {
        vec![Error::expected("a token")]
    }
}

pub fn any<S: Stream>() -> Any<S> {
    Any(PhantomData)
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

    fn parse_lazy(&mut self, mut stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match stream.peek() {
            Some(t) if t == self.token => {
                stream.pop();
                stream.ok(t)
            }
            result => stream.err(match result {
                Some(t) => Error::unexpected_token(t),
                None => Error::unexpected_eoi(),
            }),
        }
    }

    fn expected_errors(&self) -> Vec<Error<Self::Stream>> {
        vec![Error::expected_token(self.token)]
    }
}

pub fn token<S: Stream>(token: u8) -> Token<S> {
    Token {
        token: S::Item::from(token),
    }
}

pub struct EOI<S, O>(PhantomData<(S, O)>);

impl<S: Stream, O> Parser for EOI<S, O> {
    type Stream = S;
    type Output = O;

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match stream.peek() {
            Some(t) => stream.err(Error::unexpected_token(t)),
            None => stream.noop(),
        }
    }

    fn expected_errors(&self) -> Vec<Error<Self::Stream>> {
        vec![Error::expected(Info::EOI())]
    }
}

pub fn eoi<S: Stream, O>() -> EOI<S, O> {
    EOI(PhantomData)
}

pub struct Satisfy<S: Stream, F>
where
    F: Fn(&S::Item) -> bool,
{
    f: F,
    _marker: PhantomData<S>,
}

impl<S: Stream, F> Parser for Satisfy<S, F>
where
    F: Fn(&S::Item) -> bool,
{
    type Stream = S;
    type Output = S::Item;

    fn parse_lazy(&mut self, mut stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match stream.peek() {
            Some(ref t) if (self.f)(t) => {
                stream.pop();
                stream.ok(*t)
            }
            Some(t) => stream.err(Error::unexpected_token(t)),
            _ => stream.err(Error::unexpected_eoi()),
        }
    }
}

pub fn satisfy<S: Stream, F>(f: F) -> Satisfy<S, F>
where
    F: Fn(&S::Item) -> bool,
{
    Satisfy {
        f,
        _marker: PhantomData,
    }
}

#[cfg(test)]
macro_rules! def_token_parser_tests {
    ($name:ident => $p:expr, $e:expr; valid( $($t_ok:expr),+ ) error( $($t_err:expr),+ ) ) => {
        #[test]
        fn $name() {
            let mut p = $p();
            test_parser!(IndexedStream<&str> => char | p, {
                $(
                    concat!($t_ok) => ok($t_ok, ("", 1)),
                )+
                "" => err(0, vec![Error::unexpected_eoi(), Error::expected($e)]),
                $(
                    concat!($t_err) => err(0, vec![Error::unexpected_token($t_err), Error::expected($e)]),
                )+
            });
            let mut p = $p();
            test_parser!(IndexedStream<&[u8]> => u8 | p, {
                $(
                    concat!($t_ok).as_bytes() => ok($t_ok as u8, ("".as_bytes(), 1)),
                )+
                "".as_bytes() => err(0, vec![Error::unexpected_eoi(), Error::expected($e)]),
                $(
                    concat!($t_err).as_bytes() => err(0, vec![Error::unexpected_token($t_err as u8), Error::expected($e)]),
                )+
            });
        }
    };
}

pub mod ascii {
    //! Parsers for ASCII characters.

    use super::*;

    macro_rules! def_ascii_parser {
        ($(#[$attr:meta])* $name:ident, $f:ident, $expected:expr) => {
            $(#[$attr])*
            pub fn $name<S>() -> Expect<Satisfy<S, fn(&S::Item) -> bool>>
            where
                S: Stream,
                S::Position: Position<S::Stream>,
            {
                let f: fn(&S::Item) -> bool = <S::Item as StreamItem>::$f;
                Satisfy {
                    f,
                    _marker: PhantomData,
                }.expect($expected)
            }
        };
    }

    def_ascii_parser!(
        /// Parses any ASCII character.
        ascii,
        is_ascii,
        "an ascii character"
    );
    def_ascii_parser!(
        /// Parses an ASCII letter.
        letter,
        is_ascii_alphabetic,
        "an ascii letter"
    );
    def_ascii_parser!(
        /// Parses an ASCII letter or digit.
        alpha_num,
        is_ascii_alphanumeric,
        "an ascii letter or digit"
    );
    def_ascii_parser!(
        /// Parses a digit according to [`std::char::is_ascii_digit`].
        ///
        /// [`std::char::is_ascii_digit`]: https://doc.rust-lang.org/std/primitive.char.html#method.is_ascii_digit
        digit,
        is_ascii_digit,
        "an ascii digit"
    );
    def_ascii_parser!(
        /// Parses a hexadecimal digit.
        hexdigit,
        is_ascii_hexdigit,
        "a hexadecimal digit"
    );
    def_ascii_parser!(
        /// Parses an ASCII punctuation character.
        punctuation,
        is_ascii_punctuation,
        "an ascii punctuation character"
    );
    def_ascii_parser!(
        /// Parses an ASCII graphic character.
        graphic,
        is_ascii_graphic,
        "an ascii graphic character"
    );
    def_ascii_parser!(
        /// Parses an ASCII whitespace character.
        whitespace,
        is_ascii_whitespace,
        "an ascii whitespace character"
    );
    def_ascii_parser!(
        /// Parses an ASCII control character.
        control,
        is_ascii_control,
        "an ascii control character"
    );
    def_ascii_parser!(
        /// Parses a lowercase ASCII letter.
        lowercase,
        is_ascii_lowercase,
        "a lowercase ascii letter"
    );
    def_ascii_parser!(
        /// Parses an uppercase ASCII letter.
        uppercase,
        is_ascii_uppercase,
        "an uppercase ascii letter"
    );

    #[cfg(test)]
    mod test {
        use super::*;
        use parser::Parser;
        use stream::IndexedStream;

        // def_token_parser_tests!(test_ascii, ascii, '5', 'û');
        def_token_parser_tests!(test_letter => letter, "an ascii letter";
            valid('z', 'Z')
            error('_', '\n', '9')
        );
        def_token_parser_tests!(test_alpha_num => alpha_num, "an ascii letter or digit";
            valid('3', 'h', 'H')
            error('!', '\n')
        );
        def_token_parser_tests!(test_digit => digit, "an ascii digit";
            valid('1', '0', '9')
            error('a', 'f', '?')
        );
        def_token_parser_tests!(test_hexdigit => hexdigit, "a hexadecimal digit";
            valid('1', 'F', 'b')
            error('H', 'h', '?')
        );
    }
}

pub mod unicode {
    //! Parsers for unicode characters.

    use super::*;

    macro_rules! def_unicode_parser {
        ($(#[$attr:meta])* $name:ident, $f:ident, $expected:expr) => {
            $(#[$attr])*
            pub fn $name<S>() -> Expect<Satisfy<S, fn(&S::Item) -> bool>>
            where
                S: Stream<Item = char>,
                S::Position: Position<S::Stream>,
            {
                let f: fn(&S::Item) -> bool = |&c| <char>::$f(c);
                Satisfy {
                    f,
                    _marker: PhantomData,
                }.expect($expected)
            }
        };
    }

    def_unicode_parser!(
        /// Parses a Unicode alphabetic character.
        letter,
        is_alphabetic,
        "an alphabetic character"
    );
    def_unicode_parser!(
        /// Parses a Unicode alphabetic or numeric character.
        alpha_num,
        is_alphanumeric,
        "an alphabetic or numeric character"
    );
    def_unicode_parser!(
        /// Parses a Unicode numeric character.
        numeric,
        is_numeric,
        "a numeric character"
    );
    def_unicode_parser!(
        /// Parses a Unicode control character.
        control,
        is_control,
        "a control character"
    );
    def_unicode_parser!(
        /// Parses a Unicode whitespace character.
        whitespace,
        is_whitespace,
        "a whitespace character"
    );
    def_unicode_parser!(
        /// Parses a lowercase Unicode alphabetic character.
        lowercase,
        is_lowercase,
        "a lowercase alphabetic character"
    );
    def_unicode_parser!(
        /// Parses an uppercase Unicode alphabetic character.
        uppercase,
        is_uppercase,
        "an uppercase alphabetic character"
    );
}

#[cfg(test)]
mod test {
    use super::*;
    use stream::IndexedStream;

    #[test]
    fn test_any() {
        test_parser!(IndexedStream<&str> => char | any(), {
            "hello, world." => ok('h', ("ello, world.", 1)),
        });
    }

    #[test]
    fn test_token() {
        test_parser!(&str => char | token(b'c'), {
            "cat" => ok('c', "at"),
            "ace" => err(vec![Error::unexpected_token('a'), Error::expected_token('c')]),
        });
    }

    #[test]
    fn test_satisfy() {
        let mut parser = satisfy(|&c: &char| c.is_numeric());
        test_parser!(&str => char | parser, {
            "123abc" => ok('1', "23abc"),
            "abc123" => err(vec![Error::unexpected_token('a')]),
        });
    }
}
