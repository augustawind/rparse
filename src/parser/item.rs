//! Parsers that parse a single token.

use std::marker::PhantomData;

use error::{Error, Expected, Info, ParseResult};
use parser::function::Expect;
use parser::Parser;
use stream::{Position, Stream, StreamItem};

pub struct Any<S: Stream>(PhantomData<S>);

impl<S: Stream> Parser for Any<S> {
    type Stream = S;
    type Output = S::Item;

    fn parse_lazy(&mut self, mut stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let start = stream.position().clone();
        match stream.pop() {
            Some(t) => stream.ok(t),
            None => stream.err_at(start, Error::eoi()),
        }
    }

    fn expected_error(&self) -> Option<Expected<Self::Stream>> {
        Some("a token".into())
    }
}

pub fn any<S: Stream>() -> Any<S> {
    Any(PhantomData)
}

pub struct Item<S: Stream> {
    item: S::Item,
}

impl<S: Stream> Parser for Item<S>
where
    S::Item: PartialEq,
{
    type Stream = S;
    type Output = S::Item;

    fn parse_lazy(&mut self, mut stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match stream.peek() {
            Some(item) if item == self.item => {
                stream.pop();
                stream.ok(item)
            }
            result => stream.err(match result {
                Some(item) => Error::item(item),
                None => Error::eoi(),
            }),
        }
    }

    fn expected_error(&self) -> Option<Expected<Self::Stream>> {
        Some(Info::Item(self.item).into())
    }
}

pub fn item<S: Stream>(item: u8) -> Item<S> {
    Item {
        item: S::Item::from(item),
    }
}

pub struct EOI<S, O>(PhantomData<(S, O)>);

impl<S: Stream, O> Parser for EOI<S, O> {
    type Stream = S;
    type Output = O;

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match stream.peek() {
            Some(t) => stream.err(Error::item(t)),
            None => stream.noop(),
        }
    }

    fn expected_error(&self) -> Option<Expected<Self::Stream>> {
        Some(Info::EOI.into())
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
            Some(t) => stream.err(Error::item(t)),
            _ => stream.err(Error::eoi()),
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

pub struct Negate<P: Parser> {
    p: P,
}
impl<S, P> Parser for Negate<P>
where
    S: Stream,
    P: Parser<Stream = S, Output = S::Item>,
{
    type Stream = S;
    type Output = S::Item;

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let initial = stream.backup();
        let mut stream = match self.p.parse_lazy(stream) {
            Ok((Some(t), stream)) => {
                return stream.err_at(initial.position().clone(), Error::item(t))
            }
            Ok((None, stream)) => stream,
            Err((_, stream)) => stream,
        };
        stream.restore(initial);
        match stream.pop() {
            Some(t) => stream.ok(t),
            None => stream.err(Error::eoi()),
        }
    }
}

pub fn negate<S, P>(p: P) -> Negate<P>
where
    S: Stream,
    P: Parser<Stream = S, Output = S::Item>,
{
    Negate { p }
}

pub struct OneOf<S: Stream> {
    items: Vec<S::Item>,
}

impl<S: Stream> Parser for OneOf<S> {
    type Stream = S;
    type Output = S::Item;

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        satisfy(move |item: &S::Item| self.items.iter().any(|i| i == item)).parse_lazy(stream)
    }

    fn expected_error(&self) -> Option<Expected<Self::Stream>> {
        Some(Expected::OneOf(
            self.items
                .iter()
                .map(|&item| Info::Item(item).into())
                .collect(),
        ))
    }
}

pub fn one_of<'a, S: Stream>(items: &'a [u8]) -> OneOf<S> {
    OneOf {
        items: items.into_iter().map(|&b| b.into()).collect(),
    }
}

pub struct NoneOf<S: Stream> {
    items: Vec<S::Item>,
}

impl<S: Stream> Parser for NoneOf<S> {
    type Stream = S;
    type Output = S::Item;

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        satisfy(move |item: &S::Item| self.items.iter().all(|i| i != item)).parse_lazy(stream)
    }
}

pub fn none_of<'a, S: Stream>(items: &'a [u8]) -> NoneOf<S> {
    NoneOf {
        items: items.into_iter().map(|&b| b.into()).collect(),
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
                "" => err(Error::eoi().expected($e)),
                $(
                    concat!($t_err) => err(Error::item($t_err).expected($e)),
                )+
            });
            let mut p = $p();
            test_parser!(IndexedStream<&[u8]> => u8 | p, {
                $(
                    concat!($t_ok).as_bytes() => ok($t_ok as u8, ("".as_bytes(), 1)),
                )+
                "".as_bytes() => err(Error::eoi().expected($e)),
                $(
                    concat!($t_err).as_bytes() => err(Error::item($t_err as u8).expected($e)),
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
    use parser::test_utils::*;
    use stream::IndexedStream;

    type IStr = IndexedStream<&'static str>;

    #[test]
    fn test_any() {
        test_parser!(IStr => char | any(), {
            "hello, world." => ok('h', ("ello, world.", 1)),
            "" => err(Error::eoi().expected("a token")),
        });
    }

    #[test]
    fn test_token() {
        test_parser!(&str => char | item(b'c'), {
            "cat" => ok('c', "at"),
            "ace" => err(Error::item('a').expected(b'c')),
        });
    }

    #[test]
    fn test_eoi() {
        let mut parser = eoi();
        test_parser!(IStr => () | parser, {
            "" => noop(),
            "x" => err(Error::item('x').expected(Info::EOI)),
        });
        let mut parser = eoi::<_, u32>();
        assert_eq!(parser.parse(""), none_result(""));
    }

    #[test]
    fn test_satisfy() {
        let mut parser = satisfy(|&c: &char| c.is_numeric());
        test_parser!(&str => char | parser, {
            "123abc" => ok('1', "23abc"),
            "abc123" => err(Error::item('a')),
        });
    }

    mod test_negate {
        use super::*;

        #[test]
        fn test_negate() {
            let mut parser = negate(item(b'x'));
            test_parser!(IStr => char | parser, {
                "abc" => ok('a', ("bc", 1)),
                "xyz" => err(Error::item('x')),
                "" => err(Error::eoi()),
            });
        }

        #[test]
        fn test_negate_with() {
            let mut parser = negate(item(b'z').with(item(b'x')));
            test_parser!(IStr => char | parser, {
                "zy" => ok('z', ("y", 1)),
                "xx" => ok('x', ("x", 1)),
                "z" => ok('z', ("", 1)),
                "zx_" => err(Error::item('x')),
            });
        }

        #[test]
        fn test_negate_skip() {
            let mut parser = negate(item(b'z').skip(item(b'x')));
            test_parser!(IStr => char | parser, {
                "zy" => ok('z', ("y", 1)),
                "xx" => ok('x', ("x", 1)),
                "z" => ok('z', ("", 1)),
                "zx_" => err(Error::item('z')),
            });
        }
    }

    #[test]
    fn test_one_of() {
        let mut parser = one_of(&[b'a', b'0']);
        test_parser!(IStr => char | parser, {
            "ab" => ok('a', ("b", 1)),
            "0" => ok('0', ("", 1)),
            "" => err(Error::eoi().expected_one_of(vec![b'a', b'0'])),
            "z" => err(Error::item('z').expected_one_of(vec![b'a', b'0'])),
        });
    }

    #[test]
    fn test_none_of() {
        let mut parser = none_of(&[b'a', b'0']);
        test_parser!(IStr => char | parser, {
            "bc" => ok('b', ("c", 1)),
            "1" => ok('1', ("", 1)),
            "" => err(0, vec![Error::eoi()]),
            "a" => err(0, vec![Error::item('a')]),
        });
    }
}
