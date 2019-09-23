use {Error, ParseResult, Parser, Stream};

pub struct Skip<P1, P2> {
    p1: P1,
    p2: P2,
}

impl<S, P1, P2> Parser for Skip<P1, P2>
where
    S: Stream,
    P1: Parser<Stream = S>,
    P2: Parser<Stream = S>,
{
    type Stream = S;
    type Output = P1::Output;

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let (result, stream) = self.p1.parse_lazy(stream)?;
        let (_, stream) = self.p2.parse_lazy(stream)?;
        stream.result(result)
    }

    fn expected_errors(&self) -> Vec<Error<Self::Stream>> {
        vec![Error::expected(
            [self.p1.expected_errors(), self.p2.expected_errors()].concat(),
        )]
    }
}

/// Parses with `self` followed by `p`. Succeeds if both parsers succeed, otherwise fails.
/// Returns the result of `p` on success.
///
/// Useful when you need a parser to consume input but you don't care about the result. Equivalent
/// to [`parser.skip()`].
///
/// [`parser.skip()`]: Parser::skip
pub fn skip<P1, P2>(p1: P1, p2: P2) -> Skip<P1, P2> {
    Skip { p1, p2 }
}

pub struct Optional<P> {
    parser: P,
}

impl<P> Parser for Optional<P>
where
    P: Parser,
{
    type Stream = P::Stream;
    type Output = P::Output;

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.parser.parse_lazy(stream) {
            Ok((Some(result), stream)) => stream.ok(result),
            Ok((None, stream)) => stream.noop(),
            Err((_, stream)) => stream.noop(),
        }
    }

    fn expected_errors(&self) -> Vec<Error<Self::Stream>> {
        self.parser.expected_errors()
    }
}

/// Wrap `parser` so that if it would fail it returns `None` instead. Equivalent to
/// [`parser.optional()`].
///
/// [`parser.optional()`]: Parser::optional
pub fn optional<P: Parser>(parser: P) -> Optional<P> {
    Optional { parser }
}

pub struct Required<P> {
    parser: P,
}

impl<P: Parser> Parser for Required<P> {
    type Stream = P::Stream;
    type Output = P::Output;

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.parser.parse_lazy(stream) {
            Ok((Some(result), stream)) => stream.ok(result),
            Ok((None, stream)) => stream.empty_errs(),
            Err((errors, stream)) => stream.errs(errors),
        }
    }

    fn expected_errors(&self) -> Vec<Error<Self::Stream>> {
        self.parser.expected_errors()
    }
}

/// Wrap `parser` so that if it would `None` it fails instead.
///
/// Equivalent to [`parser.required()`].
///
/// [`parser.required()`]: Parser::required
pub fn required<P: Parser>(parser: P) -> Required<P> {
    Required { parser }
}

pub struct And<L, R> {
    p1: L,
    p2: R,
}

impl<S: Stream, O, L, R> Parser for And<L, R>
where
    L: Parser<Stream = S>,
    R: Parser<Stream = S, Output = O>,
{
    type Stream = S;
    type Output = O;

    fn parse_partial(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, O> {
        let (_, stream) = self.p1.parse_partial(stream)?;
        self.p2.parse_partial(stream)
    }
}

/// Equivalent to [`p1.and(p2)`].
///
/// [`p1.and(p2)`]: Parser::and
pub fn and<S: Stream, O, L, R>(p1: L, p2: R) -> And<L, R>
where
    L: Parser<Stream = S>,
    R: Parser<Stream = S, Output = O>,
{
    And { p1, p2 }
}

pub struct Or<L, R> {
    p1: L,
    p2: R,
}

impl<S: Stream, O, L, R> Parser for Or<L, R>
where
    L: Parser<Stream = S, Output = O>,
    R: Parser<Stream = S, Output = O>,
{
    type Stream = S;
    type Output = O;

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let (mut err, stream) = match self.p1.try_parse(stream) {
            Ok((result, stream)) => return stream.result(result),
            Err((err, stream)) => (err, stream),
        };
        match self.p2.parse_lazy(stream) {
            Ok((result, stream)) => stream.result(result),
            Err((mut err2, stream)) => {
                err.merge_errors(&mut err2);
                stream.errs(err)
            }
        }
    }

    fn expected_errors(&self) -> Vec<Error<Self::Stream>> {
        vec![Error::expected(Error::one_of(
            [self.p1.expected_errors(), self.p2.expected_errors()].concat(),
        ))]
    }
}

/// Equivalent to [`p1.or(p2)`].
///
/// [`p1.or(p2)`]: Parser::or
pub fn or<S: Stream, O, L, R>(p1: L, p2: R) -> Or<L, R>
where
    L: Parser<Stream = S, Output = O>,
    R: Parser<Stream = S, Output = O>,
{
    Or { p1, p2 }
}

/// Try one or more parsers, returning from the first one that succeeds.
///
/// Equivalent to chaining parsers with [`or`]. For example, these two parsers are equivalent:
///
/// ```
/// # #[macro_use]
/// # extern crate rparse;
/// # use rparse::Parser;
/// # use rparse::parser::token::token;
/// # fn main() {
/// let mut p1 = choice![token(b'x'), token(b'y'), token(b'z')];
/// let mut p2 = token(b'x').or(token(b'y').or(token(b'z')));
/// assert_eq!(p1.parse("x123"), p2.parse("x123"));
/// # }
/// ```
///
/// [`or`]: Parser::or
#[macro_export]
macro_rules! choice {
    ($head:expr) => {
        $head
    };
    ($head:expr, $($tail:expr),+ $(,)*) => {
        $head.or(choice!($($tail),+))
    };
}

#[cfg(test)]
mod test {
    use super::*;
    use error::Error::*;
    use parser::repeat::{many, many1};
    use parser::seq::then;
    use parser::test_utils::*;
    use parser::token::{any, ascii, token};
    use parser::Parser;
    use stream::IndexedStream;

    #[test]
    fn test_optional() {
        let mut parser = optional(token(b'x'));
        test_parser!(&str => char | parser, {
            "" => noop(),
            "y" => noop(),
            "x" => ok('x', ""),
            "xyz" => ok('x', "yz"),
        });

        let mut parser = optional(token(b'x'));
        test_parser!(&str => char | parser, {
            "" => noop(),
            "y" => noop(),
            "x" => ok('x', ""),
            "xyz" => ok('x', "yz"),
        });

        let mut parser = optional(many1(ascii::alpha_num()));
        test_parser!(&str => Vec<char> | parser, {
            "abc123" => ok("abc123".chars().collect(), ""),
        });

        let mut parser = optional(many(any()));
        assert_eq!(parser.parse(""), ok_result(vec![], ""));

        let mut parser = token(b'x').optional();
        test_parser!(&str => char | parser, {
            "x" => ok('x', ""),
            "y" => noop(),
        });
    }

    #[test]
    fn test_and() {
        let mut parser = and(token(b'a'), token(b'b'));
        test_parser!(IndexedStream<&str> => char | parser, {
            "abcd" => ok('b', ("cd", 2)),
            "ab" => ok('b', ("", 2)),
            "def" => err(0, vec![Error::unexpected_token('d'), Error::expected_token('a')]),
            "aab" => err(1, vec![Error::unexpected_token('a'), Error::expected_token('b')]),
            "bcd" => err(0, vec![Error::unexpected_token('b'), Error::expected_token('a')]),
        });

        let mut parser = and(many1(ascii::digit()), many1(ascii::letter()));
        test_parser!(IndexedStream<&str> => Vec<char> | parser, {
            "123abc456" => ok(vec!['a', 'b', 'c'], ("456", 6)),
            " 1 2 3" => err(0, vec![
                Unexpected(' '.into()),
                Error::expected("an ascii digit"),
            ]),
            "123 abc" => err(3, vec![
                 Unexpected(' '.into()),
                 Error::expected("an ascii letter"),
            ]),
        });
    }

    #[test]
    fn test_or() {
        let mut parser = or(token(b'a'), token(b'b'));
        test_parser!(IndexedStream<&str> => char | parser, {
            "bcd" => ok('b', ("cd", 1)),
            "a" => ok('a', ("", 1)),
            "def" => err(0, vec![
                Unexpected('d'.into()),
                Error::expected(Error::one_of(vec![Info('a'.into()), Info('b'.into())])),
            ]),
        });

        let mut parser = or(
            many1(ascii::digit()),
            then(ascii::letter(), ascii::whitespace()),
        )
        .collect();
        test_parser!(IndexedStream<&str> => String | parser, {
            "123a bc" => ok("123".into(), ("a bc", 3)),
            "a b c" => ok("a ".into(), ("b c", 2)),
        });
    }

    #[test]
    fn test_choice() {
        assert_eq!(
            choice!(token(b'a'), token(b'b')).parse("a"),
            ok_result('a', "")
        );

        let mut parser = choice!(token(b'a'), ascii::digit(), ascii::punctuation());
        test_parser!(IndexedStream<&str> => char | parser, {
            "a9." => ok('a', ("9.", 1)),
            "9.a" => ok('9', (".a", 1)),
            ".a9" => ok('.', ("a9", 1)),
            "ba9." => err(0, vec![
                Unexpected('b'.into()),
                Error::expected(Error::one_of(vec![
                    Info('a'.into()),
                    "an ascii digit".into(),
                    "an ascii punctuation character".into()
                ])),
            ]),
        });

        assert_eq!(
            choice!(token(b'a'), token(b'b'), token(b'c')).parse("bcd"),
            ok_result('b', "cd"),
        );

        assert_eq!(choice!(ascii::letter()).parse("Z"), ok_result('Z', ""));

        let mut parser = choice!(
            many1(ascii::digit()),
            then(ascii::letter(), ascii::whitespace()),
        )
        .collect();
        test_parser!(IndexedStream<&str> => String | parser, {
            "123a bc" => ok("123".to_string(), ("a bc", 3)),
            "a b c" => ok("a ".to_string(), ("b c", 2)),
        });
    }
}
