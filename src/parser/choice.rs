use error::ParseResult;
use parser::Parser;
use stream::Stream;

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
        match self.parser.parse(stream) {
            (Ok(Some(result)), stream) => stream.ok(result),
            (_, stream) => stream.noop(),
        }
    }
}

pub fn optional<P: Parser>(parser: P) -> Optional<P> {
    Optional { parser }
}

pub struct And<L, R> {
    left: L,
    right: R,
}

impl<S: Stream, O, L, R> Parser for And<L, R>
where
    L: Parser<Stream = S>,
    R: Parser<Stream = S, Output = O>,
{
    type Stream = S;
    type Output = O;

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, O> {
        match self.left.parse(stream) {
            (Ok(_), stream) => self.right.parse(stream),
            (Err(err), stream) => stream.errs(err),
        }
    }
}

pub fn and<S: Stream, O, L, R>(left: L, right: R) -> And<L, R>
where
    L: Parser<Stream = S>,
    R: Parser<Stream = S, Output = O>,
{
    And { left, right }
}

pub struct Or<L, R> {
    left: L,
    right: R,
}

impl<S: Stream, O, L, R> Parser for Or<L, R>
where
    L: Parser<Stream = S, Output = O>,
    R: Parser<Stream = S, Output = O>,
{
    type Stream = S;
    type Output = O;

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let (mut err, stream) = match self.left.parse(stream) {
            (Ok(result), stream) => return stream.result(result),
            (Err(err), stream) => (err, stream),
        };
        match self.right.parse(stream) {
            (Ok(result), stream) => stream.result(result),
            (Err(mut err2), stream) => {
                err.merge_errors(&mut err2);
                stream.errs(err)
            }
        }
    }
}

pub fn or<S: Stream, O, L, R>(left: L, right: R) -> Or<L, R>
where
    L: Parser<Stream = S, Output = O>,
    R: Parser<Stream = S, Output = O>,
{
    Or { left, right }
}

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
    use parser::seq::{many, many1, then};
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
            "def" => err(0, vec![Unexpected('d'.into()), Expected('a'.into())]),
            "aab" => err(1, vec![Unexpected('a'.into()), Expected('b'.into())]),
            "bcd" => err(0, vec![Unexpected('b'.into()), Expected('a'.into())]),
        });

        let mut parser = and(many1(ascii::digit()), many1(ascii::letter()));
        test_parser!(IndexedStream<&str> => Vec<char> | parser, {
            "123abc456" => ok(vec!['a', 'b', 'c'], ("456", 6)),
            " 1 2 3" => err(0, vec![
                Unexpected(' '.into()),
                Expected("an ascii digit".into())
            ]),
            "123 abc" => err(3, vec![
                 Unexpected(' '.into()),
                 Expected("an ascii letter".into())
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
                Expected('a'.into()),
                Expected('b'.into()),
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
                Expected('a'.into()),
                Expected("an ascii digit".into()),
                Expected("an ascii punctuation character".into()),
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
