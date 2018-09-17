use error::ParseResult;
use parser::Parser;
use stream::Stream;

pub struct Optional<P>(P);

impl<P: Parser> Parser for Optional<P> {
    type Stream = P::Stream;
    type Output = Option<P::Output>;

    fn parse_stream(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.0.parse(stream) {
            (Ok(output), stream) => stream.ok(Some(output)),
            (Err(_), stream) => stream.ok(None),
        }
    }
}

pub fn optional<P: Parser>(p: P) -> Optional<P> {
    Optional(p)
}

pub struct And<L, R> {
    left: L,
    right: R,
}

impl<S: Stream, O, L, R> Parser for And<L, R>
where
    L: Parser<Stream = S, Output = O>,
    R: Parser<Stream = S, Output = O>,
{
    type Stream = S;
    type Output = O;

    fn parse_stream(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.left.parse(stream) {
            (Ok(_), stream) => self.right.parse(stream),
            err => err,
        }
    }
}

pub fn and<S: Stream, O, L, R>(left: L, right: R) -> And<L, R>
where
    L: Parser<Stream = S, Output = O>,
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

    fn parse_stream(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let (mut err, stream) = match self.left.parse(stream) {
            (Ok(result), stream) => return stream.ok(result),
            (Err(err), stream) => (err, stream),
        };
        match self.right.parse(stream) {
            (Ok(result), stream) => stream.ok(result),
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
    use error::Error;
    use parser::seq::{many, many1, then};
    use parser::token::{any, ascii, token};
    use stream::IndexedStream;

    #[test]
    fn test_optional() {
        let mut parser = optional(token('x'));
        test_parser!(&str | parser, {
            "" => (Ok(None), "");
            "y" => (Ok(None), "y");
            "x" => (Ok(Some('x')), "");
            "xyz" => (Ok(Some('x')), "yz");
        });

        let mut parser = optional(many1::<String, _>(ascii::alpha_num()));
        test_parser!(&str | parser, {
            "abc123" => (Ok(Some("abc123".to_string())), "");
        });

        assert_eq!(
            optional(many::<String, _>(any())).parse(""),
            (Ok(Some("".into())), "")
        );
    }

    #[test]
    fn test_and() {
        let mut parser = and(token('a'), token('b'));
        test_parser!(IndexedStream<&str> | parser, {
            "abcd" => (Ok('b'), ("cd", 2));
            "ab" => (Ok('b'), ("", 2));
        });
        test_parser_errors!(IndexedStream<&str> | parser, {
            "def" => (0, vec![Error::unexpected_token('d'), Error::expected_token('a')]);
            "aab" => (1, vec![Error::unexpected_token('a'), Error::expected_token('b')]);
            "bcd" => (0, vec![Error::unexpected_token('b'), Error::expected_token('a')]);
        });

        let mut parser = and(many1(ascii::digit()), many1(ascii::letter()));
        test_parser!(IndexedStream<&str> | parser, {
            "123abc456" => (Ok(vec!['a', 'b', 'c']), ("456", 6));
        });
        test_parser_errors!(IndexedStream<&str> | parser, {
            " 1 2 3" => (0, vec![Error::unexpected_token(' ')]);
            "123 abc" => (3, vec![Error::unexpected_token(' ')]);
        });
    }

    #[test]
    fn test_or() {
        let mut parser = or(token('a'), token('b'));
        test_parser!(IndexedStream<&str> | parser, {
            "bcd" => (Ok('b'), ("cd", 1));
            "a" => (Ok('a'), ("", 1));
        });
        test_parser_errors!(IndexedStream<&str> | parser, {
            "def" => (0, vec![
                Error::unexpected_token('d'),
                Error::expected_token('a'),
                Error::expected_token('b'),
            ]);
        });

        let mut parser = or(
            many1(ascii::digit()),
            then(ascii::letter(), ascii::whitespace()),
        );
        test_parser!(IndexedStream<&str> | parser, {
            "123a bc" => (Ok("123".to_string()), ("a bc", 3));
            "a b c" => (Ok("a ".to_string()), ("b c", 2));
        });
    }

    #[test]
    fn test_choice() {
        assert_eq!(choice!(token('a'), token('b')).parse("a"), (Ok('a'), ""));

        let mut parser = choice!(token('a'), ascii::digit(), ascii::punctuation());
        test_parser!(IndexedStream<&str> | parser, {
            "a9." => (Ok('a'), ("9.", 1));
            "9.a" => (Ok('9'), (".a", 1));
            ".a9" => (Ok('.'), ("a9", 1));
        });
        test_parser_errors!(IndexedStream<&str> | parser, {
            "ba9." => (0, vec![Error::unexpected_token('b'), Error::expected_token('a')]);
        });

        assert_eq!(
            choice!(token('a'), token('b'), token('c')).parse("bcd"),
            (Ok('b'), "cd")
        );

        assert_eq!(choice!(ascii::letter()).parse("Z"), (Ok('Z'), ""));

        let mut parser = choice!(
            many1(ascii::digit()),
            then(ascii::letter(), ascii::whitespace()),
        );
        test_parser!(IndexedStream<&str> | parser, {
            "123a bc" => (Ok("123".to_string()), ("a bc", 3));
            "a b c" => (Ok("a ".to_string()), ("b c", 2));
        });
    }
}
