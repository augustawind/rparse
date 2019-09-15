use std::marker::PhantomData;

use error::ParseResult;
use parser::Parser;
use stream::Stream;
use traits::Maybe;

pub struct Optional<P, O> {
    parser: P,
    _marker: PhantomData<O>,
}

impl<P, O> Parser for Optional<P, O>
where
    P: Parser,
    O: Maybe<P::Output>,
{
    type Stream = P::Stream;
    type Output = O;

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let initial_pos = stream.position().clone();
        let (result, stream) = self.parser.parse(stream);
        // Ignore error if it occured at the top of the stream.
        if stream.position() > &initial_pos {
            if let Err(err) = result {
                return stream.errs(err);
            }
        }
        stream.ok(O::from_result(result))
    }
}

pub fn optional<P: Parser, O>(parser: P) -> Optional<P, O> {
    Optional {
        parser,
        _marker: PhantomData,
    }
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

pub struct Xor<L, R> {
    left: L,
    right: R,
}

impl<S: Stream, O, L, R> Parser for Xor<L, R>
where
    L: Parser<Stream = S, Output = O>,
    R: Parser<Stream = S, Output = O>,
{
    type Stream = S;
    type Output = O;

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let initial_pos = stream.position().clone();

        let (mut err, stream) = match self.left.parse(stream) {
            (Ok(result), stream) => return stream.ok(result),
            (Err(err), stream) => {
                if err.position > initial_pos {
                    return stream.errs(err);
                }
                (err, stream)
            }
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

pub fn xor<S: Stream, O, L, R>(left: L, right: R) -> Xor<L, R>
where
    L: Parser<Stream = S, Output = O>,
    R: Parser<Stream = S, Output = O>,
{
    Xor { left, right }
}

#[macro_export]
macro_rules! xchoice {
    ($head:expr) => {
        $head
    };
    ($head:expr, $($tail:expr),+ $(,)*) => {
        $head.xor(xchoice!($($tail),+))
    };
}

#[cfg(test)]
mod test {
    use super::*;
    use error::Error::{self, *};
    use parser::range::range;
    use parser::seq::{many, many1, then};
    use parser::token::{any, ascii, token};
    use parser::Parser;
    use stream::IndexedStream;

    #[test]
    fn test_optional() {
        let mut parser = optional(token(b'x'));
        test_parser!(&str | parser, {
            "" => (Ok(None), "");
            "y" => (Ok(None), "y");
            "x" => (Ok(Some('x')), "");
            "xyz" => (Ok(Some('x')), "yz");
        });

        let mut parser = optional(token(b'x'));
        test_parser!(&str | parser, {
            "" => (Ok(vec![]), "");
            "y" => (Ok(vec![]), "y");
            "x" => (Ok(vec!['x']), "");
            "xyz" => (Ok(vec!['x']), "yz");
        });

        let mut parser = optional(many1(ascii::alpha_num()));
        test_parser!(&str | parser, {
            "abc123" => (Ok(Some("abc123".chars().collect())), "");
        });

        let mut parser = optional(many(any()));
        assert_eq!(parser.parse(""), (Ok(vec![vec![]]), ""));

        let mut parser = token(b'x').optional();
        test_parser!(&str => String | parser, {
            "x" => ok(Ok("x".to_string()), ""),
            "y" => ok(Ok("".to_string()), "y"),
        });
    }

    #[test]
    fn test_and() {
        let mut parser = and(token(b'a'), token(b'b'));
        test_parser!(IndexedStream<&str> => char | parser, {
            "abcd" => ok(Ok('b'), ("cd", 2)),
            "ab" => ok(Ok('b'), ("", 2)),
            "def" => err(0, vec![Unexpected('d'.into()), Expected('a'.into())]),
            "aab" => err(1, vec![Unexpected('a'.into()), Expected('b'.into())]),
            "bcd" => err(0, vec![Unexpected('b'.into()), Expected('a'.into())]),
        });

        let mut parser = and(many1(ascii::digit()), many1(ascii::letter()));
        test_parser!(IndexedStream<&str> => Vec<char> | parser, {
            "123abc456" => ok(Ok(vec!['a', 'b', 'c']), ("456", 6)),
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
        test_parser!(IndexedStream<&str> | parser, {
            "bcd" => (Ok('b'), ("cd", 1));
            "a" => (Ok('a'), ("", 1));
        }, {
            "def" => (0, vec![
                Unexpected('d'.into()),
                Expected('a'.into()),
                Expected('b'.into()),
            ]);
        });

        let mut parser = or(
            many1(ascii::digit()),
            then(ascii::letter(), ascii::whitespace()),
        )
        .collect();
        test_parser!(IndexedStream<&str> | parser, {
            "123a bc" => (Ok("123".to_string()), ("a bc", 3));
            "a b c" => (Ok("a ".to_string()), ("b c", 2));
        });
    }

    #[test]
    fn test_xor() {
        let mut parser = xor(range("bar"), range("dar"));
        test_parser!(IndexedStream<&str> => &str | parser, {
            "bar9" => ok(Ok("bar"), ("9", 3)),
            "dar9" => ok(Ok("dar"), ("9", 3)),
            "bam" => err(2, vec![Unexpected('m'.into()), Error::expected_range("bar")]),
            "rar" => err(0, vec![
                Unexpected('r'.into()),
                Error::expected_range("bar"),
                Error::expected_range("dar"),
            ]),
        });

        let mut parser = many1(ascii::digit())
            .xor(token(b'x').wrap().extend(many1(token(b'x'))))
            .xor(token(b'x').then(token(b'_')).append(token(b'x')))
            .collect();
        test_parser!(IndexedStream<&str> => String | parser, {
            "12345?" => ok(Ok("12345".into()), ("?", 5)),
            "xxx666" => ok(Ok("xxx".into()), ("666", 3)),
            "" => err(0, vec![
                Error::unexpected_eoi(),
                Expected("an ascii digit".into()),
                Expected('x'.into()),
            ]),
            "x12" => err(1, vec![Unexpected('1'.into()), Expected('x'.into())]),
            "x_x" => err(1, vec![Unexpected('_'.into()), Expected('x'.into())]),
        });
    }

    #[test]
    fn test_choice() {
        assert_eq!(choice!(token(b'a'), token(b'b')).parse("a"), (Ok('a'), ""));

        let mut parser = choice!(token(b'a'), ascii::digit(), ascii::punctuation());
        test_parser!(IndexedStream<&str> => char | parser, {
            "a9." => ok(Ok('a'), ("9.", 1)),
            "9.a" => ok(Ok('9'), (".a", 1)),
            ".a9" => ok(Ok('.'), ("a9", 1)),
            "ba9." => err(0, vec![
                Unexpected('b'.into()),
                Expected('a'.into()),
                Expected("an ascii digit".into()),
                Expected("an ascii punctuation character".into()),
            ]),
        });

        assert_eq!(
            choice!(token(b'a'), token(b'b'), token(b'c')).parse("bcd"),
            (Ok('b'), "cd")
        );

        assert_eq!(choice!(ascii::letter()).parse("Z"), (Ok('Z'), ""));

        let mut parser = choice!(
            many1(ascii::digit()),
            then(ascii::letter(), ascii::whitespace()),
        )
        .collect();
        test_parser!(IndexedStream<&str> => String | parser, {
            "123a bc" => ok(Ok("123".to_string()), ("a bc", 3)),
            "a b c" => ok(Ok("a ".to_string()), ("b c", 2)),
        });
    }
}
