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
        let (err, stream) = match self.left.parse(stream) {
            (Ok(result), stream) => return stream.ok(result),
            (Err(err), stream) => (err, stream),
        };
        match self.right.parse(stream) {
            (Ok(result), stream) => stream.ok(result),
            (Err(err2), stream) => {
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
    use parser::combinator::{many, many1, then};
    use parser::token::{any, ascii, token};

    #[test]
    fn test_optional() {
        let mut parser = optional(token('x'));
        test_parser!(&str | parser, {
            "" => (Ok(None), ""),
            "y" => (Ok(None), "y"),
            "x" => (Ok(Some('x')), ""),
            "xyz" => (Ok(Some('x')), "yz"),
        });

        let mut parser = optional(many1::<_, String>(ascii::alpha_num()));
        test_parser!(&str | parser, {
            "abc123" => (Ok(Some("abc123".to_string())), ""),
        }, {
            "" => (|&err| is_match!(Error::EOF = err)),
        });

        assert_eq!(
            optional(many::<_, String>(any())).parse(""),
            (Ok(Some("".into())), "")
        );
    }

    #[test]
    fn test_and() {
        let mut parser = and(token('a'), token('b'));
        assert_eq!(parser.parse("abcd"), (Ok('b'), "cd"));
        assert_eq!(parser.parse("ab"), (Ok('b'), ""));
        assert_parse_err!(parser.parse("def"), "def");
        assert_parse_err!(parser.parse("aab"), "aab");
        assert_parse_err!(parser.parse("bcd"), "bcd");

        let mut parser = and(many1(ascii::digit()), many1(ascii::letter()));
        assert_eq!(parser.parse("123abc456"), (Ok(vec!['a', 'b', 'c']), "456"));
        assert_parse_err!(parser.parse(" 1 2 3"), " 1 2 3");
        assert_parse_err!(parser.parse("123 abc"), "123 abc");
    }

    #[test]
    fn test_or() {
        let mut parser = or(token('a'), token('b'));
        assert_eq!(parser.parse("bcd"), (Ok('b'), "cd"));
        assert_eq!(parser.parse("a"), (Ok('a'), ""));
        assert_parse_err!(parser.parse("def"), "def");

        let mut parser = or(
            many1(ascii::digit()),
            then(ascii::letter(), ascii::whitespace()),
        );
        assert_eq!(parser.parse("123a bc"), (Ok("123".to_string()), "a bc"));
        assert_eq!(parser.parse("a b c"), (Ok("a ".to_string()), "b c"));
    }

    #[test]
    fn test_choice() {
        assert_eq!(choice!(token('a'), token('b')).parse("a"), (Ok('a'), ""));

        let mut parser = choice!(token('a'), ascii::digit(), ascii::punctuation());
        assert_eq!(parser.parse("a9."), (Ok('a'), "9."));
        assert_eq!(parser.parse("9.a"), (Ok('9'), ".a"));
        assert_eq!(parser.parse(".a9"), (Ok('.'), "a9"));
        assert_parse_err!(parser.parse("ba9."), "ba9.");

        let mut parser = choice!(token('a'), token('b'), token('c'));
        assert_eq!(parser.parse("bcd"), (Ok('b'), "cd"));
        assert_parse_err!(parser.parse("def"), "def");

        let mut parser = choice!(ascii::letter());
        assert_eq!(parser.parse("Z"), (Ok('Z'), ""));
        assert_parse_err!(parser.parse("9"), "9");

        let mut parser = choice!(
            many1(ascii::digit()),
            then(ascii::letter(), ascii::whitespace()),
        );
        assert_eq!(parser.parse("123a bc"), (Ok("123".to_string()), "a bc"));
        assert_eq!(parser.parse("a b c"), (Ok("a ".to_string()), "b c"));
    }
}
