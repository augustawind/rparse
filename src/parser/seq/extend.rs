use error::ParseResult;
use parser::Parser;
use stream::Stream;

pub struct Extend<L, R> {
    left: L,
    right: R,
}

impl<S: Stream, O, L, R> Parser for Extend<L, R>
where
    L: Parser<Stream = S, Output = Vec<O>>,
    R: Parser<Stream = S, Output = Vec<O>>,
{
    type Stream = S;
    type Output = Vec<O>;

    fn parse_stream(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.left.parse_stream(stream) {
            (Ok(mut left), stream) => match self.right.parse_stream(stream) {
                (Ok(right), stream) => {
                    left.extend(right.into_iter());
                    stream.ok(left)
                }
                (Err(err), stream) => stream.errs(err),
            },
            (Err(err), stream) => stream.errs(err),
        }
    }
}

pub fn extend<S: Stream, O, L, R>(left: L, right: R) -> Extend<L, R>
where
    L: Parser<Stream = S, Output = Vec<O>>,
    R: Parser<Stream = S, Output = Vec<O>>,
{
    Extend { left, right }
}

#[macro_export]
macro_rules! concat {
    ($head:expr) => { $head };
    ($head:expr, $($tail:expr),+ $(,)*) => {
        $head.extend(concat![$($tail),+])
    };
}

#[cfg(test)]
mod test {
    use super::*;
    use error::Error;
    use parser::seq::{many, many1};
    use parser::token::ascii;
    use parser::token::token;
    use parser::Parser;
    use stream::IndexedStream;

    #[test]
    fn test_extend() {
        let mut parser = extend(many(ascii::letter()), many1(token('?')));
        test_parser!(IndexedStream<&str> | parser, {
            "huh???" => (Ok("huh???".chars().collect()), ("", 6));
            "oh?? cool" => (Ok("oh??".chars().collect()), (" cool", 4));
            "???" => (Ok("???".chars().collect()), ("", 3));
        }, {
            "" => (0, vec![Error::EOF, Error::expected_token('?')]);
            "whoops!" => (6, vec![Error::unexpected_token('!'), Error::expected_token('?')]);
            "!?" => (0, vec![Error::unexpected_token('!'), Error::expected_token('?')]);
        });
    }

    #[test]
    fn test_concat_macro() {
        let mut parser = concat![
            many(ascii::whitespace()),
            ascii::letter().then(ascii::alpha_num()),
            many(ascii::alpha_num()),
        ].collect();
        test_parser!(IndexedStream<&str> | parser, {
            "x9" => (Ok("x9".to_string()), ("", 2));
            "t1t3 man" => (Ok("t1t3".to_string()), (" man", 4));
            "  xs = [2, 3]" => (Ok("  xs".to_string()), (" = [2, 3]", 4));
        }, {
            "" => (0, vec![Error::EOF]);
            "a" => (1, vec![Error::EOF]);
            "?" => (0, vec![Error::unexpected_token('?')]);
            "a?" => (1, vec![Error::unexpected_token('?')]);
        });
    }
}
