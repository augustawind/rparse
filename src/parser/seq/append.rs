use error::ParseResult;
use parser::Parser;
use stream::Stream;

pub struct Append<L, R> {
    left: L,
    right: R,
}

impl<S: Stream, O, L, R> Parser for Append<L, R>
where
    L: Parser<Stream = S, Output = Vec<O>>,
    R: Parser<Stream = S, Output = O>,
{
    type Stream = S;
    type Output = Vec<O>;

    fn parse_partial(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.left.parse_partial(stream) {
            Ok((Some(mut left), stream)) => match self.right.parse_partial(stream) {
                Ok((right, stream)) => {
                    if let Some(r) = right {
                        left.push(r);
                    }
                    stream.ok(left)
                }
                Err((err, stream)) => stream.errs(err),
            },
            Ok((None, stream)) => stream.noop(),
            Err((err, stream)) => stream.errs(err),
        }
    }
}

pub fn append<S: Stream, O, L, R>(left: L, right: R) -> Append<L, R>
where
    L: Parser<Stream = S, Output = Vec<O>>,
    R: Parser<Stream = S, Output = O>,
{
    Append { left, right }
}

// Create a parser which runs a series of subparsers in sequence, returning the collected output.
// This is basically just syntactic sugar for chaining parsers with `append`.
#[macro_export]
macro_rules! seq {
    ($init:expr, $head:expr $(,)*) => {
        $init.then($head)
    };

    ($init:expr, $head:expr, $($tail:expr),* $(,)*) => {
        seq!(@inner $init.then($head) $(, $tail)*)
    };

    (@inner $head:expr $(,)*) => {
        $head
    };

    (@inner $head:expr, $($tail:expr),* $(,)*) => {
        $head $(.append($tail))*
    };
}

#[cfg(test)]
mod test {
    use super::*;
    use error::Error;
    use parser::seq::many;
    use parser::token::ascii::{hexdigit, letter};
    use parser::token::token;
    use parser::Parser;
    use stream::IndexedStream;
    use Error::*;

    #[test]
    fn test_append() {
        let mut parser = append(many(letter()), token(b'?'));
        test_parser!(IndexedStream<&str> => Vec<char> | parser, {
            "huh?" => ok("huh?".chars().collect(), ("", 4)),
            "oh? cool" => ok("oh?".chars().collect(), (" cool", 3)),
            "???" => ok("?".chars().collect(), ("??", 1)),
            "" => err(0, vec![Error::unexpected_eoi(), Error::expected_token('?')]),
            "" => err(0, vec![Error::unexpected_eoi(), Error::expected_token('?')]),
            "whoops!" => err(6, vec![Error::unexpected_token('!'), Error::expected_token('?')]),
            "!?" => err(0, vec![Error::unexpected_token('!'), Error::expected_token('?')]),
        });
    }

    #[test]
    fn test_seq_macro() {
        let mut parser = seq![token(b'%'), hexdigit(), hexdigit()].collect();
        test_parser!(IndexedStream<&str> => String | parser, {
            "%AF" => ok("%AF".into(), ("", 3)),
            "%d8_/^/_" => ok("%d8".into(), ("_/^/_", 3)),
            "" => err(0, vec![Error::unexpected_eoi(), Error::expected_token('%')]),
            "%0" => err(2, vec![Error::unexpected_eoi(), Error::expected("a hexadecimal digit")]),
            "%zz" => err(1, vec![Unexpected('z'.into()), Error::expected("a hexadecimal digit")]),
        });

        let mut parser = seq![token(b'x'), token(b'y')].collect();
        test_parser!(IndexedStream<&str> => String | parser, {
            "xy" => ok("xy".into(), ("", 2)),
        });

        let mut parser = seq![token(b'a'), token(b'b'), token(b'c'), token(b'd')].collect();
        test_parser!(IndexedStream<&str> => String | parser, {
            "abcd" => ok("abcd".into(), ("", 4)),
        });
    }
}
