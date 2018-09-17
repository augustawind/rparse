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

    fn parse_stream(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.left.parse_stream(stream) {
            (Ok(mut left), stream) => match self.right.parse_stream(stream) {
                (Ok(right), stream) => {
                    left.push(right);
                    stream.ok(left)
                }
                (Err(err), stream) => stream.errs(err),
            },
            (Err(err), stream) => stream.errs(err),
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
    ($init:expr, $head:expr $(, $tail:expr),* $(,)*) => {{
        let head = $init.then($head);
        seq!(@inner head, $($tail),*)
    }};

    (@inner $head:expr $(,)*) => {{
        $head
    }};

    (@inner $head:expr, $($tail:expr),* $(,)*) => {{
        $head.append(seq!(@inner $($tail),*))
    }};
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

    #[test]
    fn test_append() {
        let mut parser = append(many(letter()), token(b'?'));
        test_parser!(IndexedStream<&str> | parser, {
            "huh?" => (Ok("huh?".chars().collect()), ("", 4));
            "oh? cool" => (Ok("oh?".chars().collect()), (" cool", 3));
            "???" => (Ok("?".chars().collect()), ("??", 1));
        }, {
            "" => (0, vec![Error::EOF, Error::expected_token('?')]);
            "" => (0, vec![Error::EOF, Error::expected_token('?')]);
            "whoops!" => (6, vec![Error::unexpected_token('!'), Error::expected_token('?')]);
            "!?" => (0, vec![Error::unexpected_token('!'), Error::expected_token('?')]);
        });
    }

    #[test]
    fn test_seq_macro() {
        let mut parser = seq![token(b'%'), hexdigit(), hexdigit()];
        test_parser!(IndexedStream<&str> | parser, {
            "%AF" => (Ok("%AF".chars().collect()), ("", 3));
            "%d8_/^/_" => (Ok("%d8".chars().collect()), ("_/^/_", 3));
        }, {
            "" => (0, vec![Error::EOF, Error::expected_token('%')]);
            "%0" => (2, vec![Error::EOF]);
            "%zz" => (1, vec![Error::unexpected_token('z')]);
        });
    }
}
