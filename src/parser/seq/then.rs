use error::ParseResult;
use parser::Parser;
use stream::Stream;

pub struct Then<L, R> {
    p1: L,
    p2: R,
}

impl<S: Stream, O, L, R> Parser for Then<L, R>
where
    L: Parser<Stream = S, Output = O>,
    R: Parser<Stream = S, Output = O>,
{
    type Stream = S;
    type Output = Vec<O>;

    fn parse_partial(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let (first, stream) = self.p1.parse_partial(stream)?;
        let (second, stream) = self.p2.parse_partial(stream)?;
        stream.ok(first.into_iter().chain(second.into_iter()).collect())
    }
}

pub fn then<S: Stream, O, L, R>(p1: L, p2: R) -> Then<L, R>
where
    L: Parser<Stream = S, Output = O>,
    R: Parser<Stream = S, Output = O>,
{
    Then { p1, p2 }
}

#[cfg(test)]
mod test {
    use parser::token::token;
    use stream::IndexedStream;
    use {Error, Parser};

    #[test]
    fn test_then() {
        let mut parser = token(b'X').then(token(b'O'));
        test_parser!(IndexedStream<&str> => Vec<char> | parser, {
            "XO" => ok("XO".chars().collect(), ("", 2)),
            "XOXO" => ok("XO".chars().collect(), ("XO", 2)),
            "XY" => err(1, vec![Error::unexpected_token('Y'), Error::expected_token('O')]),
            "ZY" => err(0, vec![Error::unexpected_token('Z'), Error::expected_token('X')]),
        });
    }
}
