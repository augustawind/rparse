use error::ParseResult;
use parser::Parser;
use stream::Stream;

pub struct Then<L, R> {
    left: L,
    right: R,
}

impl<S: Stream, O, L, R> Parser for Then<L, R>
where
    L: Parser<Stream = S, Output = O>,
    R: Parser<Stream = S, Output = O>,
{
    type Stream = S;
    type Output = Vec<O>;

    fn parse_partial(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.left.parse_partial(stream) {
            Ok((first, stream)) => match self.right.parse_partial(stream) {
                Ok((second, stream)) => {
                    stream.ok(first.into_iter().chain(second.into_iter()).collect())
                }
                Err((err, stream)) => stream.errs(err),
            },
            Err((err, stream)) => stream.errs(err),
        }
    }
}

pub fn then<S: Stream, O, L, R>(left: L, right: R) -> Then<L, R>
where
    L: Parser<Stream = S, Output = O>,
    R: Parser<Stream = S, Output = O>,
{
    Then { left, right }
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
