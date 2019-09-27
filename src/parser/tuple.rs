use {ParseResult, Parser, Stream};

impl<S: Stream, P0, P1> Parser for (P0, P1)
where
    P0: Parser<Stream = S>,
    P1: Parser<Stream = S>,
{
    type Stream = S;
    type Output = (P0::Output, P1::Output);

    fn parse_partial(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.0.parse_partial(stream)? {
            (Some(r0), stream) => match self.1.parse_partial(stream)? {
                (Some(r1), stream) => stream.ok((r0, r1)),
                (None, stream) => {
                    let mut errors = stream.new_errors();
                    self.1.add_expected_error(&mut errors);
                    stream.errs(errors)
                }
            },
            (None, stream) => {
                let mut errors = stream.new_errors();
                self.0.add_expected_error(&mut errors);
                stream.errs(errors)
            }
        }
    }
}

impl<S: Stream, P0, P1, P2> Parser for (P0, P1, P2)
where
    P0: Parser<Stream = S>,
    P1: Parser<Stream = S>,
    P2: Parser<Stream = S>,
{
    type Stream = S;
    type Output = (P0::Output, P1::Output, P2::Output);

    fn parse_partial(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.0.parse_partial(stream)? {
            (Some(r0), stream) => match self.1.parse_partial(stream)? {
                (Some(r1), stream) => match self.2.parse_partial(stream)? {
                    (Some(r2), stream) => stream.ok((r0, r1, r2)),
                    (None, stream) => {
                        let mut errors = stream.new_errors();
                        self.2.add_expected_error(&mut errors);
                        stream.errs(errors)
                    }
                },
                (None, stream) => {
                    let mut errors = stream.new_errors();
                    self.1.add_expected_error(&mut errors);
                    stream.errs(errors)
                }
            },
            (None, stream) => {
                let mut errors = stream.new_errors();
                self.0.add_expected_error(&mut errors);
                stream.errs(errors)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use parser::repeat::many1;
    use parser::token::{ascii, token};
    use stream::IndexedStream;

    #[test]
    fn test_2_tuple() {
        let mut parser = (token(b'a'), token(b'b'));
        test_parser!(IndexedStream<&str> => (char, char) | parser, {
            "abcd" => ok(('a', 'b'), ("cd", 2)),
            "ab" => ok(('a', 'b'), ("", 2)),
            "def" => err(0, vec![Error::unexpected_token('d'), Error::expected(b'a')]),
            "aab" => err(1, vec![Error::unexpected_token('a'), Error::expected(b'b')]),
            "bcd" => err(0, vec![Error::unexpected_token('b'), Error::expected(b'a')]),
        });

        let mut parser = (many1(ascii::digit()), many1(ascii::letter()));
        test_parser!(IndexedStream<&str> => (Vec<char>, Vec<char>) | parser, {
            "123abc456" => ok((vec!['1', '2', '3'], vec!['a', 'b', 'c']), ("456", 6)),
            " 1 2 3" => err(0, vec![
                Error::unexpected_token(' '),
                Error::expected("an ascii digit"),
            ]),
            "123 abc" => err(3, vec![
                Error::unexpected_token(' '),
                Error::expected("an ascii letter"),
            ]),
        });
    }

    #[test]
    fn test_3_tuple() {
        let mut parser = (token(b'a'), token(b'b'), token(b'c'));
        test_parser!(IndexedStream<&str> => (char, char, char) | parser, {
            "abcd" => ok(('a', 'b', 'c'), ("d", 3)),
            "abc" => ok(('a', 'b', 'c'), ("", 3)),
            "def" => err(0, vec![Error::unexpected_token('d'), Error::expected(b'a')]),
            "abb" => err(2, vec![Error::unexpected_token('b'), Error::expected(b'c')]),
        });

        let mut parser = (
            many1(ascii::digit()),
            many1(ascii::letter()),
            many1(ascii::whitespace()),
        );
        test_parser!(IndexedStream<&str> => (Vec<char>, Vec<char>, Vec<char>) | parser, {
            "123abc 456" => ok(
                ("123".chars().collect(), "abc".chars().collect(), vec![' ']),
                ("456", 7)
            ),
            " 1 2 3" => err(0, vec![
                Error::unexpected_token(' '),
                Error::expected("an ascii digit"),
            ]),
            "123abc456" => err(6, vec![
                Error::unexpected_token('4'),
                Error::expected("an ascii whitespace character"),
            ]),
        });
    }
}
