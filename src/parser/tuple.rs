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
                    let mut error = stream.new_error();
                    self.1.add_expected_error(&mut error);
                    stream.err(error)
                }
            },
            (None, stream) => {
                let mut error = stream.new_error();
                self.0.add_expected_error(&mut error);
                stream.err(error)
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
                        let mut error = stream.new_error();
                        self.0.add_expected_error(&mut error);
                        stream.err(error)
                    }
                },
                (None, stream) => {
                    let mut error = stream.new_error();
                    self.0.add_expected_error(&mut error);
                    stream.err(error)
                }
            },
            (None, stream) => {
                let mut error = stream.new_error();
                self.0.add_expected_error(&mut error);
                stream.err(error)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use error::Error;
    use parser::item::{ascii, item};
    use parser::repeat::many1;
    use stream::IndexedStream;

    #[test]
    fn test_2_tuple() {
        let mut parser = (item(b'a'), item(b'b'));
        test_parser!(IndexedStream<&str> => (char, char) | parser, {
            "abcd" => ok(('a', 'b'), ("cd", 2)),
            "ab" => ok(('a', 'b'), ("", 2)),
            "def" => err(Error::item('d').expected(b'a').at(0)),
            "aab" => err(Error::item('a').expected(b'b').at(1)),
            "bcd" => err(Error::item('b').expected(b'a').at(0)),
        });

        let mut parser = (many1(ascii::digit()), many1(ascii::letter()));
        test_parser!(IndexedStream<&str> => (Vec<char>, Vec<char>) | parser, {
            "123abc456" => ok((vec!['1', '2', '3'], vec!['a', 'b', 'c']), ("456", 6)),
            " 1 2 3" => err(Error::item(' ').expected("an ascii digit").at(0)),
            "123 abc" => err(Error::item(' ').expected("an ascii letter").at(3)),
        });
    }

    #[test]
    fn test_3_tuple() {
        let mut parser = (item(b'a'), item(b'b'), item(b'c'));
        test_parser!(IndexedStream<&str> => (char, char, char) | parser, {
            "abcd" => ok(('a', 'b', 'c'), ("d", 3)),
            "abc" => ok(('a', 'b', 'c'), ("", 3)),
            "def" => err(Error::item('d').expected(b'a').at(0)),
            "abb" => err(Error::item('b').expected(b'c').at(2)),
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
            " 1 2 3" => err(Error::item(' ').expected("an ascii digit").at(0)),
            "123abc456" => err(
                Error::item('4').expected("an ascii whitespace character").at(6)
            ),
        });
    }
}
