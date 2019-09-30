use crate::{Expected, ParseResult, Parser, Stream};

pub struct And<L, R> {
    p1: L,
    p2: R,
}

impl<S: Stream, L, R> Parser for And<L, R>
where
    L: Parser<Stream = S>,
    R: Parser<Stream = S>,
{
    type Stream = S;
    type Output = (L::Output, R::Output);

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let (left, stream) = match self.p1.parse_lazy(stream)? {
            (Some(result), stream) => (result, stream),
            (None, stream) => return stream.noop(),
        };
        let (right, stream) = match self.p2.parse_lazy(stream)? {
            (Some(result), stream) => (result, stream),
            (None, stream) => return stream.noop(),
        };
        stream.ok((left, right))
    }

    fn expected_error(&self) -> Option<Expected<Self::Stream>> {
        Expected::merge_seq(vec![self.p1.expected_error(), self.p2.expected_error()])
    }
}

/// Equivalent to [`p1.and(p2)`].
///
/// [`p1.and(p2)`]: Parser::and
pub fn and<S: Stream, O, L, R>(p1: L, p2: R) -> And<L, R>
where
    L: Parser<Stream = S>,
    R: Parser<Stream = S, Output = O>,
{
    And { p1, p2 }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::{
        item::{ascii, item},
        repeat::many1,
    };
    use crate::stream::IndexedStream;
    use crate::Error;

    #[test]
    fn test_and() {
        let mut parser = and(item(b'a'), item(b'b'));
        test_parser!(IndexedStream<&str> => (char, char) | parser, {
            "abcd" => ok(('a', 'b'), ("cd", 2)),
            "ab" => ok(('a', 'b'), ("", 2)),
            "def" => err(Error::item('d').expected(vec![b'a', b'b']).at(0)),
            "aab" => err(Error::item('a').expected(vec![b'a', b'b']).at(1)),
            "bcd" => err(Error::item('b').expected(vec![b'a', b'b']).at(0)),
        });

        let mut parser = and(many1(ascii::digit()), many1(ascii::letter()));
        let into_expected = vec!["an ascii digit", "an ascii letter"];
        test_parser!(IndexedStream<&str> => (Vec<char>, Vec<char>) | parser, {
            "123abc456" => ok((vec!['1', '2', '3'], vec!['a', 'b', 'c']), ("456", 6)),
            " 1 2 3" => err(Error::item(' ').expected(into_expected.clone()).at(0)),
            "123 abc" => err(Error::item(' ').expected(into_expected.clone()).at(3)),
        });
    }
}
