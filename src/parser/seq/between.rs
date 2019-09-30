use crate::{ParseResult, Parser};

pub struct Between<P, L, R> {
    p: P,
    left: L,
    right: R,
}

impl<P, L, R> Parser for Between<P, L, R>
where
    P: Parser,
    L: Parser<Stream = P::Stream>,
    R: Parser<Stream = P::Stream>,
{
    type Stream = P::Stream;
    type Output = P::Output;

    fn parse_partial(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        (self.left.by_ref(), self.p.by_ref(), self.right.by_ref())
            .map(|(_, result, _)| result)
            .parse_partial(stream)
    }
}

fn between<P, L, R>(left: L, right: R, p: P) -> Between<P, L, R>
where
    P: Parser,
    L: Parser<Stream = P::Stream>,
    R: Parser<Stream = P::Stream>,
{
    Between { p, left, right }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::Error;
    use crate::parser::{
        item::{item, ascii},
        repeat::many1,
    };
    use crate::stream::IndexedStream;

    #[test]
    fn test_between() {
        let mut parser = between(item(b'<'), item(b'>'), many1(ascii::alpha_num()));
        test_parser!(IndexedStream<&str> => String | parser, {
            r"<y2k>" => ok("y2k".to_string(), ("", 5)),
            r"<5>..." => ok("5".to_string(), ("...", 3)),
            r"" => err(Error::eoi().expected_item('<').at(0)),
            r"5>" => err(Error::item('5').expected_item('<').at(0)),
            r"<5" => err(Error::eoi().expected_item('>').at(2)),
            r"< 5>" => err(Error::item(' ').expected("an ascii letter or digit").at(1)),
        });
    }
}