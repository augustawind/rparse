use crate::{ParseResult, Parser};

pub struct Between<L, R, P> {
    open: L,
    close: R,
    p: P,
}

impl<L, R, P> Parser for Between<L, R, P>
where
    P: Parser,
    L: Parser<Stream = P::Stream>,
    R: Parser<Stream = P::Stream>,
{
    type Stream = P::Stream;
    type Output = P::Output;

    fn parse_partial(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        (self.open.by_ref(), self.p.by_ref(), self.close.by_ref())
            .map(|(_, result, _)| result)
            .parse_partial(stream)
    }
}

fn between<L, R, P>(open: L, close: R, p: P) -> Between<L, R, P>
where
    P: Parser,
    L: Parser<Stream = P::Stream>,
    R: Parser<Stream = P::Stream>,
{
    Between { open, close, p }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::{
        item::{ascii, item, negate},
        range::range,
        repeat::{many1, take_until},
    };
    use crate::stream::IndexedStream;
    use crate::Error;

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

    #[test]
    fn test_between_ambiguous() {
        let mut parser = between(
            range("'''"),
            range("'''"),
            take_until(negate(ascii::whitespace()), range("'''")),
        );
        test_parser!(IndexedStream<&str> => String | parser, {
            "'''1-2-3'''" => ok("1-2-3".to_string(), ("", 11)),
            "'''x'x'''" => ok("x'x".to_string(), ("", 9)),
            "'''x''x'''" => ok("x''x".to_string(), ("", 10)),
            "'''x'''x'''" => ok("x".to_string(), ("x'''", 7)),
            r"'''x''''''" => ok("x".to_string(), ("'''", 7)),
            r"''''x'''" => ok("'x".to_string(), ("", 8)),
            r"'''''x'''" => ok("''x".to_string(), ("", 9)),
            r"''''''x'''" => ok("".to_string(), ("x'''", 6)),
        });
    }
}
