use error::ParseResult;
use parser::Parser;
use stream::Stream;

pub struct Append<L, R> {
    p1: L,
    p2: R,
}

impl<L, R> Parser for Append<L, R>
where
    L: Parser<Stream = R::Stream, Output = Vec<R::Output>>,
    R: Parser,
{
    type Stream = L::Stream;
    type Output = L::Output;

    fn parse_partial(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.p1.parse_partial(stream)? {
            (Some(mut p1), stream) => {
                let (p2, stream) = self.p2.parse_partial(stream)?;
                if let Some(r) = p2 {
                    p1.push(r);
                }
                stream.ok(p1)
            }
            (None, stream) => stream.noop(),
        }
    }
}

pub fn append<L, R>(p1: L, p2: R) -> Append<L, R>
where
    L: Parser<Stream = R::Stream, Output = Vec<R::Output>>,
    R: Parser,
{
    Append { p1, p2 }
}

// Create a parser which runs a series of subparsers in sequence, returning the collected output.
// This is basically just syntactic sugar for chaining parsers with `append`.
#[macro_export]
macro_rules! seq {
    ($init:expr, $head:expr $(,)*) => {
        $init.then::<Vec<_>, _>($head)
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
    use parser::item::ascii::{hexdigit, letter};
    use parser::item::item;
    use parser::repeat::many;
    use parser::Parser;
    use stream::IndexedStream;

    #[test]
    fn test_append() {
        let mut parser = append(many(letter()), item(b'?'));
        test_parser!(IndexedStream<&str> => Vec<char> | parser, {
            "huh?" => ok("huh?".chars().collect(), ("", 4)),
            "oh? cool" => ok("oh?".chars().collect(), (" cool", 3)),
            "???" => ok("?".chars().collect(), ("??", 1)),
            "" => err(Error::eoi().expected_item('?').at(0)),
            "" => err(Error::eoi().expected_item('?').at(0)),
            "whoops!" => err(Error::item('!').expected_item('?').at(6)),
            "!?" => err(Error::item('!').expected_item('?').at(0)),
        });
    }

    #[test]
    fn test_seq_macro() {
        let mut parser = seq![item(b'%'), hexdigit(), hexdigit()].collect();
        test_parser!(IndexedStream<&str> => String | parser, {
            "%AF" => ok("%AF".into(), ("", 3)),
            "%d8_/^/_" => ok("%d8".into(), ("_/^/_", 3)),
            "" => err(Error::eoi().expected_item('%').at(0)),
            "%0" => err(Error::eoi().expected("a hexadecimal digit").at(2)),
            "%zz" => err(Error::item('z').expected("a hexadecimal digit").at(1)),
        });

        let mut parser = seq![item(b'x'), item(b'y')].collect();
        test_parser!(IndexedStream<&str> => String | parser, {
            "xy" => ok("xy".into(), ("", 2)),
        });

        let mut parser = seq![item(b'a'), item(b'b'), item(b'c'), item(b'd')].collect();
        test_parser!(IndexedStream<&str> => String | parser, {
            "abcd" => ok("abcd".into(), ("", 4)),
        });
    }
}
