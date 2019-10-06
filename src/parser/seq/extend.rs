use std::iter;

use error::ParseResult;
use parser::Parser;
use stream::Stream;

pub struct Extend<L, R> {
    p1: L,
    p2: R,
}

impl<O, L, R> Parser for Extend<L, R>
where
    L: Parser,
    L::Output: iter::Extend<O> + Default + iter::IntoIterator<Item = O>,
    R: Parser<Stream = L::Stream, Output = L::Output>,
{
    type Stream = L::Stream;
    type Output = L::Output;

    fn parse_partial(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let mut output: Self::Output = Self::Output::default();
        let stream = match self.p1.parse_partial(stream)? {
            (Some(p1), stream) => {
                output.extend::<L::Output>(p1);
                stream
            }
            (None, stream) => stream,
        };
        let stream = match self.p2.parse_partial(stream)? {
            (Some(p2), stream) => {
                output.extend::<R::Output>(p2);
                stream
            }
            (None, stream) => stream,
        };
        stream.ok(output)
    }
}

pub fn extend<O, L, R>(p1: L, p2: R) -> Extend<L, R>
where
    L: Parser,
    L::Output: iter::Extend<O> + Default + iter::IntoIterator<Item = O>,
    R: Parser<Stream = L::Stream, Output = L::Output>,
{
    Extend { p1, p2 }
}

#[macro_export]
macro_rules! concat {
    ($head:expr) => { $head };
    ($head:expr, $($tail:expr),+ $(,)*) => {
        $head.extend(concat![$($tail),+])
    };
}

#[cfg(test)]
mod test {
    use super::*;
    use error::Error;
    use parser::item::ascii;
    use parser::item::item;
    use parser::repeat::{many, many1};
    use parser::Parser;
    use stream::IndexedStream;

    #[test]
    fn test_extend() {
        let mut parser = extend(many(ascii::letter()), many1(item(b'?')));
        test_parser!(IndexedStream<&str> => Vec<char> | parser, {
            "huh???" => ok("huh???".chars().collect(), ("", 6)),
            "oh?? cool" => ok("oh??".chars().collect(), (" cool", 4)),
            "???" => ok("???".chars().collect(), ("", 3)),
            "" => err(Error::eoi().expected_item('?').at(0)),
            "whoops!" => err(Error::item('!').expected_item('?').at(6)),
            "!?" => err(Error::item('!').expected_item('?').at(0)),
        });
    }

    #[test]
    fn test_concat_macro() {
        let mut parser = concat![
            many(ascii::whitespace()),
            ascii::letter().then(ascii::alpha_num()),
            many(ascii::alpha_num()),
        ]
        .collect();

        test_parser!(IndexedStream<&str> => String | parser, {
            "x9" => ok("x9".to_string(), ("", 2)),
            "t1t3 man" => ok("t1t3".to_string(), (" man", 4)),
            "  xs = [2, 3]" => ok("  xs".to_string(), (" = [2, 3]", 4)),
            "" => err(Error::eoi().expected("an ascii letter").at(0)),
            "?" => err(Error::item('?').expected("an ascii letter").at(0)),
            "a" => err(Error::eoi().expected("an ascii letter or digit").at(1)),
            "a?" => err(Error::item('?').expected("an ascii letter or digit").at(1)),
        });
    }
}
