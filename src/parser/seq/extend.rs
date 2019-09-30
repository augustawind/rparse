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
    L::Output: iter::Extend<O>,
    R: Parser<Stream = L::Stream>,
    R::Output: iter::IntoIterator<Item = O>,
{
    type Stream = L::Stream;
    type Output = L::Output;

    fn parse_partial(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.p1.parse_partial(stream)? {
            (Some(mut p1), stream) => {
                let (p2, stream) = self.p2.parse_partial(stream)?;
                if let Some(r) = p2 {
                    p1.extend(r.into_iter());
                }
                stream.ok(p1)
            }
            (None, stream) => stream.noop(),
        }
    }
}

pub fn extend<S: Stream, O, L, R>(p1: L, p2: R) -> Extend<L, R>
where
    L: Parser<Stream = S, Output = Vec<O>>,
    R: Parser<Stream = S, Output = Vec<O>>,
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
    use parser::item::token;
    use parser::repeat::{many, many1};
    use parser::Parser;
    use stream::IndexedStream;
    use Error::*;

    #[test]
    fn test_extend() {
        let mut parser = extend(many(ascii::letter()), many1(token(b'?')));
        test_parser!(IndexedStream<&str> => Vec<char> | parser, {
            "huh???" => ok("huh???".chars().collect(), ("", 6)),
            "oh?? cool" => ok("oh??".chars().collect(), (" cool", 4)),
            "???" => ok("???".chars().collect(), ("", 3)),
            "" => err(0, vec![Error::eoi(), Error::expected_token('?')]),
            "whoops!" => err(6, vec![Error::unexpected_token('!'), Error::expected_token('?')]),
            "!?" => err(0, vec![Error::unexpected_token('!'), Error::expected_token('?')]),
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
            "" => err(0, vec![
                Error::eoi(),
                Error::expected("an ascii letter"),
            ]),
            "?" => err(0, vec![
                Unexpected('?'.into()),
                Error::expected("an ascii letter"),
            ]),
            "a" => err(1, vec![
                Error::eoi(),
                Error::expected("an ascii letter or digit"),
            ]),
            "a?" => err(1, vec![
                Unexpected('?'.into()),
                Error::expected("an ascii letter or digit"),
            ]),
        });
    }
}
