use std::iter;

use error::ParseResult;
use parser::Parser;
use stream::Stream;

pub struct Extend<L, R> {
    left: L,
    right: R,
}

impl<S: Stream, O, L, R> Parser for Extend<L, R>
where
    L: Parser<Stream = S>,
    L::Output: iter::Extend<O>,
    R: Parser<Stream = S>,
    R::Output: iter::IntoIterator<Item = O>,
{
    type Stream = S;
    type Output = L::Output;

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.left.parse_partial(stream) {
            (Ok(mut left), stream) => match self.right.parse_partial(stream) {
                (Ok(right), stream) => {
                    left.extend(right.into_iter());
                    stream.ok(left)
                }
                (Err(err), stream) => stream.errs(err),
            },
            (Err(err), stream) => stream.errs(err),
        }
    }
}

pub fn extend<S: Stream, O, L, R>(left: L, right: R) -> Extend<L, R>
where
    L: Parser<Stream = S, Output = Vec<O>>,
    R: Parser<Stream = S, Output = Vec<O>>,
{
    Extend { left, right }
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
    use parser::seq::{many, many1};
    use parser::token::ascii;
    use parser::token::token;
    use parser::Parser;
    use stream::IndexedStream;
    use Error::*;

    #[test]
    fn test_extend() {
        let mut parser = extend(many(ascii::letter()), many1(token(b'?')));
        test_parser!(IndexedStream<&str> | parser, {
            "huh???" => (Ok("huh???".chars().collect()), ("", 6));
            "oh?? cool" => (Ok("oh??".chars().collect()), (" cool", 4));
            "???" => (Ok("???".chars().collect()), ("", 3));
        }, {
            "" => (0, vec![Error::unexpected_eoi(), Error::expected_token('?')]);
            "whoops!" => (6, vec![Error::unexpected_token('!'), Error::expected_token('?')]);
            "!?" => (0, vec![Error::unexpected_token('!'), Error::expected_token('?')]);
        });
    }

    #[test]
    fn test_concat_macro() {
        let mut parser = concat![
            many(ascii::whitespace()),
            ascii::letter().then(ascii::alpha_num()),
            many(ascii::alpha_num()),
        ].collect();

        test_parser!(IndexedStream<&str> => String | parser, {
            "x9" => ok(Ok("x9".to_string()), ("", 2)),
            "t1t3 man" => ok(Ok("t1t3".to_string()), (" man", 4)),
            "  xs = [2, 3]" => ok(Ok("  xs".to_string()), (" = [2, 3]", 4)),
            "" => err(0, vec![
                Error::unexpected_eoi(), 
                Expected("an ascii letter".into()),
            ]),
            "?" => err(0, vec![
                Unexpected('?'.into()),
                Expected("an ascii letter".into()),
            ]),
            "a" => err(1, vec![
                Error::unexpected_eoi(), 
                Expected("an ascii letter or digit".into()),
            ]),
            "a?" => err(1, vec![
                Unexpected('?'.into()),
                Expected("an ascii letter or digit".into()),
            ]),
        });
    }
}
