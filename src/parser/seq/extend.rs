use std::iter;
use std::marker::PhantomData;

use error::ParseResult;
use parser::Parser;
use stream::Stream;

pub struct Extend<E, I, L, R> {
    left: L,
    right: R,
    __marker: PhantomData<(E, I)>,
}

impl<S: Stream, E, I, O, L, R> Parser for Extend<E, I, L, R>
where
    L: Parser<Stream = S, Output = E>,
    R: Parser<Stream = S, Output = I>,
    E: iter::Extend<O>,
    I: IntoIterator<Item = O>,
{
    type Stream = S;
    type Output = E;

    fn parse_stream(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.left.parse_stream(stream) {
            (Ok(mut left), stream) => match self.right.parse_stream(stream) {
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

pub fn extend<S: Stream, E, I, O, L, R>(left: L, right: R) -> Extend<E, I, L, R>
where
    L: Parser<Stream = S, Output = E>,
    R: Parser<Stream = S, Output = I>,
    E: iter::Extend<O>,
    I: IntoIterator<Item = O>,
{
    Extend {
        left,
        right,
        __marker: PhantomData,
    }
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
    use parser::token::ascii::letter;
    use parser::token::token;
    use parser::Parser;
    use stream::IndexedStream;

    #[test]
    fn test_extend() {
        let mut parser = extend::<_, String, Vec<char>, _, _, _>(many(letter()), many1(token('?')));
        test_parser!(IndexedStream<&str> | parser, {
            "huh???" => (Ok("huh???".to_string()), ("", 6));
            "oh?? cool" => (Ok("oh??".to_string()), (" cool", 4));
            "???" => (Ok("???".to_string()), ("", 3));
        }, {
            "" => (0, vec![Error::EOF, Error::expected_token('?')]);
            "whoops!" => (6, vec![Error::unexpected_token('!'), Error::expected_token('?')]);
            "!?" => (0, vec![Error::unexpected_token('!'), Error::expected_token('?')]);
        });
    }

    //     #[test]
    //     fn test_concat_macro() {
    //         let mut parser: Extend<String, String, _, _> = concat![
    //             many(ascii::whitespace()),
    //             ascii::letter().s(),
    //             many1(ascii::alpha_num()),
    //         ];
    //         test_parser!(IndexedStream<&str> | parser, {
    //             "x9" => (Ok("x9".to_string()), "", 2);
    //             "t1t3 man" => (Ok("t1t3".to_string()), " man", 5);
    //             "  xs = [2, 3]" => (Ok("  xs".to_string()), " = [2, 3]", 5);
    //         });
    //         // test_parser_errors!(IndexedStream<&str> | parser, {
    //         //     "" => (0, vec![Error::EOF, Error::expected_token('%')]);
    //         //     "%0" => (2, vec![Error::EOF]);
    //         //     "%zz" => (1, vec![Error::unexpected_token('z')]);
    //         // });
    //     }
}
