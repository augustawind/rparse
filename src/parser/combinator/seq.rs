use std::iter::FromIterator;
use std::marker::PhantomData;

use {ParseResult, Parser, Stream};

pub struct Seq<P: Parser, O> {
    parsers: Vec<P>,
    __marker: PhantomData<O>,
}

impl<P, O> Parser for Seq<P, O>
where
    P: Parser,
    O: FromIterator<P::Output>,
{
    type Stream = P::Stream;
    type Output = O;

    fn parse_stream(
        &mut self,
        mut stream: Self::Stream,
    ) -> ParseResult<Self::Stream, Self::Output> {
        let backup = stream.backup();
        let mut output = Vec::new();

        for parser in self.parsers.iter_mut() {
            stream = match parser.parse_stream(stream) {
                (Ok(item), stream) => {
                    output.push(item);
                    stream
                }
                (Err(errors), mut stream) => {
                    stream.restore(backup);
                    return stream.errs(errors);
                }
            }
        }

        stream.ok(output.into_iter().collect())
    }
}

pub fn seq<P, O>(parsers: Vec<P>) -> Seq<P, O>
where
    P: Parser,
    O: FromIterator<P::Output>,
{
    Seq {
        parsers,
        __marker: PhantomData,
    }
}

#[macro_export]
macro_rules! seq {
    ($($parser:expr),+) => {
        seq(vec![$($parser),+])
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use parser::token::token;
    use stream::IndexedStream;
    use Error;

    #[test]
    fn test_seq() {
        let mut parser = seq(vec![token('x'), token('y'), token('z')]);
        test_parser!(IndexedStream<&str> | parser, {
            "xyz" => (Ok("xyz".to_string()), "", 3);
            "xyzxyz" => (Ok("xyz".to_string()), "xyz", 3);
        });
        test_parser_errors!(IndexedStream<&str> | parser, {
            "" => at 0; vec![Error::EOF, Error::expected_token('x')];
            "w" => at 0; vec![Error::unexpected_token('w'), Error::expected_token('x')];
            "x" => at 1; vec![Error::EOF, Error::expected_token('y')];
            "xy" => at 2; vec![Error::EOF, Error::expected_token('z')];
            "xy3" => at 2; vec![Error::unexpected_token('3'), Error::expected_token('z')];
        });
    }
}
