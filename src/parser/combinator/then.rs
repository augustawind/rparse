use std::iter;
use std::marker::PhantomData;

use error::ParseResult;
use parser::Parser;
use stream::Stream;

pub struct Then<L, R, I> {
    left: L,
    right: R,
    __marker: PhantomData<I>,
}

impl<S: Stream, O, I, L, R> Parser for Then<L, R, I>
where
    L: Parser<Stream = S, Output = O>,
    R: Parser<Stream = S, Output = O>,
    I: iter::FromIterator<O>,
{
    type Stream = S;
    type Output = I;

    fn parse_stream(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.left.parse(stream) {
            (Ok(first), stream) => match self.right.parse(stream) {
                (Ok(second), stream) => {
                    let result: I = iter::once(first).chain(iter::once(second)).collect();
                    stream.ok(result)
                }
                (Err(err), stream) => stream.errs(err),
            },
            (Err(err), stream) => stream.errs(err),
        }
    }
}

pub fn then<S: Stream, O, I, L, R>(left: L, right: R) -> Then<L, R, I>
where
    L: Parser<Stream = S, Output = O>,
    R: Parser<Stream = S, Output = O>,
    I: iter::FromIterator<O>,
{
    Then {
        left,
        right,
        __marker: PhantomData,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use error::Error;
    use parser::token::token;
    use stream::IndexedStream;

    #[test]
    fn test_then() {
        let mut parser = token('X').then::<_, String>(token('O'));
        test_parser!(IndexedStream<&str> | parser, {
            "XO" => (Ok("XO".to_string()), "", 2),
            "XOXO" => (Ok("XO".to_string()), "XO", 2),
        });
        test_parser_errors!(IndexedStream<&str> | parser, {
            "XY" => at 1; (|&err| err == Error::expected_token('O')),
            "ZY" => at 0; (|&err| err == Error::expected_token('X')),
        });
    }
}
