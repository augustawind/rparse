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
                (Err(err), stream) => stream.err(err),
            },
            (Err(err), stream) => stream.err(err),
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
        // TODO: errors should show where the error occured
        test_parser!(IndexedStream<&str> | token('X').then::<_, String>(token('O')), {
            "XO" => (Ok("XO".to_string()), "", 2),
            "XOXO" => (Ok("XO".to_string()), "XO", 2),
            "XY" => (Err(Error::expected_token('O')), "XY", 0),
            "ZY" => (Err(Error::expected_token('X')), "ZY", 0),
        });
    }
}
