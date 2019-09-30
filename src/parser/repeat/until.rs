use std::marker::PhantomData;

use crate::error::Expected;
use {Error, ParseResult, Parser, Stream};

pub struct TakeUntil<O, P, U> {
    p: P,
    until: U,
    _marker: PhantomData<O>,
}

impl<O, P, U> Parser for TakeUntil<O, P, U>
where
    O: Extend<P::Output> + Default,
    P: Parser,
    U: Parser<Stream = P::Stream>,
{
    type Stream = P::Stream;
    type Output = O;

    fn parse_lazy(&mut self, mut stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let mut output = O::default();
        loop {
            let backup = stream.backup();
            stream = match self.until.parse_lazy(stream) {
                Ok((Some(_), mut stream)) => {
                    stream.restore(backup);
                    return stream.ok(output);
                }
                Ok((None, stream)) => stream,
                Err((_, stream)) => stream,
            };
            stream.restore(backup);

            stream = match self.p.parse_lazy(stream)? {
                (Some(value), stream) => {
                    output.extend(std::iter::once(value));
                    stream
                }
                (None, stream) => return stream.err(Error::eoi()),
            };
        }
    }

    fn expected_error(&self) -> Option<Expected<Self::Stream>> {
        Expected::merge_one_of(vec![self.p.expected_error(), self.until.expected_error()])
    }
}

pub fn take_until<O, P, U>(p: P, until: U) -> TakeUntil<O, P, U>
where
    O: Extend<P::Output> + Default,
    P: Parser,
    U: Parser<Stream = P::Stream>,
{
    TakeUntil {
        p,
        until,
        _marker: PhantomData,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::error::{Error, Info};
    use crate::parser::{range::range, repeat::take_until};
    use crate::stream::IndexedStream;

    #[test]
    fn test_take_until() {
        let mut parser = take_until(range("foo"), range("foo!"));
        test_parser!(IndexedStream<&str> => String | parser, {
            "foo!" => ok("".to_string(), ("foo!", 0)),
            "foofoofoo!12" => ok("foofoo".to_string(), ("foo!12", 6)),
            "" => err(
                Error::eoi()
                    .expected_one_of(vec![
                        Info::Range("foo"),
                        Info::Range("foo!")],
                    )
                    .at(0)
            ),
            "foo" => err(
                Error::eoi()
                    .expected_one_of(vec![
                        Info::Range("foo"),
                        Info::Range("foo!"),
                    ])
                    .at(3)
            ),
        });
    }
}
