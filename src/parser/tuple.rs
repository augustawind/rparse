use error::ParseResult;
use parser::Parser;
use stream::Stream;

impl<S: Stream, P0, P1> Parser for (P0, P1)
where
    P0: Parser<Stream = S>,
    P1: Parser<Stream = S>,
{
    type Stream = S;
    type Output = (P0::Output, P1::Output);

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.0.parse_partial(stream) {
            (Ok(r0), stream) => match self.1.parse_partial(stream) {
                (Ok(r1), stream) => stream.ok((r0, r1)),
                (Err(err), stream) => stream.errs(err),
            },
            (Err(err), stream) => stream.errs(err),
        }
    }
}
