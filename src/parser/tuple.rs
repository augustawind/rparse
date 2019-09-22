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
            Ok((Some(r0), stream)) => match self.1.parse_partial(stream) {
                Ok((Some(r1), stream)) => stream.ok((r0, r1)),
                Ok((None, stream)) => {
                    let mut errors = stream.new_errors();
                    self.1.add_expected_error(&mut errors);
                    stream.errs(errors)
                }
                Err((err, stream)) => stream.errs(err),
            },
            Ok((None, stream)) => {
                let mut errors = stream.new_errors();
                self.0.add_expected_error(&mut errors);
                stream.errs(errors)
            }
            Err((err, stream)) => stream.errs(err),
        }
    }
}

impl<S: Stream, P0, P1, P2> Parser for (P0, P1, P2)
where
    P0: Parser<Stream = S>,
    P1: Parser<Stream = S>,
    P2: Parser<Stream = S>,
{
    type Stream = S;
    type Output = (P0::Output, P1::Output, P2::Output);

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.0.parse_partial(stream) {
            Ok((Some(r0), stream)) => match self.1.parse_partial(stream) {
                Ok((Some(r1), stream)) => match self.2.parse_partial(stream) {
                    Ok((Some(r2), stream)) => stream.ok((r0, r1, r2)),
                    Ok((None, stream)) => {
                        let mut errors = stream.new_errors();
                        self.2.add_expected_error(&mut errors);
                        stream.errs(errors)
                    }
                    Err((err, stream)) => stream.errs(err),
                },
                Ok((None, stream)) => {
                    let mut errors = stream.new_errors();
                    self.1.add_expected_error(&mut errors);
                    stream.errs(errors)
                }
                Err((err, stream)) => stream.errs(err),
            },
            Ok((None, stream)) => {
                let mut errors = stream.new_errors();
                self.0.add_expected_error(&mut errors);
                stream.errs(errors)
            }
            Err((err, stream)) => stream.errs(err),
        }
    }
}
