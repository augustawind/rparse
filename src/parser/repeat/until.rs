use {Error, ParseResult, Parser, Stream};

pub struct TakeUntil<P> {
    p: P,
}

impl<S, P> Parser for TakeUntil<P>
where
    S: Stream,
    P: Parser<Stream = S, Output = S::Item>,
{
    type Stream = S;
    type Output = Vec<S::Item>;

    fn parse_lazy(&mut self, mut stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let mut output = Vec::new();
        loop {
            let backup = stream.backup();
            stream = match self.p.parse_lazy(stream) {
                Ok((Some(_), mut stream)) => {
                    stream.restore(backup);
                    return stream.ok(output);
                }
                Ok((None, stream)) => stream,
                Err((_, stream)) => stream,
            };
            match stream.pop() {
                Some(item) => output.push(item),
                None => return stream.err(Error::unexpected_eoi()),
            };
        }
    }

    fn expected_errors(&self) -> Vec<Error<Self::Stream>> {
        self.p.expected_errors()
    }
}

pub fn take_until<S, P>(p: P) -> TakeUntil<P>
where
    S: Stream,
    P: Parser<Stream = S, Output = S::Item>,
{
    TakeUntil { p }
}
