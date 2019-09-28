use std::marker::PhantomData;

use {Error, ParseResult, Parser, Stream};

pub struct SepBy<O, P, Sep> {
    p: P,
    sep: Sep,
    min: usize,
    _marker: PhantomData<O>,
}

impl<O, P, Sep> Parser for SepBy<O, P, Sep>
where
    P: Parser,
    Sep: Parser<Stream = P::Stream>,
    O: Extend<P::Output> + Default,
{
    type Stream = P::Stream;
    type Output = O;

    fn parse_lazy(&mut self, mut stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let mut output = O::default();
        let mut i = 0;
        stream = match self.p.parse_lazy(stream) {
            Ok((Some(result), stream)) => {
                output.extend(std::iter::once(result));
                stream
            },
            Ok((None, stream)) => stream,
            Err((errors, stream)) => {
                if i < self.min {
                    return stream.errs(errors);
                }
                return stream.ok(output);
            },
        };
        i += 1;

        loop {
            stream = match self.sep.by_ref().with(self.p.by_ref()).parse_lazy(stream) {
                Ok((Some(result), stream)) => {
                    output.extend(std::iter::once(result));
                    stream
                },
                Ok((None, stream)) => stream,
                Err((errors, stream)) => {
                    if i < self.min {
                        return stream.errs(errors);
                    }
                    return stream.ok(output);
                },
            };
            i += 1;
        }
    }

    fn expected_errors(&self) -> Vec<Error<Self::Stream>> {
        self.sep.expected_errors()
    }
}