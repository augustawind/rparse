use std::marker::PhantomData;

use crate::{ParseResult, Parser, Stream};

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

    fn parse_partial(
        &mut self,
        mut stream: Self::Stream,
    ) -> ParseResult<Self::Stream, Self::Output> {
        let mut output = O::default();
        let mut i = 0;
        stream = match self.p.parse_partial(stream) {
            Ok((Some(result), stream)) => {
                output.extend(std::iter::once(result));
                stream
            }
            Ok((None, stream)) => stream,
            Err((errors, stream)) => {
                if i < self.min {
                    return stream.errs(errors);
                }
                return stream.ok(output);
            }
        };
        i += 1;

        loop {
            stream = match self.sep.by_ref().with(self.p.by_ref()).parse(stream) {
                Ok((Some(result), stream)) => {
                    output.extend(std::iter::once(result));
                    stream
                }
                Ok((None, stream)) => stream,
                Err((errors, stream)) => {
                    if i < self.min {
                        return stream.errs(errors);
                    }
                    return stream.ok(output);
                }
            };
            i += 1;
        }
    }
}

pub fn sep_by<O, P, Sep>(p: P, sep: Sep) -> SepBy<O, P, Sep>
where
    P: Parser,
    Sep: Parser<Stream = P::Stream>,
    O: Extend<P::Output> + Default,
{
    SepBy {
        p,
        sep,
        min: 0,
        _marker: PhantomData,
    }
}

pub fn sep_by1<O, P, Sep>(p: P, sep: Sep) -> SepBy<O, P, Sep>
where
    P: Parser,
    Sep: Parser<Stream = P::Stream>,
    O: Extend<P::Output> + Default,
{
    SepBy {
        p,
        sep,
        min: 1,
        _marker: PhantomData,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::{
        item::{ascii, token},
        repeat::many_n,
    };
    use crate::stream::IndexedStream;
    use crate::Error;

    #[test]
    fn test_sep_by() {
        let mut parser = sep_by(many_n(ascii::letter(), 2), token(b','));
        test_parser!(IndexedStream<&str> => Vec<String> | parser, {
            "" => ok(vec![], ("", 0)),
            "," => ok(vec![], (",", 0)),
            "33a,b" => ok(vec![], ("33a,b", 0)),
            "foo" => ok(vec!["foo".to_string()], ("", 3)),
            "foo," => ok(vec!["foo".to_string()], (",", 3)),
            "foo,,bar" => ok(vec!["foo".to_string()], (",,bar", 3)),
            "foo,bar,baz" => ok(
                vec!["foo".to_string(), "bar".to_string(), "baz".to_string()],
                ("", 11),
            ),
            "foo,bar,baz," => ok(
                vec!["foo".to_string(), "bar".to_string(), "baz".to_string()],
                (",", 11),
            ),
        });
    }

    #[test]
    fn test_sep_by1() {
        let mut parser = sep_by1(many_n(ascii::letter(), 2), token(b','));
        test_parser!(IndexedStream<&str> => Vec<String> | parser, {
            "foo" => ok(vec!["foo".to_string()], ("", 3)),
            "foo," => ok(vec!["foo".to_string()], (",", 3)),
            "foo,,bar" => ok(vec!["foo".to_string()], (",,bar", 3)),
            "foo,bar,baz" => ok(
                vec!["foo".to_string(), "bar".to_string(), "baz".to_string()],
                ("", 11),
            ),
            "foo,bar,baz," => ok(
                vec!["foo".to_string(), "bar".to_string(), "baz".to_string()],
                (",", 11),
            ),
            "" => err(0, vec![Error::eoi(), Error::expected("an ascii letter")]),
            "," => err(0, vec![Error::unexpected_token(','), Error::expected("an ascii letter")]),
            "33a,b" => err(0, vec![
                Error::unexpected_token('3'),
                Error::expected("an ascii letter"),
            ]),
        });
    }
}
