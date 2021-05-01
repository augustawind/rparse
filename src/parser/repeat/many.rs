use std::marker::PhantomData;

use crate::error::Expected;
use {ParseResult, Parser, Stream};

pub struct Many<O, P> {
    p: P,
    min: usize,
    max: Option<usize>,
    _marker: PhantomData<O>,
}

impl<O, P> Parser for Many<O, P>
where
    P: Parser,
    O: Extend<P::Output> + Default,
{
    type Stream = P::Stream;
    type Output = O;

    fn parse_lazy(&mut self, mut stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let mut output = O::default();
        let mut i = 0;
        loop {
            if let Some(max) = self.max {
                if i == max {
                    return stream.ok(output);
                }
            }

            stream = match self.p.try_parse_lazy(stream) {
                Ok((Some(result), stream)) => {
                    output.extend(std::iter::once(result));
                    stream
                }
                Ok((None, stream)) => stream,
                Err((error, stream)) => {
                    if i < self.min {
                        return stream.err(error);
                    }
                    return stream.ok(output);
                }
            };

            i += 1;
        }
    }

    fn expected_error(&self) -> Option<Expected<Self::Stream>> {
        self.p.expected_error()
    }
}

pub fn many<O, P>(p: P) -> Many<O, P>
where
    P: Parser,
    O: Extend<P::Output> + Default,
{
    Many {
        p,
        min: 0,
        max: None,
        _marker: PhantomData,
    }
}

pub fn many1<O, P>(p: P) -> Many<O, P>
where
    P: Parser,
    O: Extend<P::Output> + Default,
{
    Many {
        p,
        min: 1,
        max: None,
        _marker: PhantomData,
    }
}

pub fn many_n<O, P>(p: P, min: usize) -> Many<O, P>
where
    P: Parser,
    O: Extend<P::Output> + Default,
{
    Many {
        p,
        min,
        max: None,
        _marker: PhantomData,
    }
}

pub fn many_n_m<O, P>(p: P, min: usize, max: usize) -> Many<O, P>
where
    P: Parser,
    O: Extend<P::Output> + Default,
{
    Many {
        p,
        min,
        max: Some(max),
        _marker: PhantomData,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use error::Error;
    use parser::item::item;
    use parser::test_utils::*;
    use stream::IndexedStream;

    #[test]
    fn test_many() {
        assert_eq!(
            many(item(b'a')).parse("aaabcd"),
            ok_result("aaa".to_string(), "bcd")
        );
        assert_eq!(
            many(item(b'a')).parse("aaabcd"),
            ok_result(vec!['a', 'a', 'a'], "bcd")
        );
        assert_eq!(
            many(item(b'b')).parse("abcd"),
            ok_result("".to_string(), "abcd")
        );
        assert_eq!(
            many(item(b'a')).parse("aaaa"),
            ok_result("aaaa".to_string(), "")
        );
        assert_eq!(
            many(many1(item(b'a'))).parse("aaabcd"),
            ok_result(vec!["aaa".to_string()], "bcd")
        );
        assert_eq!(
            many(many1(item(b'b'))).parse("aaabcd"),
            ok_result(Vec::<Vec<char>>::new(), "aaabcd")
        );
    }

    #[test]
    fn test_many1() {
        let mut parser = many1::<String, _>(item(b'a'));
        test_parser!(IndexedStream<&str> => String | parser, {
            "aaabcd" => ok("aaa".into(), ("bcd", 3)),
            "abcd" => ok("a".into(), ("bcd", 1)),
            "aaaa" => ok("aaaa".into(), ("", 4)),
            "baaa" => err(Error::item('b').expected_item('a').at(0)),
        });
    }
}
