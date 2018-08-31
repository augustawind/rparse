use std::iter::FromIterator;
use std::marker::PhantomData;

use error::ParseResult;
use parser::Parser;
use stream::Stream;

pub struct Many<P, O> {
    p: P,
    min: usize,
    __marker: PhantomData<O>,
}

impl<P, O> Parser for Many<P, O>
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
        let mut output = Vec::new();
        let mut i = 0;
        loop {
            stream = match self.p.parse_stream(stream) {
                (Ok(result), stream) => {
                    output.push(result);
                    stream
                }
                (Err(errors), stream) => {
                    if i < self.min {
                        return stream.errs(errors);
                    }
                    return stream.ok(output.into_iter().collect());
                }
            };

            i += 1;
        }
    }
}

pub fn many<P, O>(p: P) -> Many<P, O>
where
    P: Parser,
    O: FromIterator<P::Output>,
{
    Many {
        p,
        min: 0,
        __marker: PhantomData,
    }
}

pub fn many1<P, O>(p: P) -> Many<P, O>
where
    P: Parser,
    O: FromIterator<P::Output>,
{
    Many {
        p,
        min: 1,
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
    fn test_many() {
        assert_eq!(
            many(token('a')).parse("aaabcd"),
            (Ok("aaa".to_string()), "bcd")
        );
        assert_eq!(
            many(token('a')).parse("aaabcd"),
            (Ok(vec!['a', 'a', 'a']), "bcd")
        );
        assert_eq!(many(token('b')).parse("abcd"), (Ok("".to_string()), "abcd"));
        assert_eq!(many(token('a')).parse("aaaa"), (Ok("aaaa".to_string()), ""));
        assert_eq!(
            many(many1(token('a'))).parse("aaabcd"),
            (Ok(vec!["aaa".to_string()]), "bcd")
        );
        assert_eq!(
            many(many1(token('b'))).parse("aaabcd"),
            (Ok(Vec::<String>::new()), "aaabcd")
        );
    }

    #[test]
    fn test_many1() {
        let mut parser = many1(token('a'));
        test_parser!(IndexedStream<&str> | parser, {
            "aaabcd" => (Ok("aaa".to_string()), "bcd", 3),
            "abcd" => (Ok("a".to_string()), "bcd", 1),
            "aaaa" => (Ok("aaaa".to_string()), "", 4),
        });
        test_parser_errors!(IndexedStream<&str> | parser, {
            "baaa" => at 0; (|e| e == &Error::expected_token('a')),
        });
    }
}
