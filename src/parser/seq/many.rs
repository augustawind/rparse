use {Error, ParseResult, Parser, Stream};

pub struct Many<P> {
    p: P,
    min: usize,
}

impl<P> Parser for Many<P>
where
    P: Parser,
{
    type Stream = P::Stream;
    type Output = Vec<P::Output>;

    fn parse_lazy(&mut self, mut stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let mut output = Vec::new();
        let mut i = 0;
        loop {
            stream = match self.p.parse_lazy(stream) {
                Ok((Some(result), stream)) => {
                    output.push(result);
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

    fn expected_error(&self) -> Option<Error<Self::Stream>> {
        self.p.expected_error()
    }
}

pub fn many<P>(p: P) -> Many<P>
where
    P: Parser,
{
    Many { p, min: 0 }
}

pub fn many1<P>(p: P) -> Many<P>
where
    P: Parser,
{
    Many { p, min: 1 }
}

#[cfg(test)]
mod test {
    use super::*;
    use error::Error;
    use parser::test_utils::*;
    use parser::token::token;
    use stream::IndexedStream;

    #[test]
    fn test_many() {
        assert_eq!(
            many(token(b'a')).parse("aaabcd"),
            ok_result("aaa".chars().collect(), "bcd")
        );
        assert_eq!(
            many(token(b'a')).parse("aaabcd"),
            ok_result(vec!['a', 'a', 'a'], "bcd")
        );
        assert_eq!(
            many(token(b'b')).parse("abcd"),
            ok_result("".chars().collect(), "abcd")
        );
        assert_eq!(
            many(token(b'a')).parse("aaaa"),
            ok_result("aaaa".chars().collect(), "")
        );
        assert_eq!(
            many(many1(token(b'a'))).parse("aaabcd"),
            ok_result(vec!["aaa".chars().collect()], "bcd")
        );
        assert_eq!(
            many(many1(token(b'b'))).parse("aaabcd"),
            ok_result(Vec::<Vec<char>>::new(), "aaabcd")
        );
    }

    #[test]
    fn test_many1() {
        let mut parser = many1(token(b'a')).collect();
        test_parser!(IndexedStream<&str> => String | parser, {
            "aaabcd" => ok("aaa".into(), ("bcd", 3)),
            "abcd" => ok("a".into(), ("bcd", 1)),
            "aaaa" => ok("aaaa".into(), ("", 4)),
            "baaa" => err(0, vec![Error::unexpected_token('b'), Error::expected_token('a')]),
        });
    }
}
