use error::Errors;
use error::ParseResult;
use parser::Parser;
use stream::Stream;

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
                (Ok(result), stream) => {
                    output.push(result);
                    stream
                }
                (Err(errors), stream) => {
                    if i < self.min {
                        return stream.errs(errors);
                    }
                    return stream.ok(output);
                }
            };

            i += 1;
        }
    }

    fn add_expected_error(&self, errors: &mut Errors<Self::Stream>) {
        self.p.add_expected_error(errors)
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
    use parser::token::token;
    use stream::IndexedStream;

    #[test]
    fn test_many() {
        assert_eq!(
            many(token(b'a')).parse("aaabcd"),
            (Ok("aaa".chars().collect()), "bcd")
        );
        assert_eq!(
            many(token(b'a')).parse("aaabcd"),
            (Ok(vec!['a', 'a', 'a']), "bcd")
        );
        assert_eq!(
            many(token(b'b')).parse("abcd"),
            (Ok("".chars().collect()), "abcd")
        );
        assert_eq!(
            many(token(b'a')).parse("aaaa"),
            (Ok("aaaa".chars().collect()), "")
        );
        assert_eq!(
            many(many1(token(b'a'))).parse("aaabcd"),
            (Ok(vec!["aaa".chars().collect()]), "bcd")
        );
        assert_eq!(
            many(many1(token(b'b'))).parse("aaabcd"),
            (Ok(Vec::<Vec<char>>::new()), "aaabcd")
        );
    }

    #[test]
    fn test_many1() {
        let mut parser = many1(token(b'a'));
        test_parser!(IndexedStream<&str> | parser, {
            "aaabcd" => (Ok("aaa".chars().collect()), ("bcd", 3));
            "abcd" => (Ok("a".chars().collect()), ("bcd", 1));
            "aaaa" => (Ok("aaaa".chars().collect()), ("", 4));
        }, {
            "baaa" => (0, vec![Error::unexpected_token('b'), Error::expected_token('a')]);
        });
    }
}
