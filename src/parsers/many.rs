use std::iter::FromIterator;
use std::marker::PhantomData;

use parser::{Error, ParseResult, Parser};

#[derive(Copy, Clone)]
pub struct Many<P, O> {
    p: P,
    min: usize,
    _marker: PhantomData<O>,
}

impl<P, O> Parser for Many<P, O>
where
    P: Parser,
    O: FromIterator<P::Output>,
{
    type Input = P::Input;
    type Output = O;

    fn parse(&mut self, mut input: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        let mut output = Vec::new();
        let mut i = 0;
        loop {
            match self.p.parse(input) {
                (Ok(result), remaining) => {
                    output.push(result);
                    input = remaining;
                }
                (Err(Error::Expected(info)), remaining) => {
                    if i < self.min {
                        break (Err(Error::Expected(info)), remaining);
                    }
                    input = remaining;
                    break (Ok(output.into_iter().collect()), input);
                }
                (Err(err), remaining) => {
                    break (Err(err), remaining);
                }
            }

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
        _marker: PhantomData,
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
        _marker: PhantomData,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use parser::Info;
    use parsers::token::*;

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
        assert_eq!(many(token('a')).parse("aaaa"), (Ok("aaaa".to_string()), ""))
    }

    #[test]
    fn test_many1() {
        assert_eq!(
            many1(token('a')).parse("aaabcd"),
            (Ok("aaa".to_string()), "bcd")
        );
        assert_eq!(
            many1(token('a')).parse("abcd"),
            (Ok("a".to_string()), "bcd")
        );
        assert_eq!(
            many1(token('a')).parse("aaaa"),
            (Ok(vec!['a', 'a', 'a', 'a']), "")
        );
        assert_eq!(
            many1(token('b')).parse("abcd"),
            (Err(Error::Expected(Info::Token('b'))), "abcd") as ParseResult<&str, String>
        );
    }
}
