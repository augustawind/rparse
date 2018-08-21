use std::iter::FromIterator;
use std::marker::PhantomData;

use parser::{Error, Info, Input, ParseResult, Parser};

#[derive(Copy, Clone)]
pub struct Many<P, O> {
    p: P,
    min: usize,
    max: Option<usize>,
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
        loop {
            match self.p.parse(input) {
                (Ok(result), remaining) => {
                    output.push(result);
                    input = remaining;
                }
                (Err(_), remaining) => {
                    return (Ok(output.into_iter().collect()), remaining);
                }
            }
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
        max: None,
        _marker: PhantomData,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use parsers::token::*;

    #[test]
    fn test_many() {
        let input = "aaabcdef";
        assert_eq!(
            many(token('a')).parse(input),
            (Ok("aaa".to_string()), "bcdef")
        );
        assert_eq!(
            many(token('a')).parse(input),
            (Ok(vec!['a', 'a', 'a']), "bcdef")
        );
    }
}
