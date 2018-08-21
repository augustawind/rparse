pub mod token;

use std::iter::FromIterator;
use std::marker::PhantomData;

use parser::{Error, Info, Input, ParseResult, Parser};

pub struct TokenCond<I: Input, F>(F, PhantomData<Fn(&I::Item) -> bool>);

impl<I: Input, F> Parser for TokenCond<I, F>
where
    F: Fn(&I::Item) -> bool,
{
    type Input = I;
    type Output = I::Item;

    fn parse(&mut self, mut input: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        match input.peek() {
            Some(ref t) if self.0(t) => {
                input.pop();
                input.ok(*t)
            }
            Some(ref t) => input.err(Error::Unexpected(Info::Token(*t))),
            None => input.err(Error::Expected(Info::AnyToken)),
        }
    }
}

pub fn token_if<I: Input, F>(f: F) -> TokenCond<I, F>
where
    F: Fn(&I::Item) -> bool,
{
    TokenCond(f, PhantomData)
}

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

    #[test]
    fn test_token_if() {
        let input = "123abc";
        assert_eq!(
            token_if(|&c: &char| c.is_numeric()).parse(input),
            (Ok('1'), "23abc")
        );
        let input = "123abc";
        assert_eq!(
            token_if(|&c: &char| c.is_alphabetic()).parse(input).1,
            input
        );
    }

    #[test]
    fn test_many() {
        let input = "aaabcdef";
        assert_eq!(
            many(token::token('a')).parse(input),
            (Ok("aaa".to_string()), "bcdef")
        );
        assert_eq!(
            many(token::token('a')).parse(input),
            (Ok(vec!['a', 'a', 'a']), "bcdef")
        );
    }
}
