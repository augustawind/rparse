use std::iter::FromIterator;
use std::marker::PhantomData;

use parser::{Error, Info, Input, ParseResult, Parser};

#[derive(Copy, Clone)]
pub struct Token<I: Input>(I::Item);

impl<I: Input> Parser for Token<I>
where
    I::Item: PartialEq,
{
    type Input = I;
    type Output = I::Item;

    fn parse(&mut self, mut input: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        match input.peek() {
            Some(item) if item == self.0 => {
                input.pop();
                input.ok(item)
            }
            _ => input.err(Error::Expected(Info::Token(self.0))),
        }
    }
}

pub fn token<I: Input>(item: I::Item) -> Token<I> {
    Token(item)
}

pub struct Any<I: Input>(PhantomData<I>);

impl<I: Input> Parser for Any<I> {
    type Input = I;
    type Output = I::Item;

    fn parse(&mut self, mut input: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        match input.pop() {
            Some(t) => input.ok(t),
            _ => input.err(Error::Expected(Info::AnyToken)),
        }
    }
}

pub fn any<I: Input>() -> Any<I> {
    Any(PhantomData)
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_token() {
        let mut parser = token('c');
        assert_eq!(parser.parse("cat"), (Ok('c'), "at"));
        assert_eq!(parser.parse("ace").1, "ace");
    }

    #[test]
    fn test_any() {
        let input = "hello, world.";
        assert_eq!(any().parse(input), (Ok('h'), "ello, world."));
    }

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
            many(token('a')).parse(input),
            (Ok("aaa".to_string()), "bcdef")
        );
        assert_eq!(
            many(token('a')).parse(input),
            (Ok(vec!['a', 'a', 'a']), "bcdef")
        );
    }
}
