use std::marker::PhantomData;

use parser::{Error, Input, ParseResult, Parser};

impl<I: Input, O> Parser for Fn(&mut I) -> ParseResult<I, O> {
    type Input = I;
    type Output = O;

    fn parse(&mut self, mut input: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        self(&mut input)
    }
}

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
            _ => input.err(Error::end()),
        }
    }
}

pub fn token<I: Input>(item: I::Item) -> Token<I> {
    Token(item)
}

#[derive(Copy, Clone)]
pub struct Many<P> {
    p: P,
    min: usize,
    max: Option<usize>,
}

// impl<P> Parser for Many<P>
// where
//     P: Parser,
// {
//     type Input = P::Input;
//     type Output = Vec<P::Output>;

//     fn parse(&mut self, mut i: Self::Input) -> ParseResult<Self::Input, Self::Output> {
//         let p = self.p.parse(i);
//         // TODO
//     }
// }

pub fn many<P>(p: P) -> Many<P>
where
    P: Parser,
{
    Many {
        p,
        min: 0,
        max: None,
    }
}

pub fn any<I: Input>(mut i: I) -> ParseResult<I, I::Item> {
    match i.pop() {
        Some(t) => i.ok(t),
        None => i.err(Error::end()),
    }
}

pub struct TokenCond<I: Input, F>(F, PhantomData<Fn(&I::Item) -> bool>);

impl<I: Input, F> Parser for TokenCond<I, F>
where
    F: Fn(&I::Item) -> bool,
{
    type Input = I;
    type Output = I::Item;

    fn parse(&mut self, mut i: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        match i.peek() {
            Some(ref t) if self.0(t) => {
                i.pop();
                i.ok(*t)
            }
            _ => i.err(Error::end()),
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
        assert_eq!(any(input), (Ok('h'), "ello, world."));
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
}
