use std::marker::PhantomData;

use parser::{Error, Info, Input, ParseResult, Parser};

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

pub struct Cond<I: Input, F>(F, PhantomData<Fn(&I::Item) -> bool>);

impl<I: Input, F> Parser for Cond<I, F>
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

pub fn cond<I: Input, F>(f: F) -> Cond<I, F>
where
    F: Fn(&I::Item) -> bool,
{
    Cond(f, PhantomData)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_any() {
        let input = "hello, world.";
        assert_eq!(any().parse(input), (Ok('h'), "ello, world."));
    }

    #[test]
    fn test_token() {
        let mut parser = token('c');
        assert_eq!(parser.parse("cat"), (Ok('c'), "at"));
        assert_eq!(parser.parse("ace").1, "ace");
    }

    #[test]
    fn test_cond() {
        let input = "123abc";
        assert_eq!(
            cond(|&c: &char| c.is_numeric()).parse(input),
            (Ok('1'), "23abc")
        );
        let input = "123abc";
        assert_eq!(cond(|&c: &char| c.is_alphabetic()).parse(input).1, input);
    }
}
