use std::fmt::Debug;
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

/// Parses a digit according to [`std::char::is_digit`].
///
/// [`std::char::is_digit`]: https://doc.rust-lang.org/std/primitive.char.html#method.is_alphabetic
pub fn digit<I, T>() -> Cond<I, fn(&I::Item) -> bool>
where
    I: Input<Item = T>,
    T: Copy + Debug + Into<char>,
{
    Cond(|&c: &T| c.into().is_ascii_digit(), PhantomData)
}

/// Parses an alphabetic code point.
pub fn letter<I, T>() -> Cond<I, fn(&I::Item) -> bool>
where
    I: Input<Item = T>,
    T: Copy + Debug + Into<char>,
{
    Cond(|&c: &T| c.into().is_alphabetic(), PhantomData)
}

/// Parses a numeric code point.
pub fn numeric<I, T>() -> Cond<I, fn(&I::Item) -> bool>
where
    I: Input<Item = T>,
    T: Copy + Debug + Into<char>,
{
    Cond(|&c: &T| c.into().is_numeric(), PhantomData)
}

/// Parses an alphabetic or numeric code point.
pub fn alpha_num<I, T>() -> Cond<I, fn(&I::Item) -> bool>
where
    I: Input<Item = T>,
    T: Copy + Debug + Into<char>,
{
    Cond(|&c: &T| c.into().is_alphanumeric(), PhantomData)
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
        assert_eq!(
            cond(|&c: &char| c.is_numeric()).parse("123abc"),
            (Ok('1'), "23abc")
        );
        assert_eq!(
            cond(|&c: &char| c.is_alphabetic()).parse("123abc"),
            (Err(Error::Unexpected(Info::Token('1'))), "123abc")
        );
    }

    #[test]
    fn test_digit() {
        assert_eq!(digit().parse("1ab23"), (Ok('1'), "ab23"));
        assert_eq!(
            digit().parse("a1b23"),
            (Err(Error::Unexpected(Info::Token('a'))), "a1b23")
        );
    }
}
