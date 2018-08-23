//! A collection of various parsers and combinators.
//!
//! Defines the `Parser` trait.

#[cfg(test)]
macro_rules! assert_parse_err {
    ($result:expr, $input:expr) => {
        let (result, input): ParseResult<_, _> = $result;
        assert!(result.is_err());
        assert_eq!(input, $input);
    };
}

pub mod combinator;
pub mod token;
pub mod transform;

use self::combinator::{or, Or};
use self::transform::{bind, map, Bind, Map};
use error::ParseResult;
use input::Input;

pub trait Parser {
    type Input: Input;
    type Output;

    fn parse_input(&mut self, Self::Input) -> ParseResult<Self::Input, Self::Output>;

    fn parse(&mut self, input: Self::Input) -> ParseResult<Self::Input, Self::Output>
    where
        Self: Sized,
    {
        let backup = input.backup();
        let mut result = self.parse_input(input);
        if let (Err(_), ref mut input) = result {
            input.restore(backup);
        }
        result
    }

    fn map<F, O>(self, f: F) -> Map<Self, F>
    where
        Self: Sized,
        F: Fn(Self::Output) -> O,
    {
        map(self, f)
    }

    fn bind<F, O>(self, f: F) -> Bind<Self, F>
    where
        Self: Sized,
        F: Fn(Self::Output, Self::Input) -> O,
    {
        bind(self, f)
    }

    fn or<P>(self, other: P) -> Or<Self, P>
    where
        Self: Sized,
        P: Parser<Input = Self::Input, Output = Self::Output>,
    {
        or(self, other)
    }
}

impl<'a, I: Input, O> Parser for FnMut(I) -> ParseResult<I, O> + 'a {
    type Input = I;
    type Output = O;

    fn parse_input(&mut self, input: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        self(input)
    }
}

impl<I: Input, O> Parser for fn(I) -> ParseResult<I, O> {
    type Input = I;
    type Output = O;

    fn parse_input(&mut self, input: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        self(input)
    }
}
