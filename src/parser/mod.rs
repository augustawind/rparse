//! A collection of various parsers and combinators.
//!
//! Defines the `Parser` trait.

#[cfg(test)]
#[cfg_attr(rustfmt, rustfmt_skip)]
macro_rules! assert_parse_ok {
    ($parsed:expr, {
        output => $output:expr,
        stream => $input:expr,
        position => $pos_type:ty | $pos:expr,
    }) => {
        let (parsed, input) = $parsed;
        assert!(parsed.is_ok());
        assert_eq!(parsed.unwrap(), $output);
        assert_eq!(input, State::<_, $pos_type>::new($input, $pos));
    };
}

#[cfg(test)]
macro_rules! assert_parse_err {
    ($parsed:expr, $input:expr) => {
        let (parsed, input) = $parsed;
        assert!(parsed.is_err());
        assert_eq!(input, $input);
    };
    ($parsed:expr, $input:expr, $type:ty | $position:expr) => {
        let (parsed, input) = $parsed;
        assert!(parsed.is_err());
        assert_eq!(input, State::<_, $type>::new($input, $position));
    };
}

#[macro_use]
pub mod combinator;
pub mod token;
pub mod transform;

use self::combinator::{and, or, And, Or};
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

    fn and<P>(self, other: P) -> And<Self, P>
    where
        Self: Sized,
        P: Parser<Input = Self::Input, Output = Self::Output>,
    {
        and(self, other)
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
