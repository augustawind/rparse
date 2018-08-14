use super::{Input, ParseResult};

pub trait Parser {
    type Input: Input;
    type Output;

    fn parse(&mut self, Self::Input) -> ParseResult<Self::Input, Self::Output>;
}

impl<'a, I: Input, O> Parser for FnMut(&mut I) -> ParseResult<I, O> + 'a {
    type Input = I;
    type Output = O;

    fn parse(&mut self, mut i: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        self(&mut i)
    }
}
