mod input;

pub use self::input::Input;

#[derive(Debug, PartialEq)]
pub struct Error<I: Input> {
    msg: String,
    token: Option<I::Item>,
}

impl<I: Input> Error<I> {
    pub fn end() -> Self {
        Error {
            msg: "unexpected end of input".to_string(),
            token: None,
        }
    }
}

pub type ParseResult<I, O> = (Result<O, Error<I>>, I);

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

impl<I: Input, O> Parser for fn(&mut I) -> ParseResult<I, O> {
    type Input = I;
    type Output = O;

    fn parse(&mut self, mut i: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        self(&mut i)
    }
}
