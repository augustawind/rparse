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

impl<'a, I: Input, O> Parser for FnMut(I) -> ParseResult<I, O> + 'a {
    type Input = I;
    type Output = O;

    fn parse(&mut self, input: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        self(input)
    }
}

impl<I: Input, O> Parser for fn(I) -> ParseResult<I, O> {
    type Input = I;
    type Output = O;

    fn parse(&mut self, input: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        self(input)
    }
}
