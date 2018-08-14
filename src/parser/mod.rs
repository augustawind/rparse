mod input;

pub use self::input::Input;

#[derive(Debug, PartialEq)]
pub struct Error(String);

impl Error {
    pub fn eof() -> Self {
        Error("unexpected end of input".to_string())
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseResult<I: Input, O> {
    Ok((I, O)),
    Err((I, Error)),
}

impl<I: Input, O> ParseResult<I, O> {
    pub fn result(self) -> Result<(I, O), (I, Error)> {
        match self {
            ParseResult::Ok(ok) => Ok(ok),
            ParseResult::Err(err) => Err(err),
        }
    }
}

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
