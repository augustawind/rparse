pub mod input;
pub mod parser;

use self::input::Input;

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
