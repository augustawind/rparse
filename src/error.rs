//! Error and Result types that are used by parsers.

use std::error::Error as StdError;
use std::fmt::Debug;

use input::Input;

#[derive(Debug)]
pub enum Info<I: Input> {
    Token(I::Item),
    Range(I),
    Description(String),
}

impl<I: Input> From<&'static str> for Info<I> {
    fn from(s: &str) -> Self {
        Info::Description(s.to_string())
    }
}

impl<I: Input> From<String> for Info<I> {
    fn from(s: String) -> Self {
        Info::Description(s)
    }
}

impl<I: Input<Item = char>> From<char> for Info<I> {
    fn from(c: char) -> Self {
        Info::Token(c)
    }
}

impl<I: Input<Item = u8>> From<u8> for Info<I> {
    fn from(b: u8) -> Self {
        Info::Token(b)
    }
}

impl<I, T> PartialEq for Info<I>
where
    I: Input<Item = T>,
    T: Copy + Debug + PartialEq,
{
    fn eq(&self, other: &Info<I>) -> bool {
        match (self, other) {
            (&Info::Token(ref l), &Info::Token(ref r)) => l == r,
            (&Info::Range(ref l), &Info::Range(ref r)) => l.tokens::<I>().eq(r.tokens::<I>()),
            (&Info::Description(ref l), &Info::Description(ref r)) => l == r,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub enum Error<I: Input> {
    EOF,
    Unexpected(Info<I>),
    Expected(Info<I>),
    Message(Info<I>),
    Errors(Vec<Error<I>>),
    Other(Box<StdError + Send + Sync>),
}

impl<I, T> Error<I>
where
    I: Input<Item = T>,
    T: Copy + PartialEq + Debug,
{
    pub fn expected_token(token: T) -> Self {
        Error::Expected(Info::Token(token))
    }

    pub fn unexpected_token(token: T) -> Self {
        Error::Unexpected(Info::Token(token))
    }

    pub fn add_error(&mut self, error: Error<I>) {
        if let Error::Errors(v) = self {
            v.push(error);
        } else {
            *self = error;
        }
    }
}

impl<I: Input> From<&'static str> for Error<I> {
    fn from(s: &str) -> Self {
        Error::Message(Info::Description(s.to_string()))
    }
}

impl<I: Input> From<String> for Error<I> {
    fn from(s: String) -> Self {
        Error::Message(Info::Description(s))
    }
}

impl<I: Input> From<Box<StdError + Send + Sync>> for Error<I> {
    fn from(error: Box<StdError + Send + Sync>) -> Self {
        Error::Other(error)
    }
}

impl<I, T> PartialEq for Error<I>
where
    I: Input<Item = T>,
    T: Copy + Debug + PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&Error::EOF, &Error::EOF) => true,
            (&Error::Unexpected(ref l), &Error::Unexpected(ref r)) => l == r,
            (&Error::Expected(ref l), &Error::Expected(ref r)) => l == r,
            (&Error::Message(ref l), &Error::Message(ref r)) => l == r,
            (&Error::Other(ref l), &Error::Other(ref r)) => l.to_string() == r.to_string(),
            _ => false,
        }
    }
}

pub type ParseResult<I, O> = (Result<O, Error<I>>, I);
