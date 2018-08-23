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

impl<I, T> PartialEq for Info<I>
where
    I: Input<Item = T>,
    T: Copy + Debug + PartialEq,
{
    fn eq(&self, other: &Info<I>) -> bool {
        match (self, other) {
            (&Info::Token(ref l), &Info::Token(ref r)) => l == r,
            (&Info::Range(ref l), &Info::Range(ref r)) => l.tokens().eq(r.tokens()),
            (&Info::Description(ref l), &Info::Description(ref r)) => l == r,
            _ => false,
        }
    }
}

type Position = (usize, usize);

pub trait ParseError<I: Input>: PartialEq {
    type Error: ParseError<I>;

    fn from_error(position: Position, error: Self::Error) -> Self;
    fn add(&mut self, error: Self::Error);
    fn is_eof(&self) -> bool;
}

#[derive(Debug)]
pub enum Error<I: Input> {
    EOF,
    Unexpected(Info<I>),
    Expected(Info<I>),
    Message(Info<I>),
    Other(Box<StdError + Send + Sync>),
    Errors(Vec<Error<I>>),
}

impl<I, T> Error<I>
where
    I: Input<Item = T>,
    T: Copy + PartialEq + Debug,
{
    pub fn expected_token(token: T) -> Self {
        Error::Expected(Info::Token(token))
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

impl<I: Input> ParseError<I> for Error<I> {
    type Error = Self;

    fn from_error(_: Position, error: Self::Error) -> Self {
        error
    }

    fn add(&mut self, error: Self::Error) {
        *self = error;
    }

    fn is_eof(&self) -> bool {
        match self {
            Error::EOF => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct Errors<I: Input> {
    position: Position,
    errors: Vec<Error<I>>,
}

impl<I, T> PartialEq for Errors<I>
where
    I: Input<Item = T>,
    T: Copy + Debug + PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.position == other.position && self.errors == other.errors
    }
}

impl<I: Input> ParseError<I> for Errors<I> {
    type Error = Error<I>;

    fn from_error(position: Position, error: Self::Error) -> Self {
        Errors {
            position,
            errors: vec![error],
        }
    }

    fn add(&mut self, error: Self::Error) {
        if !self.errors.contains(&error) {
            self.errors.push(error);
        }
    }

    fn is_eof(&self) -> bool {
        self.errors.iter().any(|e| e.is_eof())
    }
}

pub type ParseResult<I, O> = (Result<O, Error<I>>, I);
