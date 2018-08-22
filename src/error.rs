//! Error and Result types that are used by parsers.

use std::error::Error as StdError;
use std::fmt::Debug;

use input::Input;

#[derive(Debug)]
pub enum Info<I: Input> {
    Token(I::Item),
    Range(I),
    Description(&'static str),
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

#[derive(Debug)]
pub enum Error<I: Input> {
    Expected(Info<I>),
    Message(Info<I>),
    Other(Box<StdError + Send + Sync>),
}

impl<I, T> PartialEq for Error<I>
where
    I: Input<Item = T>,
    T: Copy + Debug + PartialEq,
{
    fn eq(&self, other: &Error<I>) -> bool {
        match (self, other) {
            (&Error::Expected(ref l), &Error::Expected(ref r)) => l == r,
            (&Error::Message(ref l), &Error::Message(ref r)) => l == r,
            (&Error::Other(ref l), &Error::Other(ref r)) => l.to_string() == r.to_string(),
            _ => false,
        }
    }
}

pub type ParseResult<I, O> = (Result<O, Error<I>>, I);
