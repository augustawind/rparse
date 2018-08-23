//! Error and Result types that are used by parsers.

use std::error::Error as StdError;
use std::fmt::Debug;
use std::iter::FromIterator;

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

type Position = (usize, usize);

#[derive(Debug)]
pub struct Errors<I: Input> {
    position: Position,
    errors: Vec<Error<I>>,
}

impl<I: Input> Errors<I> {
    pub fn from_error(position: Position, error: Error<I>) -> Self {
        Errors {
            position,
            errors: vec![error],
        }
    }

    pub fn from_errors<E>(position: Position, errors: E) -> Self
    where
        E: IntoIterator<Item = Error<I>>,
    {
        Errors {
            position,
            errors: Vec::from_iter(errors.into_iter()),
        }
    }

    pub fn add_error(&mut self, error: Error<I>) {
        if !self.errors.contains(&error) {
            self.errors.push(error);
        };
    }
}

pub type ParseResult<I, O> = (Result<O, Error<I>>, I);
