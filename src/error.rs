//! Error and Result types that are used by parsers.

use std::error::Error as StdError;
use std::fmt;
use std::fmt::Debug;
use std::iter::FromIterator;

use stream::{Position, Stream};

#[derive(Debug)]
pub enum Info<I: Stream> {
    Token(I::Item),
    Range(I),
    Msg(&'static str),
    MsgOwned(String),
}

impl<I: Stream> fmt::Display for Info<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Info::Token(token) => write!(f, "token {:?}", token),
            Info::Range(range) => write!(f, "range {:?}", range),
            Info::Msg(msg) => write!(f, "{}", msg),
            Info::MsgOwned(msg) => write!(f, "{}", msg),
        }
    }
}

impl<I: Stream> From<&'static str> for Info<I> {
    fn from(s: &'static str) -> Self {
        Info::Msg(s)
    }
}

impl<I: Stream> From<String> for Info<I> {
    fn from(s: String) -> Self {
        Info::MsgOwned(s)
    }
}

impl<I: Stream<Item = char>> From<char> for Info<I> {
    fn from(c: char) -> Self {
        Info::Token(c)
    }
}

impl<I: Stream<Item = u8>> From<u8> for Info<I> {
    fn from(b: u8) -> Self {
        Info::Token(b)
    }
}

impl<I, T> PartialEq for Info<I>
where
    I: Stream<Item = T>,
    T: Copy + Debug + PartialEq,
{
    fn eq(&self, other: &Info<I>) -> bool {
        match (self, other) {
            (&Info::Token(ref l), &Info::Token(ref r)) => l == r,
            (&Info::Range(ref l), &Info::Range(ref r)) => l.tokens().eq(r.tokens()),
            (&Info::Msg(ref l), &Info::Msg(ref r)) => l == r,
            (&Info::MsgOwned(ref l), &Info::MsgOwned(ref r)) => l == r,
            (&Info::Msg(ref l), &Info::MsgOwned(ref r)) => l == r,
            (&Info::MsgOwned(ref l), &Info::Msg(ref r)) => l == r,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub enum Error<I: Stream> {
    EOF,
    Unexpected(Info<I>),
    Expected(Info<I>),
    Message(Info<I>),
    Other(Box<StdError + Send + Sync>),
}

impl<I, T> Error<I>
where
    I: Stream<Item = T>,
    T: Copy + PartialEq + Debug,
{
    pub fn expected_token(token: T) -> Self {
        Error::Expected(Info::Token(token))
    }

    pub fn unexpected_token(token: T) -> Self {
        Error::Unexpected(Info::Token(token))
    }
}

impl<I: Stream> fmt::Display for Error<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::EOF => write!(f, "unexpected end of input"),
            Error::Unexpected(info) => write!(f, "unexpected {}", info),
            Error::Expected(info) => write!(f, "expected {}", info),
            Error::Message(info) => write!(f, "{}", info),
            // TODO: use this for Errors<I, X>
            // Error::Errors(errs) => {
            //     write!(
            //         f,
            //         "{}",
            //         errs.iter()
            //             .map(|e| e.to_string())
            //             .collect::<Vec<String>>()
            //             .join("\n")
            //     )
            // }
            Error::Other(err) => write!(f, "{}", err),
        }
    }
}

impl<I: Stream> StdError for Error<I> {}

impl<I: Stream> From<&'static str> for Error<I> {
    fn from(s: &'static str) -> Self {
        Error::Message(Info::Msg(s))
    }
}

impl<I: Stream> From<String> for Error<I> {
    fn from(s: String) -> Self {
        Error::Message(Info::MsgOwned(s))
    }
}

impl<I: Stream, E: StdError + Send + Sync + 'static> From<Box<E>> for Error<I> {
    fn from(error: Box<E>) -> Self {
        Error::Other(error)
    }
}

#[derive(Debug, PartialEq)]
pub struct Errors<I: Stream, X: Position<I::Item>> {
    pub position: X,
    pub errors: Vec<Error<I>>,
}

impl<I: Stream, X: Position<I::Item>> Errors<I, X> {
    pub fn new(position: X, error: Error<I>) -> Self {
        Errors {
            position,
            errors: vec![error],
        }
    }

    pub fn add_error(&mut self, error: Error<I>) {
        self.errors.push(error);
    }
}

impl<I: Stream, X: Position<I::Item>> fmt::Display for Errors<I, X> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Parse error at {}:", self.position)?;
        for error in self.errors.iter() {
            writeln!(f, "{}", error)?;
        }
        Ok(())
    }
}

impl<I: Stream, X: Position<I::Item>> StdError for Errors<I, X> {}

impl<I: Stream, X: Position<I::Item>> From<Error<I>> for Errors<I, X> {
    fn from(error: Error<I>) -> Self {
        Errors {
            position: Default::default(),
            errors: vec![error],
        }
    }
}

impl<I: Stream, X: Position<I::Item>> FromIterator<Error<I>> for Errors<I, X> {
    fn from_iter<T: IntoIterator<Item = Error<I>>>(iter: T) -> Self {
        Errors {
            position: Default::default(),
            errors: iter.into_iter().collect(),
        }
    }
}

impl<I: Stream, X: Position<I::Item>> From<Vec<Error<I>>> for Errors<I, X> {
    fn from(v: Vec<Error<I>>) -> Self {
        Self::from_iter(v)
    }
}

impl<I, T> PartialEq for Error<I>
where
    I: Stream<Item = T>,
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
