//! Error and Result types that are used by parsers.

use std::error::Error as StdError;
use std::fmt;
use std::fmt::Debug;
use std::iter::FromIterator;

use stream::{Position, Stream};

#[derive(Debug)]
pub enum Info<S: Stream> {
    Token(S::Item),
    Range(S),
    Msg(&'static str),
    MsgOwned(String),
}

impl<S: Stream> fmt::Display for Info<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Info::Token(token) => write!(f, "token {:?}", token),
            Info::Range(range) => write!(f, "range {:?}", range),
            Info::Msg(msg) => write!(f, "{}", msg),
            Info::MsgOwned(msg) => write!(f, "{}", msg),
        }
    }
}

impl<S: Stream> From<&'static str> for Info<S> {
    fn from(s: &'static str) -> Self {
        Info::Msg(s)
    }
}

impl<S: Stream> From<String> for Info<S> {
    fn from(s: String) -> Self {
        Info::MsgOwned(s)
    }
}

impl<S> From<char> for Info<S>
where
    S: Stream<Item = char>,
    S::Position: Position<char>,
{
    fn from(c: char) -> Self {
        Info::Token(c)
    }
}

impl<S> From<u8> for Info<S>
where
    S: Stream<Item = u8>,
    S::Position: Position<u8>,
{
    fn from(b: u8) -> Self {
        Info::Token(b)
    }
}

impl<S, T> PartialEq for Info<S>
where
    S: Stream<Item = T>,
    S::Position: Position<T>,
    T: Copy + Debug + PartialEq,
{
    fn eq(&self, other: &Info<S>) -> bool {
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
pub enum Error<S: Stream> {
    EOF,
    Unexpected(Info<S>),
    Expected(Info<S>),
    Message(Info<S>),
    Other(Box<StdError + Send + Sync>),
}

impl<S, T> Error<S>
where
    S: Stream<Item = T>,
    S::Position: Position<T>,
    T: Copy + PartialEq + Debug,
{
    pub fn expected_token(token: T) -> Self {
        Error::Expected(Info::Token(token))
    }

    pub fn unexpected_token(token: T) -> Self {
        Error::Unexpected(Info::Token(token))
    }
}

impl<S, T> PartialEq for Error<S>
where
    S: Stream<Item = T>,
    S::Position: Position<T>,
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

impl<S: Stream> fmt::Display for Error<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::EOF => write!(f, "unexpected end of input"),
            Error::Unexpected(info) => write!(f, "unexpected {}", info),
            Error::Expected(info) => write!(f, "expected {}", info),
            Error::Message(info) => write!(f, "{}", info),
            Error::Other(err) => write!(f, "{}", err),
        }
    }
}

impl<S: Stream> StdError for Error<S> {}

impl<S: Stream> From<&'static str> for Error<S> {
    fn from(s: &'static str) -> Self {
        Error::Message(Info::Msg(s))
    }
}

impl<S: Stream> From<String> for Error<S> {
    fn from(s: String) -> Self {
        Error::Message(Info::MsgOwned(s))
    }
}

impl<S: Stream, E: StdError + Send + Sync + 'static> From<Box<E>> for Error<S> {
    fn from(error: Box<E>) -> Self {
        Error::Other(error)
    }
}

#[derive(Debug, PartialEq)]
pub struct Errors<S: Stream, X: Position<S::Item>> {
    pub position: X,
    pub errors: Vec<Error<S>>,
}

impl<S: Stream, X: Position<S::Item>> Errors<S, X> {
    pub fn new(position: X, error: Error<S>) -> Self {
        Errors {
            position,
            errors: vec![error],
        }
    }

    pub fn add_error(&mut self, error: Error<S>) {
        self.errors.push(error);
    }
}

impl<S: Stream, X: Position<S::Item>> fmt::Display for Errors<S, X> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}:", self.position.fmt_msg("Parse error"))?;
        for error in self.errors.iter() {
            writeln!(f, "{}", error)?;
        }
        Ok(())
    }
}

impl<S: Stream, X: Position<S::Item>> StdError for Errors<S, X> {}

impl<S: Stream, X: Position<S::Item>> From<Error<S>> for Errors<S, X> {
    fn from(error: Error<S>) -> Self {
        Errors {
            position: Default::default(),
            errors: vec![error],
        }
    }
}

impl<S: Stream, X: Position<S::Item>> FromIterator<Error<S>> for Errors<S, X> {
    fn from_iter<T: IntoIterator<Item = Error<S>>>(iter: T) -> Self {
        Errors {
            position: Default::default(),
            errors: iter.into_iter().collect(),
        }
    }
}

impl<S: Stream, X: Position<S::Item>> From<Vec<Error<S>>> for Errors<S, X> {
    fn from(v: Vec<Error<S>>) -> Self {
        Self::from_iter(v)
    }
}

pub type ParseResult<S, O> = (Result<O, Error<S>>, S);
