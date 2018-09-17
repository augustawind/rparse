//! Error and Result types that are used by parsers.

use std::cmp::Ordering;
use std::error::Error as StdError;
use std::fmt;
use std::iter::FromIterator;

use stream::{Position, Stream};

#[derive(Debug, Clone)]
pub enum Info<S: Stream> {
    Token(S::Item),
    Range(S::Range),
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
    S::Position: Position<S::Stream>,
{
    fn from(c: char) -> Self {
        Info::Token(c)
    }
}

impl<S> From<u8> for Info<S>
where
    S: Stream<Item = u8>,
    S::Position: Position<S::Stream>,
{
    fn from(b: u8) -> Self {
        Info::Token(b)
    }
}

impl<S> PartialEq for Info<S>
where
    S: Stream,
    S::Position: Position<S::Stream>,
{
    fn eq(&self, other: &Info<S>) -> bool {
        match (self, other) {
            (&Info::Token(ref l), &Info::Token(ref r)) => l == r,
            (&Info::Range(ref l), &Info::Range(ref r)) => l == r,
            (&Info::Msg(ref l), &Info::Msg(ref r)) => l == r,
            (&Info::MsgOwned(ref l), &Info::MsgOwned(ref r)) => l == r,
            (&Info::Msg(ref l), &Info::MsgOwned(ref r)) => l == r,
            (&Info::MsgOwned(ref l), &Info::Msg(ref r)) => l == r,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Error<S: Stream> {
    EOF,
    Unexpected(Info<S>),
    Expected(Info<S>),
    Message(Info<S>),
}

impl<S> Error<S>
where
    S: Stream,
    S::Position: Position<S::Stream>,
{
    pub fn expected_token(token: S::Item) -> Self {
        Error::Expected(Info::Token(token))
    }

    pub fn unexpected_token(token: S::Item) -> Self {
        Error::Unexpected(Info::Token(token))
    }

    pub fn expected_range(range: S::Range) -> Self {
        Error::Expected(Info::Range(range))
    }

    pub fn unexpected_range(range: S::Range) -> Self {
        Error::Unexpected(Info::Range(range))
    }
}

impl<S> PartialEq for Error<S>
where
    S: Stream,
    S::Position: Position<S::Stream>,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&Error::EOF, &Error::EOF) => true,
            (&Error::Unexpected(ref l), &Error::Unexpected(ref r)) => l == r,
            (&Error::Expected(ref l), &Error::Expected(ref r)) => l == r,
            (&Error::Message(ref l), &Error::Message(ref r)) => l == r,
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
        error.to_string().into()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Errors<S: Stream> {
    pub position: S::Position,
    pub errors: Vec<Error<S>>,
}

impl<S: Stream> Errors<S> {
    pub fn new(position: S::Position) -> Self {
        Errors {
            position,
            errors: Vec::new(),
        }
    }

    pub fn from_error(position: S::Position, error: Error<S>) -> Self {
        Errors {
            position,
            errors: vec![error],
        }
    }

    pub fn from_errors(position: S::Position, errors: Vec<Error<S>>) -> Self {
        Errors { position, errors }
    }

    pub fn add_error(&mut self, error: Error<S>) {
        if self.errors.contains(&error) {
            return;
        }
        self.errors.push(error);
    }

    pub fn add_errors(&mut self, errors: Vec<Error<S>>) {
        for err in errors {
            self.add_error(err);
        }
    }

    pub fn merge_errors(&mut self, errors: &mut Errors<S>) {
        match errors.position.cmp(&self.position) {
            Ordering::Greater => {
                self.position = errors.position.clone();
                self.errors.clear();
                self.errors.extend(errors.errors.drain(..));
            }
            Ordering::Equal => {
                for err in errors.errors.drain(..) {
                    if !self.errors.contains(&err) {
                        self.errors.push(err);
                    }
                }
            }
            Ordering::Less => errors.errors.clear(),
        }
    }
}

impl<S: Stream> fmt::Display for Errors<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}:", self.position.fmt_msg("Parse error"))?;
        for error in self.errors.iter() {
            writeln!(f, "{}", error)?;
        }
        Ok(())
    }
}

impl<S: Stream> StdError for Errors<S> {}

impl<S: Stream> From<Error<S>> for Errors<S> {
    fn from(error: Error<S>) -> Self {
        Self::from_error(Default::default(), error)
    }
}

impl<S: Stream> FromIterator<Error<S>> for Errors<S> {
    fn from_iter<T: IntoIterator<Item = Error<S>>>(iter: T) -> Self {
        Self::from_errors(Default::default(), iter.into_iter().collect())
    }
}

impl<S: Stream> From<Vec<Error<S>>> for Errors<S> {
    fn from(v: Vec<Error<S>>) -> Self {
        Self::from_iter(v)
    }
}

impl<S: Stream, P, I> From<(P, I)> for Errors<S>
where
    I: IntoIterator<Item = Error<S>>,
    P: Into<S::Position>,
{
    fn from((pos, iter): (P, I)) -> Self {
        Self::from_errors(pos.into(), iter.into_iter().collect())
    }
}

pub type ParseResult<S, O> = (Result<O, Errors<S>>, S);
