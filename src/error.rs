//! Error and Result types that are used by parsers.

use std::cmp::Ordering;
use std::error::Error as StdError;
use std::fmt;
use std::fmt::Debug;
use std::iter::FromIterator;

use stream::{Position, Stream};

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum Error<S: Stream> {
    EOF,
    Unexpected(Info<S>),
    Expected(Info<S>),
    Message(Info<S>),
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

    pub fn is_eof(&self) -> bool {
        match self {
            Error::EOF => true,
            _ => false,
        }
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
pub struct Errors<S: Stream, X: Position<S::Item>> {
    pub position: X,
    pub errors: Vec<Error<S::Range>>,
    is_eof: bool,
}

impl<S: Stream, X: Position<S::Item>> Errors<S, X> {
    pub fn new(position: X) -> Self {
        Errors {
            position,
            errors: Vec::new(),
            is_eof: false,
        }
    }

    pub fn from_error(position: X, error: Error<S::Range>) -> Self {
        let is_eof = error.is_eof();
        Errors {
            position,
            errors: vec![error],
            is_eof,
        }
    }

    pub fn from_errors(position: X, errors: Vec<Error<S::Range>>) -> Self {
        let is_eof = errors.iter().any(|err| err.is_eof());
        Errors {
            position,
            errors,
            is_eof,
        }
    }

    pub fn is_eof(&self) -> bool {
        return self.is_eof;
    }

    pub fn add_error(&mut self, error: Error<S::Range>) {
        if error.is_eof() {
            self.is_eof = true;
        }
        self.errors.push(error);
    }

    pub fn add_errors(&mut self, mut errors: Vec<Error<S::Range>>) {
        if errors.iter().any(|err| err.is_eof()) {
            self.is_eof = true;
        }
        self.errors.append(&mut errors);
    }

    pub fn merge_errors(&mut self, errors: &mut Errors<S, X>) {
        match errors.position.cmp(&self.position) {
            Ordering::Greater => {
                self.position = errors.position.clone();
                if errors.is_eof() {
                    self.is_eof = true;
                }
                self.errors.clear();
                self.errors.append(&mut errors.errors);
            }
            Ordering::Equal => {
                if errors.is_eof() {
                    self.is_eof = true;
                }
                self.errors.append(&mut errors.errors);
            }
            Ordering::Less => {}
        }
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

impl<S: Stream, X: Position<S::Item>> From<Error<S::Range>> for Errors<S, X> {
    fn from(error: Error<S::Range>) -> Self {
        Self::from_error(Default::default(), error)
    }
}

impl<S: Stream, X: Position<S::Item>> FromIterator<Error<S::Range>> for Errors<S, X> {
    fn from_iter<T: IntoIterator<Item = Error<S::Range>>>(iter: T) -> Self {
        Self::from_errors(Default::default(), iter.into_iter().collect())
    }
}

impl<S: Stream, X: Position<S::Item>> From<Vec<Error<S::Range>>> for Errors<S, X> {
    fn from(v: Vec<Error<S::Range>>) -> Self {
        Self::from_iter(v)
    }
}

pub type ParseResult<S, O> = (Result<O, Errors<S, <S as Stream>::Position>>, S);
