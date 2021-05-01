//! Error and Result types that are used by parsers.

use std::collections::HashSet;
use std::error::Error as StdError;
use std::fmt;

use stream::{Position, Stream};

/// The content of a parse error.
#[derive(Debug, Clone, Hash)]
pub enum Info<S: Stream> {
    Item(S::Item),
    Range(S::Range),
    Msg(&'static str),
    MsgOwned(String),
    EOI,
}

impl<S: Stream> PartialEq for Info<S> {
    fn eq(&self, other: &Info<S>) -> bool {
        match (self, other) {
            (&Info::Item(ref l), &Info::Item(ref r)) => l == r,
            (&Info::Range(ref l), &Info::Range(ref r)) => l == r,
            (&Info::Msg(ref l), &Info::Msg(ref r)) => l == r,
            (&Info::MsgOwned(ref l), &Info::MsgOwned(ref r)) => l == r,
            (&Info::Msg(ref l), &Info::MsgOwned(ref r)) => l == r,
            (&Info::MsgOwned(ref l), &Info::Msg(ref r)) => l == r,
            (&Info::EOI, &Info::EOI) => true,
            _ => false,
        }
    }
}

impl<S: Stream> Eq for Info<S> {}

impl<S: Stream> fmt::Display for Info<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Info::Item(item) => write!(f, "item {:?}", item),
            Info::Range(range) => write!(f, "range {:?}", range),
            Info::Msg(msg) => write!(f, "{}", msg),
            Info::MsgOwned(msg) => write!(f, "{}", msg),
            Info::EOI => write!(f, "end of input"),
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
{
    fn from(c: char) -> Self {
        Info::Item(c)
    }
}

impl<S> From<u8> for Info<S>
where
    S: Stream<Item = u8>,
{
    fn from(b: u8) -> Self {
        Info::Item(b)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expected<S: Stream> {
    Seq(Vec<Expected<S>>),
    OneOf(Vec<Expected<S>>),
    Info(Info<S>),
}

impl<S: Stream> Expected<S> {
    pub fn merge_seq<I>(errors: I) -> Option<Self>
    where
        I: IntoIterator<Item = Option<Expected<S>>>,
    {
        Self::merge_errors(errors).map(Expected::Seq)
    }

    pub fn merge_one_of<I>(errors: I) -> Option<Self>
    where
        I: IntoIterator<Item = Option<Expected<S>>>,
    {
        Self::merge_errors(errors).map(Expected::OneOf)
    }

    fn merge_errors<I>(errors: I) -> Option<Vec<Expected<S>>>
    where
        I: IntoIterator<Item = Option<Expected<S>>>,
    {
        let vec = errors.into_iter().fold(Vec::new(), |mut acc, e| {
            match e {
                Some(Expected::OneOf(xs)) => acc.extend(xs.into_iter()),
                Some(expected) => acc.push(expected),
                None => (),
            };
            acc
        });
        (!vec.is_empty()).then(|| vec)
    }

    pub fn item(item: S::Item) -> Self {
        Expected::Info(Info::Item(item))
    }

    pub fn range(range: S::Range) -> Self {
        Expected::Info(Info::Range(range))
    }

    fn join_expected(errors: &[Expected<S>], sep: &str) -> String {
        errors
            .into_iter()
            .map(|e| e.to_string())
            .collect::<Vec<String>>()
            .join(sep)
    }
}

impl<S: Stream> fmt::Display for Expected<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Seq(errors) => write!(f, "({})", Self::join_expected(errors, ", ")),
            Self::OneOf(errors) => write!(f, "({})", Self::join_expected(errors, " OR ")),
            Self::Info(info) => write!(f, "{}", info),
        }
    }
}

impl<S: Stream, T: Into<Expected<S>>> From<Vec<T>> for Expected<S> {
    fn from(errors: Vec<T>) -> Self {
        Expected::Seq(
            errors
                .into_iter()
                .map(|e| e.into())
                .collect::<Vec<Expected<S>>>(),
        )
    }
}

impl<S: Stream, T: Into<Expected<S>>> From<HashSet<T>> for Expected<S> {
    fn from(errors: HashSet<T>) -> Self {
        Expected::OneOf(
            errors
                .into_iter()
                .map(|e| e.into())
                .collect::<Vec<Expected<S>>>(),
        )
    }
}

impl<S: Stream> From<Info<S>> for Expected<S> {
    fn from(info: Info<S>) -> Self {
        Expected::Info(info)
    }
}

impl<S: Stream> From<u8> for Expected<S> {
    fn from(b: u8) -> Self {
        Expected::Info(Info::Item(b.into()))
    }
}

impl<S> From<char> for Expected<S>
where
    S: Stream<Item = char>,
{
    fn from(ch: char) -> Self {
        Expected::Info(Info::Item(ch))
    }
}

impl<S: Stream> From<&'static str> for Expected<S> {
    fn from(s: &'static str) -> Self {
        Expected::Info(Info::Msg(s))
    }
}

impl<S: Stream> From<String> for Expected<S> {
    fn from(s: String) -> Self {
        Expected::Info(Info::MsgOwned(s))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error<S: Stream> {
    cause: Info<S>,
    expected: Option<Expected<S>>,
    position: S::Position,
}

/// A parse error.
impl<S: Stream> Error<S> {
    pub fn new(cause: Info<S>) -> Self {
        Error {
            cause,
            expected: None,
            position: Default::default(),
        }
    }

    pub fn eoi() -> Self {
        Error {
            cause: Info::EOI,
            expected: None,
            position: Default::default(),
        }
    }

    pub fn item(item: S::Item) -> Self {
        Error {
            cause: Info::Item(item),
            expected: None,
            position: Default::default(),
        }
    }

    pub fn range(range: S::Range) -> Self {
        Error {
            cause: Info::Range(range),
            expected: None,
            position: Default::default(),
        }
    }

    pub fn at<P>(mut self, position: P) -> Self
    where
        P: Into<S::Position>,
    {
        self.position = position.into();
        self
    }

    pub fn expected<E>(mut self, expected: E) -> Self
    where
        E: Into<Expected<S>>,
    {
        self.expected = Some(expected.into());
        self
    }

    pub fn expected_one_of<E, I>(mut self, errors: I) -> Self
    where
        E: Into<Expected<S>>,
        I: IntoIterator<Item = E>,
    {
        self.expected = Some(Expected::OneOf(
            errors.into_iter().map(|e| e.into()).collect(),
        ));
        self
    }

    pub fn expected_item(mut self, item: S::Item) -> Self {
        self.expected = Some(Expected::Info(Info::Item(item)));
        self
    }

    pub fn expected_range(mut self, range: S::Range) -> Self {
        self.expected = Some(Expected::Info(Info::Range(range)));
        self
    }

    pub fn add_expected<E>(&mut self, expected: E)
    where
        E: Into<Expected<S>>,
    {
        self.expected = Some(expected.into());
    }
}

impl<S: Stream> fmt::Display for Error<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}: unexpected {}",
            self.position.fmt_msg("parsing failed"),
            self.cause
        )?;
        if let Some(expected) = &self.expected {
            write!(f, "; expected {}", expected)?;
        }
        Ok(())
    }
}

impl<S: Stream> StdError for Error<S> {}

impl<S: Stream> From<Info<S>> for Error<S> {
    fn from(info: Info<S>) -> Self {
        Error::new(info)
    }
}

impl<S: Stream> From<u8> for Error<S> {
    fn from(b: u8) -> Self {
        Error::new(Info::Item(b.into()))
    }
}

impl<S> From<char> for Error<S>
where
    S: Stream<Item = char>,
{
    fn from(ch: char) -> Self {
        Error::new(Info::Item(ch))
    }
}

impl<S: Stream> From<&'static str> for Error<S> {
    fn from(s: &'static str) -> Self {
        Error::new(Info::Msg(s))
    }
}

impl<S: Stream> From<String> for Error<S> {
    fn from(s: String) -> Self {
        Error::new(Info::MsgOwned(s))
    }
}

impl<S: Stream, E: StdError + Send + Sync + 'static> From<Box<E>> for Error<S> {
    fn from(error: Box<E>) -> Self {
        error.to_string().into()
    }
}

pub type ParseResult<S, O> = Result<(Option<O>, S), (Error<S>, S)>;
