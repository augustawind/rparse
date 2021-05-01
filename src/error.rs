//! Error and Result types that are used by parsers.

use std::collections::HashSet;
use std::error::Error as StdError;
use std::fmt;

use stream::{Position, Stream, StreamItem};

pub type ParseResult<S, O> = Result<(Option<O>, S), (Error<S>, S)>;

/// A parse error. This is the library error type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error<S: Stream> {
    /// The cause of the error.
    pub cause: Info<S>,
    /// The expected input. If `cause` describes the input that _caused_ the error, `expected`
    /// describes the input that _wouldn't have_ caused an error.
    pub expected: Option<Expected<S>>,
    /// The position in the input at which the error occurred.
    pub position: S::Position,
}

impl<S: Stream> Error<S> {
    /// Create a new `Error`.
    ///
    /// This is the most general constructor for `Error`, setting all of its fields explicitly.
    pub fn new(cause: Info<S>, expected: Option<Expected<S>>, position: S::Position) -> Self {
        Error {
            cause,
            expected,
            position,
        }
    }

    /// Create a new `Error` with the given `cause`.
    ///
    /// Sets the other fields to their [`Default::default()`] values.
    pub fn cause<E: Into<Info<S>>>(cause: E) -> Self {
        Error::new(cause.into(), None, Default::default())
    }

    /// Create a new `Error` caused by a specific item of the input.
    ///
    /// Sets the other fields to their [`Default::default()`] values.
    pub fn item(item: S::Item) -> Self {
        Error::cause(Info::Item(item))
    }

    /// Create a new `Error` caused by a specific range of the input.
    ///
    /// Sets the other fields to their [`Default::default()`] values.
    pub fn range(range: S::Range) -> Self {
        Error::cause(Info::Range(range))
    }

    /// Create a new `Error` caused by reaching end-of-input.
    ///
    /// Sets the other fields to their [`Default::default()`] values.
    pub fn eoi() -> Self {
        Error::cause(Info::EOI)
    }

    /// Set the error's `position`. Chainable.
    pub fn at<P: Into<S::Position>>(mut self, position: P) -> Self {
        self.position = position.into();
        self
    }

    /// Sets the error's `expected` field. Chainable.
    pub fn expected<E: Into<Expected<S>>>(mut self, expected: E) -> Self {
        self.expected = Some(expected.into());
        self
    }

    /// Sets the error's `expected` to a specific [item](stream::Item) of input. Chainable.
    pub fn expected_item(mut self, item: S::Item) -> Self {
        self.expected = Some(Expected::Info(Info::Item(item)));
        self
    }

    /// Sets the error's `expected` to a specific [range](Stream::Range) of input. Chainable.
    pub fn expected_range(mut self, range: S::Range) -> Self {
        self.expected = Some(Expected::Info(Info::Range(range)));
        self
    }

    /// Sets the error's `expected` field, combining multiple [`Expected`] objects into a single
    /// [`Expected::OneOf`]. Chainable.
    pub fn expected_one_of<E, I>(mut self, errors: I) -> Self
    where
        E: Into<Expected<S>>,
        I: IntoIterator<Item = E>,
    {
        self.expected = Expected::merge_one_of(errors.into_iter().map(|e| Some(e.into())));
        self
    }

    /// Add the given [`Expected`] object to the error, merging with its existing `expected`.
    pub fn add_expected<E: Into<Expected<S>>>(&mut self, expected: E) {
        self.expected = Expected::merge_one_of(vec![self.expected.take(), Some(expected.into())]);
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
            write!(f, ": expected {}", expected)?;
        }
        Ok(())
    }
}

impl<S: Stream> StdError for Error<S> {}

impl<S: Stream> From<Info<S>> for Error<S> {
    fn from(info: Info<S>) -> Self {
        Error::cause(info)
    }
}

impl<S: Stream> From<u8> for Error<S> {
    fn from(b: u8) -> Self {
        Error::cause(Info::Item(b.into()))
    }
}

impl<S> From<char> for Error<S>
where
    S: Stream<Item = char>,
{
    fn from(ch: char) -> Self {
        Error::cause(Info::Item(ch))
    }
}

impl<S: Stream> From<&'static str> for Error<S> {
    fn from(s: &'static str) -> Self {
        Error::cause(Info::Msg(s))
    }
}

impl<S: Stream> From<String> for Error<S> {
    fn from(s: String) -> Self {
        Error::cause(Info::MsgOwned(s))
    }
}

impl<S: Stream, E: StdError + Send + Sync + 'static> From<Box<E>> for Error<S> {
    fn from(error: Box<E>) -> Self {
        error.to_string().into()
    }
}

/// Holds the _cause_ of a parse error.
#[derive(Debug, Clone, Hash)]
pub enum Info<S: Stream> {
    /// Error caused by a specific [item](stream::StreamItem) of the parsed input.
    Item(S::Item),
    /// Error caused by a specific [range](stream::RangeStream) of the parsed input.
    Range(S::Range),
    /// Message describing the cause of the error.
    Msg(&'static str),
    /// Owned version of [`Msg`].
    MsgOwned(String),
    /// Parser reached end-of-input too soon.
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
            Info::Item(item) => write!(f, "item '{}'", item.as_char()),
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

#[derive(Debug, Clone)]
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
        Self::merge_errors(errors, Expected::Seq)
    }

    pub fn merge_one_of<I>(errors: I) -> Option<Self>
    where
        I: IntoIterator<Item = Option<Expected<S>>>,
    {
        Self::merge_errors(errors, Expected::OneOf)
    }

    fn merge_errors<I, F>(errors: I, f: F) -> Option<Expected<S>>
    where
        I: IntoIterator<Item = Option<Expected<S>>>,
        F: FnOnce(Vec<Expected<S>>) -> Expected<S>,
    {
        let mut vec = errors.into_iter().fold(Vec::new(), |mut acc, e| {
            match e {
                Some(Expected::OneOf(xs)) => acc.extend(xs.into_iter()),
                Some(expected) => acc.push(expected),
                _ => (),
            };
            acc
        });
        match vec.len() {
            0 => None,
            1 => vec.pop(),
            _ => Some(f(vec)),
        }
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

impl<S: Stream> PartialEq for Expected<S> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expected::Seq(l), Expected::Seq(r)) => l == r,
            (Expected::OneOf(l), Expected::OneOf(r)) => l == r,
            (Expected::Info(l), Expected::Info(r)) => l == r,
            _ => false,
        }
    }
}

impl<S: Stream> Eq for Expected<S> {}

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
