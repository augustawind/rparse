use super::position::Position;
use super::{Stream, Tokens};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct State<S: Stream, X: Position<S>> {
    pub stream: S,
    pub position: X,
}

impl<S: Stream, X: Position<S>> State<S, X> {
    pub fn new<T: Into<X>>(stream: S, position: T) -> Self {
        State {
            stream,
            position: position.into(),
        }
    }
}

impl<S: Stream, X: Position<S>> From<S> for State<S, X> {
    fn from(stream: S) -> Self {
        State {
            stream,
            position: Default::default(),
        }
    }
}

impl<S: Stream, X: Position<S>, T: Into<X>> From<(S, T)> for State<S, X> {
    fn from((stream, pos): (S, T)) -> Self {
        State::new(stream, pos)
    }
}

impl<S, X> Stream for State<S, X>
where
    S: Stream,
    X: Position<S>,
{
    type Stream = S;
    type Position = X;
    type Item = S::Item;
    type Range = S::Range;

    fn peek(&self) -> Option<Self::Item> {
        self.stream.peek()
    }

    fn pop(&mut self) -> Option<Self::Item> {
        self.stream.pop().map(|item| {
            self.position.update(&item);
            item
        })
    }

    fn tokens(&self) -> Tokens<Self::Item> {
        self.stream.tokens()
    }

    fn range(&mut self, to_idx: usize) -> Option<Self::Range> {
        self.stream.range(to_idx).map(|range| {
            self.position.update_range(&range);
            range
        })
    }

    fn as_range(&mut self) -> Self::Range {
        let range = self.stream.as_range();
        self.position.update_range(&range);
        range
    }

    fn position(&self) -> &Self::Position {
        &self.position
    }
}
