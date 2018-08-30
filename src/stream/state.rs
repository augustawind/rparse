use super::position::Position;
use super::{Stream, Tokens};
use error::{Error, Errors};

#[derive(Debug, Clone, PartialEq)]
pub struct State<I: Stream, X: Position<I::Item>> {
    pub stream: I,
    pub position: X,
}

impl<I: Stream, X: Position<I::Item>> State<I, X> {
    pub fn new<Y: Into<X>>(stream: I, position: Y) -> Self {
        State {
            stream,
            position: position.into(),
        }
    }

    pub fn conv_error<O>(self, error: Error<I>) -> Errors<I, X> {
        Errors::new(self.position.clone(), error)
    }
}

impl<I: Stream, X: Position<I::Item>> From<I> for State<I, X> {
    fn from(stream: I) -> Self {
        State {
            stream,
            position: Default::default(),
        }
    }
}

impl<I: Stream, X: Position<I::Item>, F: Into<X>> From<(I, F)> for State<I, X> {
    fn from((stream, pos): (I, F)) -> Self {
        State {
            stream,
            position: pos.into(),
        }
    }
}

impl<I: Stream, X: Position<I::Item>> Stream for State<I, X> {
    type Item = I::Item;
    type Position = X;

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
}
