use super::position::Position;
use super::{Input, Tokens};

#[derive(Debug, Clone, PartialEq)]
pub struct State<I: Input, X: Position<I::Item>> {
    pub input: I,
    pub position: X,
}

impl<I: Input, X: Position<I::Item>> State<I, X> {
    pub fn new<Y: Into<X>>(input: I, position: Y) -> Self {
        State {
            input,
            position: position.into(),
        }
    }
}

impl<I: Input, X: Position<I::Item>> From<I> for State<I, X> {
    fn from(input: I) -> Self {
        State {
            input,
            position: Default::default(),
        }
    }
}

impl<I: Input, X: Position<I::Item>, F: Into<X>> From<(I, F)> for State<I, X> {
    fn from((input, pos): (I, F)) -> Self {
        State {
            input,
            position: pos.into(),
        }
    }
}

impl<I: Input, X: Position<I::Item>> Input for State<I, X> {
    type Item = I::Item;

    fn peek(&self) -> Option<Self::Item> {
        self.input.peek()
    }

    fn pop(&mut self) -> Option<Self::Item> {
        self.input.pop().map(|item| {
            self.position.update(&item);
            item
        })
    }

    fn tokens(&self) -> Tokens<Self::Item> {
        self.input.tokens()
    }
}
