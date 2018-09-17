use std::fmt::Debug;
use std::mem;

use super::{NullPosition, RangeStream, Stream, Tokens};

impl Stream for String {
    type Item = char;
    type Range = Self;
    type Position = NullPosition;

    fn peek(&self) -> Option<Self::Item> {
        self.chars().next()
    }

    fn pop(&mut self) -> Option<Self::Item> {
        if self.is_empty() {
            None
        } else {
            Some(self.remove(0))
        }
    }

    fn tokens(&self) -> Tokens<Self::Item> {
        Tokens::new(self.chars())
    }

    fn range(&mut self, idx: usize) -> Option<Self::Range> {
        if idx > self.len() {
            None
        } else {
            let range = self.split_off(idx);
            Some(mem::replace(self, range))
        }
    }

    fn position(&self) -> Self::Position {
        NullPosition(())
    }
}

impl RangeStream for String {
    fn len(&self) -> usize {
        String::len(self)
    }
}

impl<'a> Stream for &'a str {
    type Item = char;
    type Range = Self;
    type Position = NullPosition;

    fn peek(&self) -> Option<Self::Item> {
        self.chars().next()
    }

    fn pop(&mut self) -> Option<Self::Item> {
        let mut iter = self.char_indices();
        iter.next().map(|(_, c)| {
            match iter.next() {
                Some((n, _)) => *self = &self[n..],
                None => *self = &self[..0],
            }

            c
        })
    }

    fn tokens(&self) -> Tokens<Self::Item> {
        Tokens::new(self.chars())
    }

    fn range(&mut self, idx: usize) -> Option<Self::Range> {
        let range = &self.get(..idx);
        range.map(|range| {
            *self = &mut &self[idx..];
            range
        })
    }

    fn position(&self) -> Self::Position {
        NullPosition(())
    }
}

impl<'a> RangeStream for &'a str {
    fn len(&self) -> usize {
        str::len(self)
    }
}

impl<T> Stream for Vec<T>
where
    T: Copy + PartialEq + Debug,
{
    type Item = T;
    type Range = Self;
    type Position = NullPosition;

    fn peek(&self) -> Option<Self::Item> {
        self.first().map(|&t| t)
    }

    fn pop(&mut self) -> Option<Self::Item> {
        self.pop()
    }

    fn tokens(&self) -> Tokens<Self::Item> {
        Tokens::new(self.iter().map(|t| *t))
    }

    fn range(&mut self, idx: usize) -> Option<Self::Range> {
        if idx > self.len() {
            None
        } else {
            let range = self.split_off(idx);
            Some(mem::replace(self, range))
        }
    }

    fn position(&self) -> Self::Position {
        NullPosition(())
    }
}

impl<T> RangeStream for Vec<T>
where
    T: Copy + PartialEq + Debug,
{
    fn len(&self) -> usize {
        Vec::len(self)
    }
}

impl<'a, T> Stream for &'a [T]
where
    T: Copy + PartialEq + Debug,
{
    type Item = T;
    type Range = Self;
    type Position = NullPosition;

    fn peek(&self) -> Option<Self::Item> {
        self.first().map(|&t| t)
    }

    fn pop(&mut self) -> Option<Self::Item> {
        self.split_first().map(|(head, tail)| {
            *self = &tail;
            *head
        })
    }

    fn tokens(&self) -> Tokens<Self::Item> {
        Tokens::new(self.iter().map(|t| *t))
    }

    fn range(&mut self, idx: usize) -> Option<Self::Range> {
        if idx > self.len() {
            None
        } else {
            let (head, tail) = self.split_at(idx);
            *self = &tail;
            Some(head)
        }
    }

    fn position(&self) -> Self::Position {
        NullPosition(())
    }
}

impl<'a, T> RangeStream for &'a [T]
where
    T: Copy + PartialEq + Debug,
{
    fn len(&self) -> usize {
        <[T]>::len(self)
    }
}