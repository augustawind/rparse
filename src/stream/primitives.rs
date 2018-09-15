use std::mem;

use super::{NullPosition, RangeStream, Stream, ToStream, Tokens};

impl Stream for String {
    type Item = char;
    type Range = Self;
    type Position = NullPosition;

    fn from_token(token: Self::Item) -> Self {
        token.to_string()
    }

    fn from_range(range: Self::Range) -> Self {
        range
    }

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
        if self.len() > idx {
            return None;
        }
        let mut range = self.split_off(idx);
        mem::swap(&mut range, self);
        Some(range)
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

impl ToStream<Self> for String {
    fn to_stream(self) -> Self {
        self
    }
}

impl ToStream<String> for char {
    fn to_stream(self) -> String {
        self.to_string()
    }
}

impl<'a> ToStream<String> for &'a str {
    fn to_stream(self) -> String {
        self.to_string()
    }
}

impl<'a> Stream for &'a str {
    type Item = char;
    type Range = Self;
    type Position = NullPosition;

    fn from_token(token: Self::Item) -> Self {
        let s = token.to_string();
        unsafe {
            let stream: &'a str = mem::transmute_copy(&s.as_str());
            mem::forget(s);
            stream
        }
    }

    fn from_range(range: Self::Range) -> Self {
        range
    }

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

impl<'a> ToStream<Self> for &'a str {
    fn to_stream(self) -> Self {
        self
    }
}

impl<'a> ToStream<&'a str> for char {
    fn to_stream(self) -> &'a str {
        let s = self.to_string();
        unsafe {
            let stream: &'a str = mem::transmute_copy(&s.as_str());
            mem::forget(s);
            stream
        }
    }
}

impl<'a> Stream for &'a [u8] {
    type Item = u8;
    type Range = Self;
    type Position = NullPosition;

    fn from_token(token: Self::Item) -> Self {
        token.to_stream()
    }

    fn from_range(range: Self::Range) -> Self {
        range
    }

    fn peek(&self) -> Option<Self::Item> {
        self.first().map(|t| *t)
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

impl<'a> RangeStream for &'a [u8] {
    fn len(&self) -> usize {
        <[u8]>::len(self)
    }
}

impl<'a> ToStream<Self> for &'a [u8] {
    fn to_stream(self) -> Self {
        self
    }
}

impl<'a> ToStream<&'a [u8]> for u8 {
    fn to_stream(self) -> &'a [u8] {
        let s = vec![self];
        unsafe {
            let stream: &'a [u8] = mem::transmute_copy(&s.as_slice());
            mem::forget(s);
            stream
        }
    }
}
