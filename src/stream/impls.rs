use super::{NullPosition, Stream, StreamRange, Tokens};

impl<'a> StreamRange for &'a str {
    fn len(&self) -> usize {
        <str>::len(self)
    }
}

impl<'a> Stream for &'a str {
    type Stream = Self;
    type Position = NullPosition;
    type Item = char;
    type Range = Self;

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

    fn position(&self) -> &Self::Position {
        &NullPosition(())
    }
}

impl<'a> StreamRange for &'a [u8] {
    fn len(&self) -> usize {
        <[u8]>::len(self)
    }
}

impl<'a> Stream for &'a [u8] {
    type Stream = Self;
    type Position = NullPosition;
    type Item = u8;
    type Range = Self;

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
        Tokens::new(self.iter().cloned())
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

    fn position(&self) -> &Self::Position {
        &NullPosition(())
    }
}
