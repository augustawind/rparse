use super::{NullPosition, Stream, StreamItem, StreamRange, Tokens};

macro_rules! impl_StreamItem {
    ($T:ty) => {
        impl StreamItem for $T {
            fn is_ascii(&self) -> bool {
                <$T>::is_ascii(self)
            }
            fn is_ascii_alphabetic(&self) -> bool {
                <$T>::is_ascii_alphabetic(self)
            }
            fn is_ascii_alphanumeric(&self) -> bool {
                <$T>::is_ascii_alphanumeric(self)
            }
            fn is_ascii_digit(&self) -> bool {
                <$T>::is_ascii_digit(self)
            }
            fn is_ascii_hexdigit(&self) -> bool {
                <$T>::is_ascii_hexdigit(self)
            }
            fn is_ascii_punctuation(&self) -> bool {
                <$T>::is_ascii_punctuation(self)
            }
            fn is_ascii_graphic(&self) -> bool {
                <$T>::is_ascii_graphic(self)
            }
            fn is_ascii_whitespace(&self) -> bool {
                <$T>::is_ascii_whitespace(self)
            }
            fn is_ascii_control(&self) -> bool {
                <$T>::is_ascii_control(self)
            }
            fn is_ascii_uppercase(&self) -> bool {
                <$T>::is_ascii_uppercase(self)
            }
            fn is_ascii_lowercase(&self) -> bool {
                <$T>::is_ascii_lowercase(self)
            }
            fn eq_ignore_ascii_case(&self, other: &$T) -> bool {
                <$T>::eq_ignore_ascii_case(self, other)
            }
        }
    };
}

impl_StreamItem!(char);
impl_StreamItem!(u8);

impl<'a> StreamRange for &'a str {
    fn len(&self) -> usize {
        <str>::len(self)
    }
    fn from_str(s: &'static str) -> Self {
        s
    }
    fn item_from_byte(b: u8) -> Self::Item {
        b.into()
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
    fn from_str(s: &'static str) -> Self {
        s.as_ref()
    }
    fn item_from_byte(b: u8) -> Self::Item {
        b
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
