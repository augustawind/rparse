use super::{NullPosition, RangeStream, Stream, StreamItem, Tokens};

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

impl<'a> RangeStream for &'a str {
    fn empty() -> Self {
        ""
    }
    fn len(&self) -> usize {
        <str>::len(self)
    }
    fn from_str(s: &'static str) -> Self {
        s
    }
    fn into_string(self) -> Result<String, Self> {
        Ok(String::from(self))
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

    fn range(&mut self, to_idx: usize) -> Option<Self::Range> {
        (to_idx <= self.len()).then(|| {
            let (head, tail) = self.split_at(to_idx);
            *self = &tail;
            head
        })
    }

    fn as_range(&mut self) -> Self::Range {
        let range = &self[..];
        *self = "";
        range
    }

    fn position(&self) -> &Self::Position {
        &NullPosition
    }
}

impl<'a> RangeStream for &'a [u8] {
    fn empty() -> Self {
        &[]
    }
    fn len(&self) -> usize {
        <[u8]>::len(self)
    }
    fn from_str(s: &'static str) -> Self {
        s.as_ref()
    }
    fn into_string(self) -> Result<String, Self> {
        String::from_utf8(self.to_vec()).map_err(|_| self)
    }
}

impl<'a> Stream for &'a [u8] {
    type Stream = Self;
    type Position = NullPosition;
    type Item = u8;
    type Range = Self;

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
        Tokens::new(self.iter().cloned())
    }

    fn range(&mut self, to_idx: usize) -> Option<Self::Range> {
        (to_idx <= self.len()).then(|| {
            let (head, tail) = self.split_at(to_idx);
            *self = &tail;
            head
        })
    }

    fn as_range(&mut self) -> Self::Range {
        let range = &self[..];
        *self = &[][..];
        range
    }

    fn position(&self) -> &Self::Position {
        &NullPosition
    }
}
