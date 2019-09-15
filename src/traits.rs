use std::str;

pub trait AsBytes {
    fn as_bytes(&self) -> &[u8];
}

impl AsBytes for str {
    fn as_bytes(&self) -> &[u8] {
        <str>::as_bytes(self)
    }
}

impl AsBytes for [u8] {
    fn as_bytes(&self) -> &[u8] {
        &self
    }
}

pub trait StrLike {
    fn from_utf8(&self) -> Result<&str, ()>;
}

impl StrLike for String {
    fn from_utf8(&self) -> Result<&str, ()> {
        Ok(self)
    }
}

impl<'a> StrLike for &'a str {
    fn from_utf8(&self) -> Result<&str, ()> {
        Ok(*self)
    }
}

impl StrLike for str {
    fn from_utf8(&self) -> Result<&str, ()> {
        Ok(self)
    }
}

impl StrLike for Vec<u8> {
    fn from_utf8(&self) -> Result<&str, ()> {
        (**self).from_utf8()
    }
}

impl<'a> StrLike for &'a [u8] {
    fn from_utf8(&self) -> Result<&str, ()> {
        (**self).from_utf8()
    }
}

impl StrLike for [u8] {
    fn from_utf8(&self) -> Result<&str, ()> {
        str::from_utf8(self).map_err(|_| ())
    }
}

pub trait Maybe<T> {
    fn from_result<E>(Result<T, E>) -> Self;
}

impl<T> Maybe<T> for Vec<T> {
    fn from_result<E>(r: Result<T, E>) -> Self {
        r.into_iter().collect()
    }
}

impl<T> Maybe<T> for Option<T> {
    fn from_result<E>(r: Result<T, E>) -> Self {
        r.ok()
    }
}

impl<T> Maybe<T> for String
where
    T: ToString,
{
    fn from_result<E>(r: Result<T, E>) -> Self {
        r.map(|t| t.to_string()).unwrap_or_default()
    }
}

// pub trait Sequence {
//     type Item;
//     fn new() -> Self;
//     fn len(&self) -> usize;
//     fn get(&self, idx: usize) -> Option<Self::Item>;
//     fn insert(&mut self, idx: usize, item: Self::Item);
//     fn remove(&mut self, idx: usize) -> Option<Self::Item>;

//     fn is_empty(&self) -> bool {
//         self.len() == 0
//     }
//     fn first(&self) -> Option<Self::Item> {
//         self.get(0)
//     }
//     fn last(&self) -> Option<Self::Item> {
//         self.get(self.len().wrapping_sub(1))
//     }
//     fn push(&mut self, item: Self::Item) {
//         let len = self.len();
//         self.insert(len, item);
//     }
//     fn pop(&mut self) -> Option<Self::Item> {
//         let len = self.len();
//         self.remove(len.wrapping_sub(1))
//     }
// }

// pub trait Iter: Sequence {
//     fn iter<'a, I>(&'a self) -> I
//     where
//         I: Iterator<Item = &'a Self::Item>;
//     fn iter_mut<'a, I>(&'a mut self) -> I
//     where
//         I: Iterator<Item = &'a mut Self::Item>;
// }

// impl Sequence for String {
//     type Item = char;
//     fn new() -> Self {
//         String::new()
//     }
//     fn len(&self) -> usize {
//         String::len(self)
//     }
//     fn get(&self, idx: usize) -> Option<Self::Item> {
//         self.chars().nth(idx)
//     }
//     fn insert(&mut self, idx: usize, item: Self::Item) {
//         String::insert(self, idx, item)
//     }
//     fn remove(&mut self, idx: usize) -> Option<Self::Item> {
//         if idx >= self.len() {
//             None
//         } else {
//             Some(String::remove(self, idx))
//         }
//     }
//     fn push(&mut self, item: Self::Item) {
//         String::push(self, item)
//     }
//     fn pop(&mut self) -> Option<Self::Item> {
//         String::pop(self)
//     }
// }
