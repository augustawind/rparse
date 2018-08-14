pub trait Input: Sized {
    type Token: Copy;

    fn peek(&self) -> Option<Self::Token>;
    fn pop(&mut self) -> Option<Self::Token>;

    fn foreach<F>(&self, F)
    where
        F: FnMut(Self::Token);

    fn ok<O>(self, result: O) -> Result<Self, O> {
        Ok((self, result))
    }

    fn err<O>(self, error: Error) -> Result<Self, O> {
        Err((self, error))
    }
}

impl<'a> Input for &'a str {
    type Token = char;

    fn peek(&self) -> Option<Self::Token> {
        self.chars().next()
    }

    fn pop(&mut self) -> Option<Self::Token> {
        let mut iter = self.char_indices();
        iter.next().map(|(_, c)| {
            match iter.next() {
                Some((n, _)) => *self = &self[n..],
                None => *self = &self[..0],
            }

            c
        })
    }

    fn foreach<F>(&self, f: F)
    where
        F: FnMut(Self::Token),
    {
        self.chars().for_each(f);
    }
}

#[derive(Debug, PartialEq)]
pub struct Error(String);

type Result<I, O> = ::std::result::Result<(I, O), (I, Error)>;

impl Error {
    fn eof() -> Self {
        Error("unexpected end of input".to_string())
    }
}

trait Parser<I: Input, O> {
    fn parse(&self, I) -> Result<I, O>;
}

impl<I: Input, O, F> Parser<I, O> for F
where
    F: Fn(&mut I) -> Result<I, O>,
{
    fn parse(&self, mut i: I) -> Result<I, O> {
        self(&mut i)
    }
}

pub fn any<I: Input>(mut i: I) -> Result<I, I::Token> {
    match i.pop() {
        Some(t) => i.ok(t),
        None => i.err(Error::eof()),
    }
}

pub fn cond<I: Input, F>(mut i: I, f: F) -> Result<I, I::Token>
where
    F: FnOnce(I::Token) -> bool,
{
    match i.peek() {
        Some(t) if f(t) => {
            i.pop();
            i.ok(t)
        }
        _ => i.err(Error::eof()),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_any() {
        let input = "hello, world.";
        assert_eq!(any(input), Ok(("ello, world.", 'h')));
    }

    #[test]
    fn test_cond() {
        let input = "123abc";
        assert_eq!(cond(input, char::is_numeric), Ok(("23abc", '1')));
        let input = "123abc";
        let (input, _) = cond(input, char::is_alphabetic).unwrap_err();
        assert_eq!(input, "123abc");
    }
}
