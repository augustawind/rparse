use parser::{Error, Input, ParseResult, Parser};

#[derive(Copy, Clone)]
pub struct Token<I: Input> {
    c: I::Item,
}

impl<I: Input> Parser for Token<I>
where
    I::Item: PartialEq,
{
    type Input = I;
    type Output = I::Item;

    fn parse(&mut self, mut i: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        match i.peek() {
            Some(item) if item == self.c => {
                i.pop();
                i.ok(item)
            }
            _ => i.err(Error::eof()),
        }
    }
}

pub fn token<I: Input>(c: I::Item) -> Token<I> {
    Token { c: c }
}

pub fn any<I: Input>(mut i: I) -> ParseResult<I, I::Item> {
    match i.pop() {
        Some(t) => i.ok(t),
        None => i.err(Error::eof()),
    }
}

pub fn cond<I: Input, F>(mut i: I, f: F) -> ParseResult<I, I::Item>
where
    F: Fn(&I::Item) -> bool,
{
    match i.peek() {
        Some(ref t) if f(t) => {
            i.pop();
            i.ok(*t)
        }
        _ => i.err(Error::eof()),
    }
}

pub struct CondParser<I: Input>(Fn(&I::Item) -> bool);

impl<I: Input> Parser for CondParser<I> {
    type Input = I;
    type Output = I::Item;

    fn parse(&mut self, mut i: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        match i.peek() {
            Some(ref t) if self.0(t) => {
                i.pop();
                i.ok(*t)
            }
            _ => i.err(Error::eof()),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_token() {
        let mut parser = token('c');
        assert_eq!(parser.parse("cat"), ParseResult::Ok((("at"), 'c')));
        assert_eq!(parser.parse("ace").input(), &"ace");
    }

    #[test]
    fn test_any() {
        let input = "hello, world.";
        assert_eq!(any(input), ParseResult::Ok(("ello, world.", 'h')));
    }

    #[test]
    fn test_cond() {
        let input = "123abc";
        assert_eq!(
            cond(input, |&c| c.is_numeric()),
            ParseResult::Ok(("23abc", '1'))
        );
        let input = "123abc";
        assert_eq!(cond(input, |&c| c.is_alphabetic()).input(), &input);
    }
}
