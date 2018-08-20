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
            _ => i.err(Error::end()),
        }
    }
}

pub fn token<I: Input>(c: I::Item) -> Token<I> {
    Token { c: c }
}

#[derive(Copy, Clone)]
pub struct Many<P> {
    p: P,
    min: usize,
    max: Option<usize>,
}

// impl<P> Parser for Many<P>
// where
//     P: Parser,
// {
//     type Input = P::Input;
//     type Output = Vec<P::Output>;

//     fn parse(&mut self, mut i: Self::Input) -> ParseResult<Self::Input, Self::Output> {
//         let p = self.p.parse(i);
//         // TODO
//     }
// }

pub fn many<P>(p: P) -> Many<P>
where
    P: Parser,
{
    Many {
        p,
        min: 0,
        max: None,
    }
}

pub fn any<I: Input>(mut i: I) -> ParseResult<I, I::Item> {
    match i.pop() {
        Some(t) => i.ok(t),
        None => i.err(Error::end()),
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
        _ => i.err(Error::end()),
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
            _ => i.err(Error::end()),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_token() {
        let mut parser = token('c');
        assert_eq!(parser.parse("cat"), (Ok('c'), "at"));
        assert_eq!(parser.parse("ace").1, "ace");
    }

    #[test]
    fn test_any() {
        let input = "hello, world.";
        assert_eq!(any(input), (Ok('h'), "ello, world."));
    }

    #[test]
    fn test_cond() {
        let input = "123abc";
        assert_eq!(cond(input, |&c| c.is_numeric()), (Ok('1'), "23abc"));
        let input = "123abc";
        assert_eq!(cond(input, |&c| c.is_alphabetic()).1, input);
    }
}
