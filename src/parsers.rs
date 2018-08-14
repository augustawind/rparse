use parser::{Error, Input, ParseResult, Parser};

pub fn any<I: Input>(mut i: I) -> ParseResult<I, I::Token> {
    match i.pop() {
        Some(t) => i.ok(t),
        None => i.err(Error::eof()),
    }
}

pub fn cond<I: Input, F>(mut i: I, f: F) -> ParseResult<I, I::Token>
where
    F: Fn(&I::Token) -> bool,
{
    match i.peek() {
        Some(ref t) if f(t) => {
            i.pop();
            i.ok(*t)
        }
        _ => i.err(Error::eof()),
    }
}

pub struct CondParser<I: Input>(Fn(&I::Token) -> bool);

impl<I: Input> Parser for CondParser<I> {
    type Input = I;
    type Output = I::Token;

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
        let (input, _) = cond(input, |&c| c.is_alphabetic()).result().unwrap_err();
        assert_eq!(input, "123abc");
    }
}
