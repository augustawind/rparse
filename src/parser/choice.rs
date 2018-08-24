use error::{Error, ParseResult};
use input::Input;
use parser::Parser;

pub struct And<L, R> {
    left: L,
    right: R,
}

impl<I: Input, O, L, R> Parser for And<L, R>
where
    L: Parser<Input = I, Output = O>,
    R: Parser<Input = I, Output = O>,
{
    type Input = I;
    type Output = O;

    fn parse_input(&mut self, input: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        match self.left.parse(input) {
            (Ok(_), remaining) => self.right.parse(remaining),
            err => err,
        }
    }
}

pub fn and<I: Input, O, L, R>(left: L, right: R) -> And<L, R>
where
    L: Parser<Input = I, Output = O>,
    R: Parser<Input = I, Output = O>,
{
    And { left, right }
}

pub struct Or<L, R> {
    left: L,
    right: R,
}

impl<I: Input, O, L, R> Parser for Or<L, R>
where
    L: Parser<Input = I, Output = O>,
    R: Parser<Input = I, Output = O>,
{
    type Input = I;
    type Output = O;

    fn parse_input(&mut self, input: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        let (err, input) = match self.left.parse(input) {
            (Ok(result), input) => return input.ok(result),
            (Err(Error::EOF), input) => return input.err(Error::EOF),
            (Err(err), input) => (err, input),
        };
        match self.right.parse(input) {
            (Ok(result), input) => input.ok(result),
            (Err(Error::EOF), input) => input.err(err),
            (Err(err), input) => input.err(err),
        }
    }
}

pub fn or<I: Input, O, L, R>(left: L, right: R) -> Or<L, R>
where
    L: Parser<Input = I, Output = O>,
    R: Parser<Input = I, Output = O>,
{
    Or { left, right }
}

#[macro_export]
macro_rules! choice {
    ($head:expr) => {
        $head
    };
    ($head:expr, $($tail:expr),+ $(,)*) => {
        $head.or(choice!($($tail),+))
    };
}

#[cfg(test)]
mod test {
    use super::*;
    use parser::combinator::{many1, sep_by};
    use parser::token::{ascii, token};

    #[test]
    fn test_and() {
        let mut parser = and(token('a'), token('b'));
        assert_eq!(parser.parse("abcd"), (Ok('b'), "cd"));
        assert_eq!(parser.parse("ab"), (Ok('b'), ""));
        assert_parse_err!(parser.parse("def"), "def");
        assert_parse_err!(parser.parse("aab"), "aab");
        assert_parse_err!(parser.parse("bcd"), "bcd");

        let mut parser = and(many1(ascii::digit()), many1(ascii::letter()));
        assert_eq!(parser.parse("123abc456"), (Ok(vec!['a', 'b', 'c']), "456"));
        assert_parse_err!(parser.parse(" 1 2 3"), " 1 2 3");
        assert_parse_err!(parser.parse("123 abc"), "123 abc");
    }

    #[test]
    fn test_or() {
        let mut parser = or(token('a'), token('b'));
        assert_eq!(parser.parse("bcd"), (Ok('b'), "cd"));
        assert_eq!(parser.parse("a"), (Ok('a'), ""));
        assert_parse_err!(parser.parse("def"), "def");

        let mut parser = or(
            many1(ascii::digit()),
            sep_by(ascii::letter(), ascii::whitespace()),
        );
        assert_eq!(parser.parse("123a bc"), (Ok("123".to_string()), "a bc"));
        assert_eq!(parser.parse("a b c"), (Ok("abc".to_string()), ""));
    }

    #[test]
    fn test_choice() {
        assert_eq!(choice!(token('a'), token('b')).parse("a"), (Ok('a'), ""));

        let mut parser = choice!(token('a'), ascii::digit(), ascii::punctuation());
        assert_eq!(parser.parse("a9."), (Ok('a'), "9."));
        assert_eq!(parser.parse("9.a"), (Ok('9'), ".a"));
        assert_eq!(parser.parse(".a9"), (Ok('.'), "a9"));
        assert_parse_err!(parser.parse("ba9."), "ba9.");

        let mut parser = choice!(token('a'), token('b'), token('c'));
        assert_eq!(parser.parse("bcd"), (Ok('b'), "cd"));
        assert_parse_err!(parser.parse("def"), "def");

        let mut parser = choice!(ascii::letter());
        assert_eq!(parser.parse("Z"), (Ok('Z'), ""));
        assert_parse_err!(parser.parse("9"), "9");

        let mut parser = choice!(
            many1(ascii::digit()),
            sep_by(ascii::letter(), ascii::whitespace()),
        );
        assert_eq!(parser.parse("123a bc"), (Ok("123".to_string()), "a bc"));
        assert_eq!(parser.parse("a b c"), (Ok("abc".to_string()), ""));
    }
}
