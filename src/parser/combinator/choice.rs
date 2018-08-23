use error::{Error, Info, ParseResult};
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

    fn parse_input(&mut self, mut input: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        match self.left.parse(input) {
            (Ok(result), remaining) => return (Ok(result), remaining),
            (Err(Error::Expected(_)), remaining) => {
                input = remaining;
            }
            err => return err,
        };
        match self.right.parse(input) {
            (Ok(result), remaining) => (Ok(result), remaining),
            (Err(Error::Expected(_)), remaining) => {
                let err = Error::Expected(Info::Description("none of the given parsers matched"));
                (Err(err), remaining)
            }
            err => err,
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
        or($head, choice!($($tail),+))
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
        assert_eq!(parser.parse("def").1, "def");

        assert_parse_err!(parser.parse("aab"), "aab");
        assert_parse_err!(parser.parse("bcd"), "bcd");

        let mut parser = and(many1(ascii::digit()), many1(ascii::letter()));
        assert_eq!(parser.parse("123abc456"), (Ok(vec!['a', 'b', 'c']), "456"));
        assert_eq!(parser.parse(" 1 2 3").1, " 1 2 3");

        assert_parse_err!(parser.parse("123 abc"), "123 abc");
    }

    #[test]
    fn test_or() {
        let mut parser = or(token('a'), token('b'));
        assert_eq!(parser.parse("bcd"), (Ok('b'), "cd"));
        assert_eq!(parser.parse("a"), (Ok('a'), ""));
        assert_eq!(parser.parse("def").1, "def");

        let mut parser = or(
            many1(ascii::digit()),
            sep_by(ascii::digit(), ascii::whitespace()),
        );
        assert_eq!(parser.parse("123 45 6"), (Ok(vec!['1', '2', '3']), " 45 6"));
        assert_eq!(parser.parse(" 1 2 3"), (Ok(vec!['1', '2', '3']), ""));
    }

    #[test]
    fn test_choice() {
        assert_eq!(choice!(token('a'), token('b')).parse("a"), (Ok('a'), ""));

        let mut parser = choice!(token('a'), ascii::digit(), ascii::punctuation());
        assert_eq!(parser.parse("a9."), (Ok('a'), "9."));
        assert_eq!(parser.parse("9.a"), (Ok('9'), ".a"));
        assert_eq!(parser.parse(".a9"), (Ok('.'), "a9"));
        assert_eq!(parser.parse("ba9.").1, "ba9.");

        let mut parser = choice!(token('a'), token('b'), token('c'));
        assert_eq!(parser.parse("bcd"), (Ok('b'), "cd"));
        assert_eq!(parser.parse("def").1, "def");

        let mut parser = choice!(ascii::letter());
        assert_eq!(parser.parse("Z"), (Ok('Z'), ""));
        assert_eq!(parser.parse("9").1, "9");

        let mut parser = choice!(
            many1(ascii::digit()),
            sep_by(ascii::digit(), ascii::whitespace()),
        );
        assert_eq!(parser.parse("123 45 6"), (Ok("123".to_string()), " 45 6"));
        assert_eq!(parser.parse(" 1 2 3"), (Ok("123".to_string()), ""));
    }
}
