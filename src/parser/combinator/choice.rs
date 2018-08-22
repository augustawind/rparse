use input::Input;
use parser::{Error, Info, ParseResult, Parser};

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

    fn parse(&mut self, mut input: Self::Input) -> ParseResult<Self::Input, Self::Output> {
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
    ($head : expr) => {
        $head
    };
    ($head : expr, $($tail : expr),+) => {
        or($head, choice!($($tail),+))
    };
}

#[cfg(test)]
mod test {
    use super::*;
    use parser::*;

    //     #[test]
    //     fn test_choice() {
    //         assert_eq!(
    //             choice(vec![Box::new(token('a')), Box::new(token('b'))]).parse("a"),
    //             (Ok('a'), "")
    //         );

    //         let mut parser = choice(vec![
    //             Box::new(token('a')),
    //             Box::new(token('b')),
    //             Box::new(token('c')),
    //         ]);
    //         assert_eq!(parser.parse("bcd"), (Ok('b'), "cd"));
    //         assert_eq!(parser.parse("def").1, "def");

    //         let mut parser = choice(vec![Box::new(ascii::letter())]);
    //         assert_eq!(parser.parse("Z"), (Ok('Z'), ""));
    //         assert_eq!(parser.parse("9").1, "9");

    //         let mut parser = choice(vec![
    //             Box::new(many1(ascii::digit())),
    //             Box::new(sep_by(ascii::digit(), ascii::whitespace())),
    //         ]);
    //         assert_eq!(parser.parse("123 45 6"), (Ok("123".to_string()), " 45 6"));
    //         assert_eq!(parser.parse(" 1 2 3"), (Ok("123".to_string()), ""));
    //     }

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
    fn test_choice_macro() {
        let mut parser = choice!(token('a'), ascii::digit(), ascii::punctuation());
        assert_eq!(parser.parse("a9."), (Ok('a'), "9."));
        assert_eq!(parser.parse("9.a"), (Ok('9'), ".a"));
        assert_eq!(parser.parse(".a9"), (Ok('.'), "a9"));
        assert_eq!(parser.parse("ba9.").1, "ba9.");
    }
}
