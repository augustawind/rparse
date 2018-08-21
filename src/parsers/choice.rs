use parser::{Error, Info, Input, ParseResult, Parser};

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

pub struct Choice<I: Input, O> {
    parsers: Vec<Box<Parser<Input = I, Output = O>>>,
}

impl<I: Input, O> Parser for Choice<I, O> {
    type Input = I;
    type Output = O;

    fn parse(&mut self, mut input: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        for ref mut parser in self.parsers.iter_mut() {
            match parser.parse(input) {
                (Ok(result), remaining) => return (Ok(result), remaining),
                (Err(Error::Expected(_)), remaining) => {
                    input = remaining;
                    continue;
                }
                (Err(err), remaining) => return (Err(err), remaining),
            }
        }
        (
            Err(Error::Expected(Info::Description(
                "none of the given parsers matched",
            ))),
            input,
        )
    }
}

pub fn choice<I: Input, O>(parsers: Vec<Box<Parser<Input = I, Output = O>>>) -> Choice<I, O> {
    Choice { parsers }
}

#[cfg(test)]
mod test {
    use super::*;
    use parsers::*;

    #[test]
    fn test_choice() {
        assert_eq!(
            choice(vec![
                Box::new(token('a')),
                Box::new(token('b')),
                Box::new(token('c')),
            ]).parse("bcd"),
            (Ok('b'), "cd")
        );
        assert_eq!(
            choice(vec![Box::new(token('a')), Box::new(token('b'))]).parse("a"),
            (Ok('a'), "")
        );
        assert_eq!(
            choice(vec![
                Box::new(token('a')),
                Box::new(token('b')),
                Box::new(token('c')),
            ]).parse("def")
                .1,
            "def"
        );
        let mut parser = choice(vec![
            Box::new(many1(ascii::digit())),
            Box::new(sep_by(ascii::digit(), ascii::whitespace())),
        ]);
        assert_eq!(parser.parse("123 45 6"), (Ok("123".to_string()), " 45 6"));
    }

    #[test]
    fn test_or() {
        assert_eq!(or(token('a'), token('b')).parse("bcd"), (Ok('b'), "cd"));
        assert_eq!(or(token('a'), token('b')).parse("a"), (Ok('a'), ""));
        assert_eq!(or(token('a'), token('b')).parse("def").1, "def");
        let mut parser = or(
            many1(ascii::digit()),
            sep_by(ascii::digit(), ascii::whitespace()),
        );
        assert_eq!(parser.parse("123 45 6"), (Ok("123".to_string()), " 45 6"));
    }
}
