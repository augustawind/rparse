use parser::{ParseResult, Parser};

pub struct Bind<P, F> {
    parser: P,
    f: F,
}

impl<P, F, O> Parser for Bind<P, F>
where
    P: Parser,
    F: Fn(P::Output, P::Input) -> ParseResult<P::Input, O>,
{
    type Input = P::Input;
    type Output = O;

    fn parse(&mut self, input: Self::Input) -> ParseResult<Self::Input, O> {
        match self.parser.parse(input) {
            (Ok(result), remaining) => (self.f)(result, remaining),
            (Err(err), remaining) => (Err(err), remaining),
        }
    }
}

pub fn bind<P: Parser, F: Fn(P::Output, P::Input) -> O, O>(p: P, f: F) -> Bind<P, F> {
    Bind { parser: p, f }
}

#[cfg(test)]
mod test {
    use super::*;
    use parser::Error;
    use parsers::*;

    #[test]
    fn test_bind() {
        let mut parser = bind(ascii::digit(), |c: char, rest: &str| {
            (Ok(c.to_string()), rest)
        });
        assert_eq!(parser.parse("3"), (Ok("3".to_string()), ""));
        assert_eq!(parser.parse("a3").1, "a3");

        // let mut parser = bind(many1(ascii::letter()), |s: String| s.to_uppercase());
        // assert_eq!(parser.parse("aBcD12e"), (Ok("ABCD".to_string()), "12e"));

        let mut parser = bind(many1(ascii::alpha_num()), |s: String, rest| {
            match s.parse::<usize>() {
                Ok(n) => (Ok(n), rest),
                Err(e) => (Err(Error::Other(Box::new(e))), rest),
            }
        });
        assert_eq!(parser.parse("324 dogs"), (Ok(324 as usize), " dogs"));
        // TODO: make this keep the stuff that wasn't processed:
        // assert_eq!(parser.parse("324dogs").1, "324dogs");
        assert_eq!(parser.parse("324dogs").1, "");
    }
}
