use parser::{ParseResult, Parser};

pub struct Map<P, F> {
    parser: P,
    f: F,
}

impl<P, F, O> Parser for Map<P, F>
where
    P: Parser,
    F: Fn(P::Output) -> O,
{
    type Input = P::Input;
    type Output = O;

    fn parse(&mut self, input: Self::Input) -> ParseResult<Self::Input, O> {
        match self.parser.parse(input) {
            (Ok(result), remaining) => (Ok((self.f)(result)), remaining),
            (Err(err), remaining) => (Err(err), remaining),
        }
    }
}

pub fn map<P: Parser, F: Fn(P::Output) -> O, O>(p: P, f: F) -> Map<P, F> {
    Map { parser: p, f }
}

#[cfg(test)]
mod test {
    use super::*;
    use parsers::*;

    #[test]
    fn test_map() {
        let mut parser = map(ascii::digit(), |c: char| c.to_string());
        assert_eq!(parser.parse("3"), (Ok("3".to_string()), ""));
        assert_eq!(parser.parse("a3").1, "a3");

        let mut parser = map(many1(ascii::letter()), |s: String| s.to_uppercase());
        assert_eq!(parser.parse("aBcD12e"), (Ok("ABCD".to_string()), "12e"));

        let mut parser = map(many1(ascii::alpha_num()), |s: String| {
            s.parse::<usize>().unwrap_or(0)
        });
        assert_eq!(parser.parse("324 dogs"), (Ok(324 as usize), " dogs"));
        assert_eq!(parser.parse("324dogs"), (Ok(0 as usize), ""));
    }
}