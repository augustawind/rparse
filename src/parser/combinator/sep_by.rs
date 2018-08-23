use std::iter::FromIterator;
use std::marker::PhantomData;

use error::ParseResult;
use input::Input;
use parser::Parser;

pub struct SepBy<P, S, O> {
    parser: P,
    separator: S,
    // min: usize,
    __marker: PhantomData<O>,
}

impl<P, S, O> Parser for SepBy<P, S, O>
where
    P: Parser,
    S: Parser<Input = P::Input, Output = P::Output>,
    O: FromIterator<P::Output>,
{
    type Input = P::Input;
    type Output = O;

    fn parse_input(&mut self, mut input: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        let mut output = Vec::new();
        loop {
            input = match self.parser.parse(input) {
                (Ok(result), input) => {
                    output.push(result);

                    // TODO: use .skip here
                    match self.separator.parse(input) {
                        (Ok(_), input) => input,
                        (Err(_), input) => {
                            break input.ok(output.into_iter().collect());
                        }
                    }
                }
                (Err(_), input) => {
                    break input.ok(output.into_iter().collect());
                }
            }
        }
    }
}

pub fn sep_by<P, S, O>(parser: P, separator: S) -> SepBy<P, S, O>
where
    P: Parser,
    S: Parser<Input = P::Input>,
    O: FromIterator<P::Output>,
{
    SepBy {
        parser,
        separator,
        __marker: PhantomData,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use parser::combinator::many1;
    use parser::token::ascii::*;

    #[test]
    fn test_sep_by() {
        assert_eq!(
            sep_by(many1(digit()), many1(whitespace())).parse("1 23\t4\n\nabc"),
            (
                Ok(vec!["1".to_string(), "23".to_string(), "4".to_string()]),
                "abc"
            )
        );
    }
}
