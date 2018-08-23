use std::iter::FromIterator;
use std::marker::PhantomData;

use error::{Error, ParseResult};
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
        let mut sep = true;
        loop {
            let p = if sep {
                self.separator.parse(input)
            } else {
                self.parser.parse(input)
            };
            match p {
                (Ok(result), remaining) => {
                    if !sep {
                        output.push(result);
                    }
                    input = remaining;
                    sep = !sep;
                }
                (Err(Error::Expected(_)), remaining) => {
                    input = remaining;
                    break (Ok(output.into_iter().collect()), input);
                }
                (Err(err), remaining) => {
                    break (Err(err), remaining);
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
            sep_by(many1(digit()), many1(whitespace())).parse(" 1 23\t4\n\nabc"),
            (
                Ok(vec!["1".to_string(), "23".to_string(), "4".to_string()]),
                "abc"
            )
        );
    }
}
