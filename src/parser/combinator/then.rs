use std::iter;
use std::marker::PhantomData;

use error::ParseResult;
use input::Input;
use parser::Parser;

pub struct Then<L, R, S> {
    left: L,
    right: R,
    __marker: PhantomData<S>,
}

impl<I: Input, O, S, L, R> Parser for Then<L, R, S>
where
    L: Parser<Input = I, Output = O>,
    R: Parser<Input = I, Output = O>,
    S: iter::FromIterator<O>,
{
    type Input = I;
    type Output = S;

    fn parse_input(&mut self, input: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        match self.left.parse(input) {
            (Ok(first), input) => match self.right.parse(input) {
                (Ok(second), input) => {
                    let result: S = iter::once(first).chain(iter::once(second)).collect();
                    input.ok(result)
                }
                (Err(err), input) => input.err(err),
            },
            (Err(err), input) => input.err(err),
        }
    }
}

pub fn then<I: Input, O, S, L, R>(left: L, right: R) -> Then<L, R, S>
where
    L: Parser<Input = I, Output = O>,
    R: Parser<Input = I, Output = O>,
    S: iter::FromIterator<O>,
{
    Then {
        left,
        right,
        __marker: PhantomData,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use error::Error;
    use input::IndexedInput;
    use parser::token::token;

    #[test]
    fn test_then() {
        // TODO: errors should show where the error occured
        test_parser!(IndexedInput<&str> | token('X').then::<_, String>(token('O')), {
            "XO" => (Ok("XO".to_string()), "", 2),
            "XOXO" => (Ok("XO".to_string()), "XO", 2),
            "XY" => (Err(Error::expected_token('O')), "XY", 0),
            "ZY" => (Err(Error::expected_token('X')), "ZY", 0),
        });
    }
}
