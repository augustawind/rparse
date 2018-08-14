use std::fmt;
use std::str::FromStr;

type Result<T> = ::std::result::Result<T, Error>;

#[derive(Debug, PartialEq)]
pub enum Error {
    Syntax(&'static str),
    Parity(char),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Syntax(s) => write!(f, "syntax error: {}", s),
            Error::Parity(op) => write!(f, "not enough arguments for operator '{}'", op),
        }
    }
}

impl ::std::error::Error for Error {}

impl From<::std::num::ParseFloatError> for Error {
    fn from(_: ::std::num::ParseFloatError) -> Self {
        Error::Syntax("invalid number")
    }
}

pub enum Token {
    BinaryOp(char),
    Number(f64),
}

impl FromStr for Token {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "+" => Ok(Token::BinaryOp('+')),
            "-" => Ok(Token::BinaryOp('-')),
            "/" => Ok(Token::BinaryOp('/')),
            "*" => Ok(Token::BinaryOp('*')),
            n => Ok(Token::Number(n.parse()?)),
        }
    }
}

pub fn lex(s: &str) -> Result<Vec<Token>> {
    s.split_whitespace().try_fold(Vec::new(), |mut tokens, s| {
        tokens.push(s.parse()?);
        Ok(tokens)
    })
}

pub fn parse(tokens: Vec<Token>) -> Result<f64> {
    let stack: Result<Vec<f64>> = tokens.iter().try_fold(Vec::new(), |mut stack, t| {
        let n = match *t {
            Token::BinaryOp(op) => {
                let (x, y) = (
                    stack.pop().ok_or_else(|| Error::Parity(op))?,
                    stack.pop().ok_or_else(|| Error::Parity(op))?,
                );
                match op {
                    '+' => x + y,
                    '-' => x - y,
                    '*' => x * y,
                    '/' => x / y,
                    _ => unimplemented!(),
                }
            }
            Token::Number(n) => n,
        };
        stack.push(n);
        Ok(stack)
    });
    Ok(stack?
        .pop()
        .expect("should be at least 1 item on the stack"))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(lex("2 3 +").and_then(parse), Ok(5.0));
        assert_eq!(lex("2 3 -").and_then(parse), Ok(1.0));
        assert_eq!(lex("2 3 *").and_then(parse), Ok(6.0));
        assert_eq!(lex("2 3 /").and_then(parse), Ok(3.0 / 2.0));
    }
}
