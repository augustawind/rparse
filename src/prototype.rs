use std::fmt;
use std::str::FromStr;

type Result<T> = ::std::result::Result<T, Error>;

#[derive(Debug, PartialEq)]
pub enum Error {
    Syntax(&'static str),
    Arity(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Syntax(s) => write!(f, "syntax error: {}", s),
            Error::Arity(op) => write!(f, "not enough arguments for operator '{}'", op),
        }
    }
}

impl ::std::error::Error for Error {}

impl From<::std::num::ParseFloatError> for Error {
    fn from(_: ::std::num::ParseFloatError) -> Self {
        Error::Syntax("invalid number")
    }
}

pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl BinOp {
    fn eval(&self, x: f64, y: f64) -> f64 {
        match self {
            BinOp::Add => x + y,
            BinOp::Sub => x - y,
            BinOp::Mul => x * y,
            BinOp::Div => x / y,
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinOp::Add => '+',
                BinOp::Sub => '-',
                BinOp::Mul => '*',
                BinOp::Div => '/',
            }
        )
    }
}

pub enum Token {
    BinOp(BinOp),
    Number(f64),
}

impl FromStr for Token {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "+" => Ok(Token::BinOp(BinOp::Add)),
            "-" => Ok(Token::BinOp(BinOp::Sub)),
            "*" => Ok(Token::BinOp(BinOp::Mul)),
            "/" => Ok(Token::BinOp(BinOp::Div)),
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
            Token::BinOp(ref op) => {
                let (x, y) = (
                    stack.pop().ok_or_else(|| Error::Arity(op.to_string()))?,
                    stack.pop().ok_or_else(|| Error::Arity(op.to_string()))?,
                );
                op.eval(x, y)
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

pub fn rpn(s: &str) -> Result<f64> {
    lex(s).and_then(parse)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(rpn("2 3 +"), Ok(5.0));
        assert_eq!(rpn("2 3 -"), Ok(1.0));
        assert_eq!(rpn("2 3 *"), Ok(6.0));
        assert_eq!(rpn("2 3 /"), Ok(3.0 / 2.0));
    }
}
