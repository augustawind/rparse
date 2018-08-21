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

pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

impl Op {
    fn eval(&self, stack: &mut Vec<f64>) -> Result<f64> {
        let (x, y) = (
            stack.pop().ok_or_else(|| Error::Arity(self.to_string()))?,
            stack.pop().ok_or_else(|| Error::Arity(self.to_string()))?,
        );
        Ok(match self {
            Op::Add => x + y,
            Op::Sub => x - y,
            Op::Mul => x * y,
            Op::Div => x / y,
        })
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::Add => '+',
                Op::Sub => '-',
                Op::Mul => '*',
                Op::Div => '/',
            }
        )
    }
}

pub enum Token {
    Op(Op),
    Number(f64),
}

impl FromStr for Token {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "+" => Ok(Token::Op(Op::Add)),
            "-" => Ok(Token::Op(Op::Sub)),
            "*" => Ok(Token::Op(Op::Mul)),
            "/" => Ok(Token::Op(Op::Div)),
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
            Token::Op(ref op) => op.eval(&mut stack)?,
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

fn main() {
    use std::env;
    let eq = match env::args().nth(1) {
        Some(eq) => eq,
        None => {
            println!("missing argument");
            return;
        }
    };
    match rpn(eq.as_str()) {
        Ok(result) => println!("({}) = {}", eq, result),
        Err(error) => println!("{}", error.to_string()),
    };
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
