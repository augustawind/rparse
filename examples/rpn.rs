#![feature(iter_intersperse)]

#[macro_use]
extern crate rparse;

use std::fmt;
use std::str::FromStr;

use rparse::parser::choice::optional;
use rparse::parser::item::{ascii, item};
use rparse::parser::parser;
use rparse::parser::repeat::{many, many1};
use rparse::traits::StrLike;
use rparse::{Parser, Stream};

macro_rules! fail {
    ($msg:expr $(, $args:expr)* $(,)*) => {{
        println!(::std::concat!("error: ", $msg) $(, $args)*);
        ::std::process::exit(1);
    }}
}

fn solve_rpn(tokens: Vec<Token>) -> Result<f64, Error> {
    let mut stack = tokens
        .iter()
        .try_fold(Vec::new(), |mut stack, t| -> Result<_, Error> {
            let n = match *t {
                Token::Op(ref op) => op.eval(&mut stack)?,
                Token::Number(n) => n,
            };
            stack.push(n);
            Ok(stack)
        })?;
    if stack.len() > 1 {
        return Err(Error::UnusedItems(stack));
    }
    stack.pop().ok_or(Error::TooFewItems)
}

fn main() {
    let input: String = std::env::args()
        .skip(1)
        .intersperse(" ".to_string())
        .collect();
    if input.is_empty() {
        fail!("input needed");
    }
    match tokens().must_parse(input.as_str()) {
        Ok((tokens, "")) => match solve_rpn(tokens) {
            Ok(result) => println!("{} = {}", input, result),
            Err(err) => fail!("{}", err),
        },
        Ok((_, rest)) => fail!(
            "invalid token '{}'",
            rest.split_ascii_whitespace().next().unwrap()
        ),
        Err((err, _)) => fail!("parsing failed: {}", err),
    };
}

#[derive(Debug, PartialEq)]
enum Error {
    Arity(Op),
    TooFewItems,
    UnusedItems(Vec<f64>),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Arity(op) => write!(f, "not enough arguments for operator '{}'", op),
            Error::TooFewItems => write!(f, "not enough items on stack"),
            Error::UnusedItems(xs) => write!(
                f,
                "unused items on stack: {}",
                xs.into_iter()
                    .map(|t| t.to_string())
                    .intersperse(" ".to_string())
                    .collect::<String>()
            ),
        }
    }
}

impl ::std::error::Error for Error {}

enum Token {
    Op(Op),
    Number(f64),
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

impl FromStr for Op {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(Op::Add),
            "-" => Ok(Op::Sub),
            "*" => Ok(Op::Mul),
            "/" => Ok(Op::Div),
            _ => Err(format!("invalid operator '{}'", s)),
        }
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

impl Op {
    fn eval(&self, stack: &mut Vec<f64>) -> Result<f64, Error> {
        let (x, y) = (
            stack.pop().ok_or_else(|| Error::Arity(*self))?,
            stack.pop().ok_or_else(|| Error::Arity(*self))?,
        );
        Ok(match self {
            Op::Add => x + y,
            Op::Sub => x - y,
            Op::Mul => x * y,
            Op::Div => x / y,
        })
    }
}

fn tokens<S>() -> impl Parser<Stream = S, Output = Vec<Token>>
where
    S: Stream,
{
    concat![token().wrap(), many(sep().with(token())),]
}

fn token<S>() -> impl Parser<Stream = S, Output = Token>
where
    S: Stream,
{
    op().map(Token::Op).or(number().map(Token::Number))
}

fn op<S>() -> impl Parser<Stream = S, Output = Op>
where
    S: Stream,
    S::Range: StrLike,
{
    parser(|mut s: S| {
        let range = s.range(1);
        s.result(range)
    })
    .from_str()
}

fn number<S>() -> impl Parser<Stream = S, Output = f64>
where
    S: Stream,
{
    concat![
        many1(ascii::digit()),
        optional(item(b'.').wrap().extend(many1(ascii::digit())))
    ]
    .collect_string()
    .from_str()
}

fn sep<S>() -> impl Parser<Stream = S, Output = Vec<S::Item>>
where
    S: Stream,
{
    many1(ascii::whitespace())
}
