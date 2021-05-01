#![feature(iter_intersperse)]

#[macro_use]
extern crate rparse;

use std::fmt::{self, Write};
use std::str::FromStr;

use rparse::parser::{
    choice::optional,
    item::{ascii, eoi_, item},
    parser,
    repeat::{many, many1},
};
use rparse::stream::IndexedStream;
use rparse::traits::StrLike;
use rparse::{Error, Parser, Stream};

macro_rules! fail {
    ($msg:expr $(, $args:expr)* $(,)*) => {{
        println!(::std::concat!("error: ", $msg) $(, $args)*);
        ::std::process::exit(1);
    }}
}

fn main() {
    let input: String = std::env::args()
        .skip(1)
        .intersperse(" ".to_string())
        .collect();
    if input.is_empty() {
        fail!("input needed");
    }

    match rpn().must_parse(IndexedStream::from(input.as_bytes())) {
        Ok((result, _)) => println!("{} = {}", input, result),
        Err((err, _)) => fail!("parsing failed: {}", err),
    };
}

enum Token {
    Op(Op),
    Number(f64),
}

#[derive(Clone, Copy)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

impl Op {
    fn eval<S: Stream>(&self, stack: &mut Vec<f64>) -> Result<f64, Error<S>> {
        fn error<S: Stream>(op: &Op) -> Error<S> {
            Error::from(format!("operator '{}'", op)).expected("a number")
        }
        let (x, y) = (
            stack.pop().ok_or_else(|| error(self))?,
            stack.pop().ok_or_else(|| error(self))?,
        );
        Ok(match self {
            Op::Add => x + y,
            Op::Sub => x - y,
            Op::Mul => x * y,
            Op::Div => x / y,
        })
    }
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
        match self {
            Op::Add => f.write_char('+'),
            Op::Sub => f.write_char('-'),
            Op::Mul => f.write_char('*'),
            Op::Div => f.write_char('/'),
        }
    }
}

fn rpn<S>() -> impl Parser<Stream = S, Output = f64>
where
    S: Stream,
{
    tokens()
        .skip(many::<Vec<_>, _>(ascii::whitespace()))
        .skip(eoi_().no_expect())
        .and_then(|tokens, stream: S| {
            match tokens
                .iter()
                .try_fold(Vec::new(), |mut stack, t| -> Result<_, Error<S>> {
                    let n = match *t {
                        Token::Op(ref op) => op.eval(&mut stack)?,
                        Token::Number(n) => n,
                    };
                    stack.push(n);
                    Ok(stack)
                }) {
                Ok(mut stack) => {
                    if stack.len() > 1 {
                        return stream.err(Error::from(format!(
                            "items still on stack: {}",
                            stack
                                .into_iter()
                                .map(|t| t.to_string())
                                .intersperse(" ".to_string())
                                .collect::<String>()
                        )));
                    }
                    match stack.pop() {
                        None => stream.err(Error::eoi().expected("a number")),
                        Some(n) => stream.ok(n),
                    }
                }
                Err(err) => stream.err(err),
            }
        })
}

fn tokens<S>() -> impl Parser<Stream = S, Output = Vec<Token>>
where
    S: Stream,
{
    concat![token().wrap(), many(sep().with(token()))]
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
