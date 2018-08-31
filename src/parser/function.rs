//! Parsers that transform the output of other Parsers with arbitrary functions.

use std::fmt::Display;
use std::marker::PhantomData;
use std::str;

use error::ParseResult;
use parser::Parser;
use stream::Stream;

pub struct Map<P, F> {
    parser: P,
    f: F,
}

impl<P, F, O> Parser for Map<P, F>
where
    P: Parser,
    F: Fn(P::Output) -> O,
{
    type Stream = P::Stream;
    type Output = O;

    fn parse_stream(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.parser.parse(stream) {
            (Ok(result), remaining) => (Ok((self.f)(result)), remaining),
            (Err(err), remaining) => (Err(err), remaining),
        }
    }
}

pub fn map<P, F, O>(p: P, f: F) -> Map<P, F>
where
    P: Parser,
    F: Fn(P::Output) -> O,
{
    Map { parser: p, f }
}

pub struct Bind<P, F> {
    parser: P,
    f: F,
}

impl<P, F, O> Parser for Bind<P, F>
where
    P: Parser,
    F: Fn(P::Output, P::Stream) -> ParseResult<P::Stream, O>,
{
    type Stream = P::Stream;
    type Output = O;

    fn parse_stream(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.parser.parse(stream) {
            (Ok(result), remaining) => (self.f)(result, remaining),
            (Err(err), remaining) => (Err(err), remaining),
        }
    }
}

pub fn bind<P, F, O>(p: P, f: F) -> Bind<P, F>
where
    P: Parser,
    F: Fn(P::Output, P::Stream) -> O,
{
    Bind { parser: p, f }
}

pub trait StrLike {
    fn from_utf8(&self) -> Result<&str, ()>;
}

impl StrLike for String {
    fn from_utf8(&self) -> Result<&str, ()> {
        Ok(self)
    }
}

impl<'a> StrLike for &'a str {
    fn from_utf8(&self) -> Result<&str, ()> {
        Ok(*self)
    }
}

impl StrLike for str {
    fn from_utf8(&self) -> Result<&str, ()> {
        Ok(self)
    }
}

impl StrLike for Vec<u8> {
    fn from_utf8(&self) -> Result<&str, ()> {
        (**self).from_utf8()
    }
}

impl<'a> StrLike for &'a [u8] {
    fn from_utf8(&self) -> Result<&str, ()> {
        (**self).from_utf8()
    }
}

impl StrLike for [u8] {
    fn from_utf8(&self) -> Result<&str, ()> {
        str::from_utf8(self).map_err(|_| ())
    }
}

pub struct FromStr<P, O> {
    parser: P,
    _marker: PhantomData<O>,
}

impl<P, O> Parser for FromStr<P, O>
where
    P: Parser,
    P::Output: StrLike,
    O: str::FromStr,
    O::Err: Display,
{
    type Stream = P::Stream;
    type Output = O;

    fn parse_stream(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.parser.parse(stream) {
            (Ok(s), stream) => (
                s.from_utf8()
                    .map_err(|_| "invalid UTF-8".into())
                    .and_then(|s| s.parse().map_err(|e: O::Err| e.to_string().into()))
                    .map_err(|e| stream.err_from(e)),
                stream,
            ),
            (Err(err), stream) => (Err(err), stream),
        }
    }
}

pub fn from_str<P, O>(parser: P) -> FromStr<P, O>
where
    P: Parser,
    P::Output: StrLike,
    O: str::FromStr,
    O::Err: Display,
{
    FromStr {
        parser,
        _marker: PhantomData,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use error::Error;
    use parser::combinator::many1;
    use parser::token::{ascii, token};
    use stream::SourceCode;

    #[test]
    fn test_map() {
        let mut parser = map(ascii::digit(), |c: char| c.to_string());
        assert_eq!(parser.parse("3"), (Ok("3".to_string()), ""));
        assert_parse_err!(parser.parse("a3"), "a3");

        let mut parser = map(many1(ascii::letter()), |s: String| s.to_uppercase());
        assert_eq!(parser.parse("aBcD12e"), (Ok("ABCD".to_string()), "12e"));

        let mut parser = map(many1(ascii::alpha_num()), |s: String| {
            s.parse::<usize>().unwrap_or(0)
        });
        assert_eq!(parser.parse("324 dogs"), (Ok(324 as usize), " dogs"));
        assert_eq!(parser.parse("324dogs"), (Ok(0 as usize), ""));
    }

    #[test]
    fn test_bind() {
        let mut parser = bind(ascii::digit(), |c: char, rest: &str| {
            (Ok(c.to_string()), rest)
        });
        assert_eq!(parser.parse("3"), (Ok("3".to_string()), ""));
        assert_parse_err!(parser.parse("a3"), "a3");

        let mut parser = bind(many1(ascii::letter()), |s: String, rest| {
            (Ok(s.to_uppercase()), rest)
        });
        assert_eq!(parser.parse("aBcD12e"), (Ok("ABCD".to_string()), "12e"));

        let mut parser = bind(many1(ascii::alpha_num()), |s: String, rest| {
            match s.parse::<usize>() {
                Ok(n) => (Ok(n), rest),
                Err(e) => (Err(Error::Other(Box::new(e))), rest),
            }
        });
        assert_eq!(parser.parse("324 dogs"), (Ok(324usize), " dogs"));
        assert_parse_err!(parser.parse("324dogs"), "324dogs");
    }

    #[test]
    fn test_from_str() {
        let mut parser = many1::<_, String>(ascii::digit()).from_str::<u32>();
        test_parser!(&str | parser, {
            "369" => (Ok(369 as u32), ""),
            "369abc" => (Ok(369 as u32), "abc"),
            "abc" => (Err(Error::unexpected_token('a')), "abc"),
        });

        let mut parser = from_str::<_, f32>(many1::<_, String>(choice!(
            token('-'),
            token('.'),
            ascii::digit()
        )));
        test_parser!(&str | parser, {
            "12e" => (Ok(12 as f32), "e"),
            "-12e" => (Ok(-12 as f32), "e"),
            "-12.5e" => (Ok(-12.5 as f32), "e"),
            "12.5.9" => (Err("invalid float literal".into()), "12.5.9"),
        });

        let mut parser = many1::<_, String>(ascii::digit()).from_str::<f32>();
        test_parser!(SourceCode | parser, {
            "12e" => (Ok(12f32), "e", (0, 2)),
            "e12" => (Err(Error::unexpected_token('e')), "e12", (0, 0)),
        });
    }
}
