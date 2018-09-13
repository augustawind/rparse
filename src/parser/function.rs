//! Parsers that transform the output of other Parsers with arbitrary functions.

use std::fmt::Display;
use std::marker::PhantomData;
use std::str;

use {Error, ParseResult, Parser, Stream};

pub struct Expect<P: Parser> {
    parser: P,
    error: Error<<<P as Parser>::Stream as Stream>::Range>,
}

impl<P: Parser> Parser for Expect<P> {
    type Stream = P::Stream;
    type Output = P::Output;

    fn parse_stream(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.parser.parse_stream(stream) {
            (Err(mut errors), stream) => {
                errors.add_error(self.error.clone());
                stream.errs(errors)
            }
            ok => ok,
        }
    }
}

pub fn expect<P, I>(parser: P, expected: I) -> Expect<P>
where
    P: Parser,
    I: Into<Error<<<P as Parser>::Stream as Stream>::Range>>,
{
    Expect {
        parser,
        error: expected.into(),
    }
}

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
        match self.parser.parse_stream(stream) {
            (Ok(s), stream) => (
                s.from_utf8()
                    .map_err(|_| "invalid UTF-8".into())
                    .and_then(|s| s.parse().map_err(|e: O::Err| e.to_string().into()))
                    .map_err(|e| {
                        let mut errors = stream.empty_err();
                        errors.add_error(e);
                        errors
                    }),
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
    use stream::{IndexedStream, SourceCode};

    #[test]
    fn test_map() {
        let mut parser = map(ascii::digit(), |c: char| c.to_string());
        test_parser!(&str | parser, {
            "3" => (Ok("3".to_string()), "");
        }, {
            "a3" => vec![Error::unexpected_token('a')];
        });

        let mut parser = map(many1(ascii::letter()), |s: String| s.to_uppercase());
        assert_eq!(parser.parse("aBcD12e"), (Ok("ABCD".to_string()), "12e"));

        let mut parser = map(many1(ascii::alpha_num()), |s: String| {
            s.parse::<usize>().unwrap_or(0)
        });
        test_parser!(&str | parser, {
            "324 dogs" => (Ok(324 as usize), " dogs");
            "324dogs" => (Ok(0 as usize), "");
        });
    }

    #[test]
    fn test_bind() {
        // TODO: use realistic use cases for these tests. many of these are better suited to map()
        let mut parser = ascii::digit().bind(|c: char, stream: &str| stream.ok(c.to_string()));
        test_parser!(&str | parser, {
            "3" =>  (Ok("3".to_string()), "");
        }, {
            "a3" => vec![Error::unexpected_token('a')];
        });

        let mut parser =
            many1(ascii::letter()).bind(|s: String, stream: &str| stream.ok(s.to_uppercase()));
        assert_eq!(parser.parse("aBcD12e"), (Ok("ABCD".to_string()), "12e"));

        let mut parser = many1(ascii::alpha_num()).bind(
            |s: String, stream: IndexedStream<&str>| match s.parse::<usize>() {
                Ok(n) => stream.ok(n),
                Err(e) => stream.err(Box::new(e).into()),
            },
        );
        test_parser!(IndexedStream<&str> | parser, {
            "324 dogs" => (Ok(324 as usize), " dogs", 3);
        });
        // TODO: add ability to control consumption, e.g. make this error show at beginning (0)
        // TODO: e.g.: many1(alpha_num()).bind(...).try()
        test_parser_errors!(IndexedStream<&str> | parser, {
            "324dogs" => at 7; (|err| err == &"invalid digit found in string".into());
        });
    }

    #[test]
    fn test_from_str() {
        let mut parser = many1::<_, String>(ascii::digit()).from_str::<u32>();
        test_parser!(&str | parser, {
            "369" => (Ok(369 as u32), "");
            "369abc" => (Ok(369 as u32), "abc");
        }, {
            "abc" => vec![Error::unexpected_token('a')];
        });

        let mut parser = from_str::<_, f32>(many1::<_, String>(choice!(
            token('-'),
            token('.'),
            ascii::digit()
        )));
        test_parser!(&str | parser, {
            "12e" => (Ok(12 as f32), "e");
            "-12e" => (Ok(-12 as f32), "e");
            "-12.5e" => (Ok(-12.5 as f32), "e");
        }, {
            "12.5.9" =>  vec!["invalid float literal".into()];
        });

        let mut parser = many1::<_, String>(ascii::digit()).from_str::<f32>();
        test_parser!(SourceCode | parser, {
            "12e" => (Ok(12f32), "e", (0, 2));
        }, {
            "e12" => vec![Error::unexpected_token('e')];
        });
    }
}
