//! Parsers that transform the output of other Parsers with arbitrary functions.

use std::fmt::Display;
use std::iter::FromIterator;
use std::marker::PhantomData;
use std::option::Option::*;
use std::str;

use error::Error;
use traits::StrLike;
use {ParseResult, Parser, Stream};

pub struct Expect<P: Parser> {
    parser: P,
    error: Error<P::Stream>,
}

impl<P: Parser> Parser for Expect<P> {
    type Stream = P::Stream;
    type Output = P::Output;

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        self.parser.parse_lazy(stream)
    }

    fn expected_errors(&self) -> Vec<Error<Self::Stream>> {
        vec![self.error.clone()]
    }
}

/// Equivalent to [`parser.expect(error)`].
///
/// [`parser.expect(error)`]: Parser::expect
pub fn expect<P, I>(parser: P, error: I) -> Expect<P>
where
    P: Parser,
    I: Into<Error<P::Stream>>,
{
    Expect {
        parser,
        error: Error::expected(error),
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

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let (result, stream) = self.parser.parse_lazy(stream)?;
        stream.result(result.map(|output| (self.f)(output)))
    }

    fn expected_errors(&self) -> Vec<Error<Self::Stream>> {
        self.parser.expected_errors()
    }
}

/// Equivalent to [`parser.map(f)`].
///
/// [`parser.map(f)`]: Parser::map
pub fn map<P, F, O>(parser: P, f: F) -> Map<P, F>
where
    P: Parser,
    F: Fn(P::Output) -> O,
{
    Map { parser, f }
}

pub type Iter<P, I> = Map<P, fn(<P as Parser>::Output) -> <I as IntoIterator>::IntoIter>;

/// Equivalent to [`p.iter()`].
///
/// [`p.iter()`]: Parser::iter
pub fn iter<P, I>(p: P) -> Iter<P, I>
where
    P: Parser<Output = I>,
    I: IntoIterator,
{
    p.map(|output| output.into_iter())
}

pub type Collect<P, O> = Map<P, fn(<P as Parser>::Output) -> O>;

/// Equivalent to [`p.collect()`].
///
/// [`p.collect()`]: Parser::collect
pub fn collect<P, O>(p: P) -> Collect<P, O>
where
    P: Parser,
    P::Output: IntoIterator,
    O: FromIterator<<P::Output as IntoIterator>::Item>,
{
    p.map(|output| output.into_iter().collect())
}

pub type Flatten<P, O> = Map<P, fn(<P as Parser>::Output) -> Vec<O>>;

/// Equivalent to [`p.flatten()`].
///
/// [`p.flatten()`]: Parser::flatten
pub fn flatten<P, O>(p: P) -> Flatten<P, O>
where
    P: Parser<Output = Vec<Vec<O>>>,
{
    p.map(|output| output.into_iter().flatten().collect())
}

pub type Wrap<O, P> = Map<P, fn(<P as Parser>::Output) -> O>;

/// Equivalent to [`p.wrap()`].
///
/// [`p.wrap()`]: Parser::wrap
pub fn wrap<O, P>(p: P) -> Wrap<O, P>
where
    P: Parser,
    O: Extend<P::Output> + Default,
{
    p.map(|output| {
        let mut wrapped = O::default();
        wrapped.extend(std::iter::once(output));
        wrapped
    })
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

    fn parse_lazy(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let (result, stream) = self.parser.parse_partial(stream)?;
        match result {
            Some(value) => (self.f)(value, stream),
            None => stream.noop(),
        }
    }

    fn expected_errors(&self) -> Vec<Error<Self::Stream>> {
        self.parser.expected_errors()
    }
}

/// Equivalent to [`p.bind()`].
///
/// [`p.bind()`]: Parser::bind
pub fn bind<P, F, O>(p: P, f: F) -> Bind<P, F>
where
    P: Parser,
    F: Fn(P::Output, P::Stream) -> O,
{
    Bind { parser: p, f }
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

    fn parse_partial(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.parser.parse_partial(stream)? {
            (Some(s), stream) => {
                let result = s
                    .from_utf8()
                    .map_err(|_| "invalid UTF-8".into())
                    .and_then(|s: &str| s.parse::<O>().map_err(|e: O::Err| e.to_string().into()));
                match result {
                    Ok(output) => stream.ok(output),
                    Err(err) => stream.err(err),
                }
            }
            (None, stream) => stream.noop(),
        }
    }
}

/// Equivalent to [`p.from_str()`].
///
/// [`p.from_str()`]: Parser::from_str
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
    use error::Error::*;
    use parser::item::{ascii, item};
    use parser::repeat::many1;
    use parser::test_utils::*;
    use stream::{IndexedStream, SourceCode};

    #[test]
    fn test_map() {
        let mut parser = map(ascii::digit(), |c: char| c.to_string());
        test_parser!(&str => String | parser, {
            "3" => ok("3".to_string(), ""),
            "a3" => err(vec![Unexpected('a'.into()), Error::expected("an ascii digit")]),
        });

        let mut parser = map(many1::<String, _>(ascii::letter()), |s| s.to_uppercase());
        assert_eq!(
            parser.parse("aBcD12e"),
            ok_result("ABCD".to_string(), "12e")
        );

        let mut parser = map(many1::<String, _>(ascii::alpha_num()), |s| {
            s.parse::<usize>().unwrap_or(0)
        });
        test_parser!(&str => usize | parser, {
            "324 dogs" => ok(324 as usize, " dogs"),
            "324dogs" => ok(0 as usize, ""),
        });
    }

    #[test]
    fn test_bind() {
        // TODO: use realistic use cases for these tests. many of these are better suited to map()
        let mut parser = ascii::digit().bind(|c: char, stream: &str| stream.ok(c.to_string()));
        test_parser!(&str => String | parser, {
            "3" => ok("3".to_string(), ""),
            "a3" => err(vec![Error::unexpected_item('a'), Error::expected("an ascii digit")]),
        });

        let mut parser = many1::<String, _>(ascii::letter())
            .bind(|s: String, stream: &str| stream.ok(s.to_uppercase()));
        assert_eq!(
            parser.parse("aBcD12e"),
            ok_result("ABCD".to_string(), "12e")
        );

        let mut parser = many1::<String, _>(ascii::alpha_num()).bind(
            |s: String, stream: IndexedStream<&str>| match s.parse::<usize>() {
                Ok(n) => stream.ok(n),
                Err(e) => stream.err(Box::new(e).into()),
            },
        );
        test_parser!(IndexedStream<&str> => usize | parser, {
            "324 dogs" => ok(324 as usize, (" dogs", 3)),
        // TODO: add ability to control consumption, e.g. make this error show at beginning (0)
        // TODO: e.g.: many1(alpha_num()).bind(...).try()
            "324dogs" => err(7, vec!["invalid digit found in string".into(), Error::expected("an ascii letter or digit")]),
        });
    }

    #[test]
    fn test_collect() {
        let mut parser = collect(many1::<Vec<_>, _>(ascii::digit()));
        test_parser!(IndexedStream<&str> => String | parser, {
            "123" => ok("123".to_string(), ("", 3)),
            "123abc" => ok("123".to_string(), ("abc", 3)),
            "" => err(0, vec![Error::eoi(), Error::expected("an ascii digit")]),
            "abc" => err(0, vec![Error::unexpected_item('a'), Error::expected("an ascii digit")]),
        });
    }

    #[test]
    fn test_flatten() {
        let mut parser = flatten(many1(ascii::digit()).then(many1(ascii::letter())));
        test_parser!(IndexedStream<&str> => Vec<char> | parser, {
            "1a" => ok(vec!['1', 'a'], ("", 2)),
            "0bb3" => ok(vec!['0', 'b', 'b'], ("3", 3)),
            "" => err(0, vec![Error::eoi(), Error::expected("an ascii digit")]),
            "3\t" => err(1, vec![Error::unexpected_item('\t'), Error::expected("an ascii letter")]),
        });
    }

    #[test]
    fn test_wrap() {
        let mut parser = item(b'x').wrap();
        test_parser!(IndexedStream<&str> => Vec<char> | parser, {
            "x" => ok(vec!['x'], ("", 1)),
            "" => err(0, vec![Error::eoi(), Error::expected(b'x')]),
            "\t" => err(0, vec![Error::unexpected_item('\t'), Error::expected(b'x')]),
        });
    }

    #[test]
    fn test_from_str() {
        let mut parser = many1::<String, _>(ascii::digit()).from_str::<u32>();
        test_parser!(&str => u32 | parser, {
            "369" => ok(369 as u32, ""),
            "369abc" => ok(369 as u32, "abc"),
            "abc" => err(vec![Unexpected('a'.into()), Error::expected("an ascii digit")]),
        });

        let mut parser =
            many1::<String, _>(choice!(item(b'-'), item(b'.'), ascii::digit())).from_str::<f32>();
        test_parser!(&str => f32 | parser, {
            "12e" => ok(12 as f32, "e"),
            "-12e" => ok(-12 as f32, "e"),
            "-12.5e" => ok(-12.5 as f32, "e"),
            "12.5.9" =>  err(vec!["invalid float literal".into()]),
        });

        let mut parser = many1::<String, _>(ascii::digit()).from_str::<f32>();
        test_parser!(SourceCode => f32 | parser, {
            "12e" => ok(12f32, ("e", (1, 3))),
            "e12" => err((1, 1), vec![Unexpected('e'.into()), Error::expected("an ascii digit")]),
        });
    }
}
