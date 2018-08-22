//! Parsers that parse a single token.

use std::fmt::Debug;
use std::marker::PhantomData;

use input::Input;
use parser::{Error, Info, ParseResult, Parser};

pub struct Any<I: Input>(PhantomData<I>);

impl<I: Input> Parser for Any<I> {
    type Input = I;
    type Output = I::Item;

    fn parse(&mut self, mut input: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        match input.pop() {
            Some(t) => input.ok(t),
            _ => input.err(Error::Expected(Info::Description("a token"))),
        }
    }
}

pub fn any<I: Input>() -> Any<I> {
    Any(PhantomData)
}

#[derive(Copy, Clone)]
pub struct Token<I: Input>(I::Item);

impl<I: Input> Parser for Token<I>
where
    I::Item: PartialEq,
{
    type Input = I;
    type Output = I::Item;

    fn parse(&mut self, mut input: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        match input.peek() {
            Some(item) if item == self.0 => {
                input.pop();
                input.ok(item)
            }
            _ => input.err(Error::Expected(Info::Token(self.0))),
        }
    }
}

pub fn token<I: Input>(item: I::Item) -> Token<I> {
    Token(item)
}

pub struct Cond<I: Input, F>(F, PhantomData<Fn(&I::Item) -> bool>);

impl<I: Input, F> Parser for Cond<I, F>
where
    // TODO: add err message (maybe add to Parser?)
    F: Fn(&I::Item) -> bool,
{
    type Input = I;
    type Output = I::Item;

    fn parse(&mut self, mut input: Self::Input) -> ParseResult<Self::Input, Self::Output> {
        match input.peek() {
            Some(ref t) if (self.0)(t) => {
                input.pop();
                input.ok(*t)
            }
            _ => input.err(Error::Expected(Info::Description("condition not met"))),
        }
    }
}

pub fn cond<I: Input, F>(f: F) -> Cond<I, F>
where
    F: Fn(&I::Item) -> bool,
{
    Cond(f, PhantomData)
}

macro_rules! def_char_parser {
    ($(#[$attr:meta])* $name:ident, $f:ident) => {
        $(#[$attr])*
        pub fn $name<I, T>() -> Cond<I, fn(&I::Item) -> bool>
        where
            I: Input<Item = T>,
            T: Copy + Debug + Into<char>,
        {
            Cond(|&c: &T| c.into().$f(), PhantomData)
        }
    };
}

pub mod ascii {
    //! Parsers for ASCII characters.

    use super::*;

    def_char_parser!(
        /// Parses an ASCII letter.
        letter,
        is_ascii_alphabetic
    );
    def_char_parser!(
        /// Parses a digit according to [`std::char::is_ascii_digit`].
        ///
        /// [`std::char::is_ascii_digit`]: https://doc.rust-lang.org/std/primitive.char.html#method.is_ascii_digit
        digit,
        is_ascii_digit
    );
    def_char_parser!(
        /// Parses an ASCII letter or digit.
        alpha_num,
        is_ascii_alphanumeric
    );
    def_char_parser!(
        /// Parses an ASCII whitespace character.
        whitespace,
        is_ascii_whitespace
    );
    def_char_parser!(
        /// Parses an ASCII punctuation character.
        punctuation,
        is_ascii_punctuation
    );

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_digit() {
            assert_eq!(digit().parse("1ab23"), (Ok('1'), "ab23"));
            // TODO: rewrite this to match (Error, input) [i.e. remove the `.1`]
            assert_eq!(digit().parse("a1b23").1, "a1b23");
        }
    }
}

pub mod unicode {
    //! Parsers for unicode characters.

    use super::*;

    def_char_parser!(
        /// Parses a Unicode alphabetic character.
        letter,
        is_alphabetic
    );
    def_char_parser!(
        /// Parses a Unicode numeric character.
        numeric,
        is_numeric
    );
    def_char_parser!(
        /// Parses a Unicode alphabetic or numeric character.
        alpha_num,
        is_alphanumeric
    );
    def_char_parser!(
        /// Parses a Unicode whitespace character.
        whitespace,
        is_whitespace
    );

    #[cfg(test)]
    mod test {
        use super::*;

        #[test]
        fn test_letter() {
            assert_eq!(letter().parse("京34a"), (Ok('京'), "34a"));
            assert_eq!(letter().parse("a京34"), (Ok('a'), "京34"));
            // TODO: rewrite this to match (Error, input) [i.e. remove the `.1`]
            assert_eq!(letter().parse("3京4a").1, "3京4a");
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_any() {
        assert_eq!(any().parse("hello, world."), (Ok('h'), "ello, world."));
    }

    #[test]
    fn test_token() {
        let mut parser = token('c');
        assert_eq!(parser.parse("cat"), (Ok('c'), "at"));
        // TODO: rewrite this to match (Error, input) [i.e. remove the `.1`]
        assert_eq!(parser.parse("ace").1, "ace");
    }

    #[test]
    fn test_cond() {
        assert_eq!(
            cond(|&c: &char| c.is_numeric()).parse("123abc"),
            (Ok('1'), "23abc")
        );
        assert_eq!(
            cond(|&c: &char| c.is_alphabetic()).parse("123abc").1,
            "123abc"
        );
    }
}
