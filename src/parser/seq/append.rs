use std::iter;
use std::marker::PhantomData;

use error::ParseResult;
use parser::Parser;
use stream::Stream;

pub struct Append<Xs, X, I> {
    items: Xs,
    item: X,
    __marker: PhantomData<I>,
}

impl<S: Stream, I, O, Xs, X> Parser for Append<Xs, X, I>
where
    Xs: Parser<Stream = S, Output = I>,
    X: Parser<Stream = S, Output = O>,
    I: Extend<O>,
{
    type Stream = S;
    type Output = I;

    fn parse_stream(&mut self, stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        match self.items.parse_stream(stream) {
            (Ok(mut items), stream) => match self.item.parse_stream(stream) {
                (Ok(item), stream) => {
                    items.extend(iter::once(item));
                    stream.ok(items)
                }
                (Err(err), stream) => stream.errs(err),
            },
            (Err(err), stream) => stream.errs(err),
        }
    }
}

pub fn append<S: Stream, I, O, Xs, X>(items: Xs, item: X) -> Append<Xs, X, I>
where
    Xs: Parser<Stream = S, Output = I>,
    X: Parser<Stream = S, Output = O>,
    I: Extend<O>,
{
    Append {
        items,
        item,
        __marker: PhantomData,
    }
}

// Create a parser which runs a series of subparsers in sequence, returning the collected output.
// This is basically just syntactic sugar for chaining parsers with `append`.
#[macro_export]
macro_rules! seq {
    ($init:expr => [ $head:expr, $($tail:expr),* $(,)* ]) => {{
        let head = $init.append($head);
        seq!(@inner head, $($tail),+)
    }};

    (@inner $head:expr $(,)*) => {{
        $head
    }};

    (@inner $head:expr, $($tail:expr),* $(,)*) => {{
        $head.append(seq!(@inner $($tail),*))
    }}
}

#[cfg(test)]
mod test {
    use super::*;
    use error::Error;
    use parser::seq::many;
    use parser::token::ascii::{hexdigit, letter};
    use parser::token::token;
    use parser::Parser;
    use stream::IndexedStream;

    #[test]
    fn test_append() {
        let mut parser = append::<_, String, _, _, _>(many(letter()), token('?'));
        test_parser!(IndexedStream<&str> | parser, {
            "huh?" => (Ok("huh?".to_string()), ("", 4));
            "oh? cool" => (Ok("oh?".to_string()), (" cool", 3));
            "???" => (Ok("?".to_string()), ("??", 1));
        }, {
            "" => (0, vec![Error::EOF, Error::expected_token('?')]);
            "" => (0, vec![Error::EOF, Error::expected_token('?')]);
            "whoops!" => (6, vec![Error::unexpected_token('!'), Error::expected_token('?')]);
            "!?" => (0, vec![Error::unexpected_token('!'), Error::expected_token('?')]);
        });
    }

    #[test]
    fn test_seq_macro() {
        let mut parser = seq!(token('%').s() => [hexdigit(), hexdigit()]);
        test_parser!(IndexedStream<&str> | parser, {
            "%AF" => (Ok(String::from("%AF")), ("", 3));
            "%d8_/^/_" => (Ok(String::from("%d8")), ("_/^/_", 3));
        });
        test_parser_errors!(IndexedStream<&str> | parser, {
            "" => (0, vec![Error::EOF, Error::expected_token('%')]);
            "%0" => (2, vec![Error::EOF]);
            "%zz" => (1, vec![Error::unexpected_token('z')]);
        });
    }
}
