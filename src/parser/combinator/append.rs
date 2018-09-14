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
    I: iter::Extend<O>,
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
    I: iter::Extend<O>,
{
    Append {
        items,
        item,
        __marker: PhantomData,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use parser::combinator::many;
    use parser::token::{ascii::letter, token};
    use parser::Parser;
    use stream::IndexedStream;

    #[test]
    fn test_append() {
        let mut parser = append::<_, String, char, _, _>(many(letter()), token('?'));
        test_parser!(IndexedStream<&str> | parser, {
            "huh?" => (Ok("huh?".to_string()), "", 4);
            "oh? cool" => (Ok("oh?".to_string()), " cool", 3);
            "???" => (Ok("?".to_string()), "??", 1);
        });
    }
}
