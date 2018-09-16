//! Parsers that parse a continuous series of tokens.

use error::{Error, ParseResult};
use parser::Parser;
use stream::{RangeStream, Stream};

pub struct Range<S: Stream> {
    range: S::Range,
}

impl<S: Stream> Parser for Range<S> {
    type Stream = S;
    type Output = S::Range;

    fn parse_stream(
        &mut self,
        mut stream: Self::Stream,
    ) -> ParseResult<Self::Stream, Self::Output> {
        let idx = self.range.len();
        let result = match stream.range(idx) {
            Some(range) => {
                if range == self.range {
                    stream.ok(range)
                } else {
                    let errs = stream.empty_err();
                    stream.errs(errs)
                }
            }
            None => stream.err(Error::EOF),
        };
        match result {
            (Err(mut errors), stream) => {
                errors.add_error(Error::expected_range(self.range.clone()));
                stream.errs(errors)
            }
            ok => ok,
        }
    }
}

pub fn range<S: Stream>(range: S::Range) -> Range<S> {
    Range { range }
}

#[cfg(test)]
mod test {
    use super::*;
    use stream::IndexedStream;

    #[test]
    fn test_range() {
        let mut parser = range("def");
        test_parser!(IndexedStream<&str> | parser, {
            "def" => (Ok("def"), ("", 3));
            "defcon" => (Ok("def"), ("con", 3));
        });
        test_parser_errors!(IndexedStream<&str> | parser, {
            "" => at 0; vec![Error::EOF, Error::expected_range("def")];
            "de" => at 0; vec![Error::EOF, Error::expected_range("def")];
            "dr" => at 0; vec![Error::EOF, Error::expected_range("def")];
            "deg" => at 3; vec![Error::expected_range("def")];
        });
    }
}

// TODO
// pub struct SatisfyRange
