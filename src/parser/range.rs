//! Parsers that parse a continuous series of tokens.

use error::{Error, ParseResult};
use parser::function::Map;
use parser::Parser;
use stream::{Position, Stream, StreamRange};

pub struct Range<S: Stream> {
    range: S::Range,
}

impl<S: Stream> Parser for Range<S> {
    type Stream = S;
    type Output = S::Range;

    fn parse_lazy(&mut self, mut stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let idx = self.range.len();
        let mut start_pos = stream.position().clone();
        let mut result = match stream.range(idx) {
            Some(mut range) => {
                if range == self.range {
                    stream.ok(range)
                } else {
                    let located = range
                        .tokens()
                        .zip(self.range.tokens())
                        .enumerate()
                        .find(|&(_, (left, right))| left != right);
                    match located {
                        Some((i, (left, _))) => {
                            let range = range.range(i).unwrap();
                            start_pos.update_range(&range);
                            stream.err(Error::unexpected_token(left))
                        }
                        None => stream.err(Error::unexpected_eoi()),
                    }
                }
            }
            None => stream.err(Error::unexpected_eoi()),
        };
        if let Err(ref mut errors) = result.data {
            errors.position = start_pos;
        }
        result
    }

    fn expected_error(&self) -> Option<Error<Self::Stream>> {
        Some(Error::expected_range(self.range.clone()))
    }
}

pub fn range<S: Stream>(range: &'static str) -> Range<S> {
    Range {
        range: S::Range::from_str(range),
    }
}

pub fn tokens<S: Stream>(tokens: &'static str) -> Map<Range<S>, fn(S::Range) -> Vec<S::Item>> {
    range(tokens).map(|range| range.tokens().collect())
}

#[cfg(test)]
mod test {
    use super::*;
    use stream::IndexedStream;
    use Error::*;

    #[test]
    fn test_range() {
        let mut parser = range("def");
        test_parser!(IndexedStream<&str> => &str | parser, {
            "def" => ok("def", ("", 3)),
            "defcon" => ok("def", ("con", 3)),
            "" => err(0, vec![Error::unexpected_eoi(), Error::expected_range("def")]),
            "de" => err(0, vec![Error::unexpected_eoi(), Error::expected_range("def")]),
            "dr" => err(0, vec![Error::unexpected_eoi(), Error::expected_range("def")]),
            "deg" => err(2, vec![Unexpected('g'.into()), Error::expected_range("def")]),
        });
    }
}

// TODO
// pub struct SatisfyRange
