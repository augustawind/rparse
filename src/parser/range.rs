//! Parsers that parse a continuous series of tokens.

use error::{Error, Expected, ParseResult};
use parser::Parser;
use stream::{Position, RangeStream, Stream};

pub struct Range<S: Stream> {
    range: S::Range,
}

impl<S: Stream> Parser for Range<S> {
    type Stream = S;
    type Output = S::Range;

    fn parse_lazy(&mut self, mut stream: Self::Stream) -> ParseResult<Self::Stream, Self::Output> {
        let idx = self.range.len();
        let mut start_pos = stream.position().clone();

        let mut range = match stream.range(idx) {
            Some(range) => {
                if range == self.range {
                    return stream.ok(range);
                } else {
                    range
                }
            }
            None => stream.as_range(),
        };

        let first_bad_idx = range
            .tokens()
            .zip(self.range.tokens())
            .enumerate()
            .find(|&(_, (left, right))| left != right);
        let (range, error) = match first_bad_idx {
            Some((i, (left, _))) => (range.range(i).unwrap(), Error::item(left)),
            None => (range.as_range(), Error::eoi()),
        };
        start_pos.update_range(&range);
        stream.err_at(start_pos, error)
    }

    fn expected_error(&self) -> Option<Expected<Self::Stream>> {
        Some(Expected::range(self.range.clone()))
    }
}

pub fn range<S: Stream>(range: &'static str) -> Range<S> {
    Range {
        range: S::Range::from_str(range),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use stream::IndexedStream;

    #[test]
    fn test_range() {
        let mut parser = range("def");
        test_parser!(IndexedStream<&str> => &str | parser, {
            "def" => ok("def", ("", 3)),
            "defcon" => ok("def", ("con", 3)),
            "" => err(Error::eoi().at(0).expected_range("def")),
            "de" => err(Error::eoi().at(2).expected_range("def")),
            "dr" => err(Error::item('r').at(1).expected_range("def")),
            "deg" => err(Error::item('g').at(2).expected_range("def")),
        });
    }
}

// TODO
// pub struct SatisfyRange
