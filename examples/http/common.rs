use rparse::parser::{
    item::{any, item, one_of, satisfy},
    range::range,
    repeat::{many, many1},
    seq::between,
};
use rparse::stream::StreamItem;
use rparse::{Error, Parser, Stream};

pub static SEPARATORS: &'static [u8] = &[
    b'(', b')', b'<', b'>', b'@', b',', b';', b':', b'\\', b'"', b'/', b'[', b']', b'?', b'=',
    b'{', b'}', b' ', b'\t',
];

/// Parses a token.
///
/// Tokens are commonly used to define values between separators. In most cases, non-tokens can
/// only be used if escaped (within quotations (" ") or by a backslash (\)).
pub fn token<S: Stream>() -> impl Parser<Stream = S, Output = S::Item> {
    satisfy(|t: &S::Item| {
        let separators = SEPARATORS
            .iter()
            .map(|&b| b.into())
            .collect::<Vec<S::Item>>();
        !(t.is_ascii_control() || separators.contains(t))
    })
    .expect("a token")
}

/// Parses any text surrounded with double quotes (").
///
/// Within the block, double quotes (") must be escaped with a backslash (\). Parsing ends
/// at the first unescaped double quote after the opening quote.
pub fn quoted_text<S: Stream>() -> impl Parser<Stream = S, Output = String> {
    between(
        item(b'"'),
        item(b'"'),
        many(any().bind(|b: S::Item, stream: S| match b.into() {
            '\\' => any().map(|b: S::Item| b.into()).parse_lazy(stream),
            '"' => stream.err(Error::eoi()),
            ch => stream.ok(ch),
        })),
    )
}

/// Parses one or more linear whitespace characters.
///
/// Linear whitespace is any whitespace character that doesn't start a new line.
pub fn lws<S: Stream>() -> impl Parser<Stream = S, Output = ()> {
    many1(one_of(&[b' ', b'\t']).map(|_| ()))
}

/// Parses a carriage return/line feed sequence (\r\n).
fn crlf<S: Stream>() -> impl Parser<Stream = S, Output = S::Range> {
    range("\r\n")
}

#[cfg(test)]
mod test {
    use super::*;
    use rparse::stream::IndexedStream;
    use rparse::Error;

    #[test]
    fn test_token() {
        let mut parser = token();
        test_parser!(IndexedStream<&str> => char | parser, {
            "a" => ok('a', ("", 1)),
            "11" => ok('1', ("1", 1)),
            "_ab" => ok('_', ("ab", 1)),
            "" => err(Error::eoi().expected("a token").at(0)),
        });

        for c in 0u8..=32 {
            let input = format!("{}_foo", c as char);
            let stream = IndexedStream::<&str>::from(input.as_ref());
            assert_eq!(
                token().parse(stream.clone()),
                Err((Error::item(c as char).expected("a token").at(0), stream)),
                "unexpectedly parsed '{}': should fail parsing control characters",
                c as char,
            );
        }
        for &item in SEPARATORS.iter() {
            let input = [item, b'\n'];
            let stream = IndexedStream::from(&input[..]);
            assert_eq!(
                token().parse(stream.clone()),
                Err((Error::item(item).expected("a token").at(0), stream)),
                "unexpectedly parsed '{}': should fail parsing SEPARATORS",
                item as char,
            );
        }
    }

    #[test]
    fn test_lws() {
        let mut parser = lws();
        test_parser!(IndexedStream<&str> => () | parser, {
            " " => ok((), ("", 1)),
            "\t" => ok((), ("", 1)),
            "  \t\t " => ok((), ("", 5)),
            "\t \tfoo" => ok((), ("foo", 3)),
            " foo" => ok((), ("foo", 1)),
            " \t foo" => ok((), ("foo", 3)),
            "" => err(Error::eoi().expected_one_of(vec![b' ', b'\t']).at(0)),
            "\r\n" => err(Error::item('\r').expected_one_of(vec![b' ', b'\t']).at(0)),
        });
    }

    #[test]
    fn test_crlf() {
        let mut parser = crlf();
        test_parser!(IndexedStream<&str> => &str | parser, {
            "\r\n" => ok("\r\n", ("", 2)),
            "\r\n\tfoo" => ok("\r\n", ("\tfoo", 2)),
            "" => err(Error::eoi().expected_range("\r\n").at(0)),
            "\r" => err(Error::eoi().expected_range("\r\n").at(0)),
            "\r\t" => err(Error::item('\t').expected_range("\r\n").at(1)),
            "\n" => err(Error::item('\n').expected_range("\r\n").at(0)),
        });
    }
}
