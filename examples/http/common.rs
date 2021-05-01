use regex::Regex;

use rparse::error::Expected;
use rparse::parser::{
    item::{any, eoi, item, none_of, satisfy},
    parser,
    range::range,
    repeat::{many, take_until},
    seq::between,
};
use rparse::stream::StreamItem;
use rparse::{Error, Parser, Stream};

pub static LWS: &'static [u8] = &[b' ', b'\t'];
pub static SEPARATORS: &'static [u8] = &[
    b'(', b')', b'<', b'>', b'@', b',', b';', b':', b'\\', b'"', b'/', b'[', b']', b'?', b'=',
    b'{', b'}', b' ', b'\t',
];

/// Parses an atom.
///
/// Atoms are commonly used to define values between separators. In most cases, non-atoms can
/// only be used if escaped (within quotations (" ") or by a backslash (\)).
pub fn atom<S: Stream>() -> impl Parser<Stream = S, Output = S::Item> {
    satisfy(|t: &S::Item| {
        let separators = SEPARATORS
            .iter()
            .map(|&b| b.into())
            .collect::<Vec<S::Item>>();
        !(t.is_ascii_control() || separators.contains(t))
    })
    .expect("an atom")
}

/// Parses any text surrounded with double quotes (").
///
/// Within the block, double quotes (") must be escaped with a backslash (\). Parsing ends
/// at the first unescaped double quote after the opening quote.
pub fn quoted_string<S: Stream>() -> impl Parser<Stream = S, Output = String> {
    between(
        item(b'"'),
        item(b'"'),
        many(parser(|stream: S| {
            any()
                .and_then(|b: S::Item, stream: S| match b.into() {
                    '\\' => backslash_escaped().parse_lazy(stream),
                    '"' => stream.err(Error::eoi()),
                    ch => stream.ok(ch),
                })
                .parse(stream)
        })),
    )
}

fn backslash_escaped<S: Stream>() -> impl Parser<Stream = S, Output = char> {
    any().and_then(|b: S::Item, stream: S| {
        let result = Some(match b.into() {
            '\\' => '\\',
            '\'' => '\'',
            '"' => '"',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '0' => '\0',
            _ => return stream.noop(),
        });
        stream.result(result)
    })
}

/// Parses everything until a CRLF (\r\n) is encountered.
pub fn text<S: Stream>() -> impl Parser<Stream = S, Output = String> {
    lazy_static! {
        static ref SPLIT_RE: Regex = Regex::new(r"[ \t]*\r\n[ \t]+").unwrap();
    }
    take_until(
        any().as_char(),
        crlf().skip(none_of(LWS)).or(crlf().skip(eoi::<(), _>())),
    )
    .expect(Expected::OneOf(vec![
        "a token".into(),
        "a CRLF sequence".into(),
    ]))
    .map(|s: String| {
        SPLIT_RE
            .split(&s)
            .filter(|s| !s.is_empty())
            .collect::<Vec<&str>>()
            .join(" ")
    })
}

/// Parses a carriage return/line feed sequence (\r\n).
pub fn crlf<S: Stream>() -> impl Parser<Stream = S, Output = ()> {
    range("\r\n").map(|_| ())
}

#[cfg(test)]
mod test {
    use super::*;
    use rparse::stream::IndexedStream;
    use rparse::Error;

    #[test]
    fn test_atom() {
        let mut parser = atom();
        test_parser!(IndexedStream<&str> => char | parser, {
            "a" => ok('a', ("", 1)),
            "11" => ok('1', ("1", 1)),
            "_ab" => ok('_', ("ab", 1)),
            "" => err(Error::eoi().expected("an atom").at(0)),
        });

        for c in 0u8..=32 {
            let input = format!("{}_foo", c as char);
            let stream = IndexedStream::<&str>::from(input.as_ref());
            assert_eq!(
                atom().parse(stream.clone()),
                Err((Error::item(c as char).expected("an atom").at(0), stream)),
                "unexpectedly parsed '{}': should fail parsing control characters",
                c as char,
            );
        }
        for &item in SEPARATORS.iter() {
            let input = [item, b'\n'];
            let stream = IndexedStream::from(&input[..]);
            assert_eq!(
                atom().parse(stream.clone()),
                Err((Error::item(item).expected("an atom").at(0), stream)),
                "unexpectedly parsed '{}': should fail parsing SEPARATORS",
                item as char,
            );
        }
    }

    #[test]
    fn test_quoted_string() {
        let mut parser = quoted_string();
        test_parser!(IndexedStream<&str> => String | parser, {
            r#""""# => ok("".to_string(), ("", 2)),
            r#"" hey ""yo""# => ok(" hey ".to_string(), (r#""yo""#, 7)),
            r#""({foo?\r\n})""# => ok("({foo?\r\n})".to_string(), ("", 14)),
            r#""my \"name\" is \"boo\"""# => ok("my \"name\" is \"boo\"".to_string(), ("", 24)),
            r#"""# => err(Error::eoi().expected(b'"').at(1)),
            r#""baz"# => err(Error::eoi().expected(b'"').at(4)),
            r#""baz\""# => err(Error::eoi().expected(b'"').at(6)),
            r#"baz"# => err(Error::item('b').expected(b'"').at(0)),
            r#"\"baz"# => err(Error::item('\\').expected(b'"').at(0)),
        });
    }

    #[test]
    fn test_text() {
        let mut parser = text();
        test_parser!(&str => String | parser, {
            "foo\r\n bar\r\n\tbaz\r\nbiff" => ok("foo bar baz".to_string(), "\r\nbiff"),
        });
    }

    #[test]
    fn test_crlf() {
        let mut parser = crlf();
        test_parser!(IndexedStream<&str> => () | parser, {
            "\r\n" => ok((), ("", 2)),
            "\r\n\tfoo" => ok((), ("\tfoo", 2)),
            "" => err(Error::eoi().expected_range("\r\n").at(0)),
            "\r" => err(Error::eoi().expected_range("\r\n").at(1)),
            "\r\t" => err(Error::item('\t').expected_range("\r\n").at(1)),
            "\n" => err(Error::item('\n').expected_range("\r\n").at(0)),
        });
    }
}
