use std::collections::HashMap;

use rparse::parser::{
    item::item,
    repeat::{many1, sep_by},
};
use rparse::{Parser, Stream};

use common::{atom, crlf, text};

pub type Headers = HashMap<String, String>;

pub fn headers<S: Stream>() -> impl Parser<Stream = S, Output = Headers> {
    sep_by::<Headers, _, _>(header_field(), crlf())
}

fn header_field<S: Stream>() -> impl Parser<Stream = S, Output = (String, String)> {
    (field_name(), item(b':'), field_value()).map(|(key, _, value)| (key, value))
}

fn field_name<S: Stream>() -> impl Parser<Stream = S, Output = String> {
    many1(atom().as_char()).map(to_title_case)
}

fn to_title_case(s: String) -> String {
    s.split('-')
        .map(|part: &str| {
            if part.is_empty() {
                return String::new();
            }
            let (first, rest) = part.split_at(1);
            first.to_ascii_uppercase() + &rest.to_ascii_lowercase()
        })
        .collect::<Vec<String>>()
        .join("-")
}

fn field_value<S: Stream>() -> impl Parser<Stream = S, Output = String> {
    text().map(|s: String| s.trim().to_string())
}

#[cfg(test)]
mod test {
    use super::*;
    use rparse::error::{Error, Info};
    use rparse::stream::IndexedStream;

    macro_rules! map {
        ( $($key:expr => $value:expr),+ ) => {
            {
                let mut m = ::std::collections::HashMap::new();
                $(
                    m.insert($key.into(), $value.into());
                )+
                m
            }
        };
    }

    #[test]
    fn test_headers() {
        let mut parser = headers();
        test_parser!(IndexedStream<&str> => Headers | parser, {
            "\r\n" => ok(HashMap::new(), ("", 2)),
            "accept: foo/bar\r\n" => ok(map!["Accept" => "foo/bar"], ("", 17)),
            "content-tYPE:   11 \r\nx:y\r\n\r\n" => ok(
                map!["Content-Type" => "11", "X" => "y"],
                ("\r\n", 26)
            ),
            "spacing: 1\r2\n3\r\r4\r\n" => ok(map!["Spacing" => "1\r2\n3\r\r4"], ("", 19)),
            "" => err(Error::eoi().expected_range("\r\n").at(0)),
            "foo: bar" => err(Error::eoi().expected_range("\r\n").at(8)),
            "foo: bar\r" => err(Error::eoi().expected_range("\r\n").at(9)),
            "foo: bar\r \n" => err(Error::eoi().expected_range("\r\n").at(11)),
        });
    }

    #[test]
    fn test_field_name() {
        let mut parser = field_name();
        test_parser!(&str => String | parser, {
            "foo" => ok("Foo".to_string(), ""),
            "foo-BAR--baz" => ok("Foo-Bar--Baz".to_string(), ""),
            "ok-15/" => ok("Ok-15".to_string(), "/"),
            "ok-15 " => ok("Ok-15".to_string(), " "),
            "ok-15\r\n" => ok("Ok-15".to_string(), "\r\n"),
            "" => err(Error::eoi().expected("an atom")),
            "/foo-bar" => err(Error::item('/').expected("an atom")),
            "\r\n" => err(Error::item('\r').expected("an atom")),
        });
    }

    #[test]
    fn test_field_value() {
        let mut parser = field_value();
        let expected = vec![Info::Msg("a token"), Info::Msg("a CRLF sequence")];
        test_parser!(IndexedStream<&str> => String | parser, {
            "foo\r\n" => ok("foo".to_string(), ("\r\n", 3)),
            "  foo\t \t\r\n" => ok("foo".to_string(), ("\r\n", 8)),
            "\tfoo-/\"bar\"\r\n" => ok("foo-/\"bar\"".to_string(), ("\r\n", 11)),
            // "" => err(Error::eoi().at(0).expected_one_of(expected.clone())),
            "foo" => err(Error::eoi().at(3).expected_one_of(expected.clone())),
            // "foo\r" => err(Error::eoi().at(4).expected_one_of(expected.clone())),
        });
    }
}
