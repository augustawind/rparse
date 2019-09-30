use std::collections::HashMap;

use rparse::parser::{
    item::item,
    repeat::{many1, sep_by},
};
use rparse::{Parser, Stream};

use common::{atom, crlf, text};

type Headers = HashMap<String, String>;

pub fn headers<S: Stream>() -> impl Parser<Stream = S, Output = Headers> {
    sep_by::<Headers, _, _>(header_field(), crlf()).skip(crlf())
}

pub fn header_field<S: Stream>() -> impl Parser<Stream = S, Output = (String, String)> {
    (field_name(), item(b':'), field_value()).map(|(key, _, value)| (key, value))
}

pub fn field_name<S: Stream>() -> impl Parser<Stream = S, Output = String> {
    many1(atom().as_char()).map(to_title_case)
}

fn to_title_case(s: String) -> String {
    s.split('-')
        .map(|part| {
            let (first, rest) = part.split_at(1);
            format!("{}{}", first.to_uppercase(), rest.to_lowercase())
        })
        .collect::<Vec<String>>()
        .join("-")
}

pub fn field_value<S: Stream>() -> impl Parser<Stream = S, Output = String> {
    text().map(|s: String| s.trim().to_string())
}

#[cfg(test)]
mod test {
    use super::*;
    use rparse::stream::IndexedStream;
    use rparse::Error;

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
}
