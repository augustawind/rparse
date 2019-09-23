use std::str::FromStr;

use rparse::parser::parser;
use rparse::parser::range::range;
use rparse::parser::seq::{many, many1};
use rparse::parser::token::{ascii, token};
use rparse::{Parser, Stream};

enum HTTPVersion {
    V1,
    V11,
    V2,
}

impl FromStr for HTTPVersion {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "1" => Ok(Self::V1),
            "1.1" => Ok(Self::V11),
            "2" => Ok(Self::V2),
            _ => Err(format!("invalid http version '{}'", s)),
        }
    }
}

pub fn request_line<S>() -> impl Parser<Stream = S, Output = (String, String, String)>
where
    S: Stream,
{
    parser(|s: S| {
        let (method, s) = http_method().must_parse(s)?;
        let (uri, s) = token(b' ').and(uri()).must_parse(s)?;
        let (version, s) = token(b' ').and(http_version()).must_parse(s)?;
        let (_, s) = crlf().must_parse(s)?;
        s.ok((method, uri, version))
    })
}

fn http_version<S>() -> impl Parser<Stream = S, Output = String>
where
    S: Stream,
{
    // an HTTP version is the text "HTTP/"
    range("HTTP/")
        // followed by a version number
        .and(choice![range("1.1"), range("1"), range("2")])
        .as_string()
}

fn http_method<S>() -> impl Parser<Stream = S, Output = String>
where
    S: Stream,
{
    choice![
        range("GET"),
        range("PUT"),
        range("POST"),
        range("HEAD"),
        range("PATCH"),
        range("TRACE"),
        range("DELETE"),
        range("OPTIONS"),
        range("CONNECT"),
    ]
    .as_string()
}

fn uri<S>() -> impl Parser<Stream = S, Output = String>
where
    S: Stream,
{
    // a URI is
    seq![
        // a scheme
        uri_scheme(),
        // optionally followed by a path
        uri_path().optional(),
    ]
    .collect()
}

fn uri_scheme<S>() -> impl Parser<Stream = S, Output = String>
where
    S: Stream,
{
    // a URI scheme is
    (
        // a scheme identifier
        many1(ascii::letter()).map(|s| s.into_iter().map(Into::into).collect()),
        // followed by a delimiter
        range("://").as_string(),
    )
        .map(|(r0, r1): (String, String)| format!("{}{}", r0, r1))
}

fn uri_path<S>() -> impl Parser<Stream = S, Output = String>
where
    S: Stream,
{
    // a URI path is either
    choice![
        concat![
            // a slash
            token(b'/').wrap(),
            // followed by zero or more URI segments separated by slashes
            concat![
                uri_segment(),
                many(token(b'/').wrap().or(uri_segment())).flatten(),
            ]
            .optional()
        ],
        // or a URI segment followed by zero or more URI segments separated by slashes
        concat![
            uri_segment(),
            many(token(b'/').wrap().or(uri_segment())).flatten(),
        ],
    ]
    .map(|s| s.into_iter().map(Into::into).collect())
}

fn uri_segment<S>() -> impl Parser<Stream = S, Output = Vec<S::Item>>
where
    S: Stream,
{
    // a URI segment is one or more
    many1(choice![
        // percent-encoded octets
        percent_encoded(),
        // and URI-safe character sequences
        many1(uri_token()),
    ])
    .flatten()
}

fn uri_token<S>() -> impl Parser<Stream = S, Output = S::Item>
where
    S: Stream,
{
    choice![
        ascii::alpha_num(),
        token(b'-'),
        token(b'.'),
        token(b'_'),
        token(b'~')
    ]
}

fn percent_encoded<S>() -> impl Parser<Stream = S, Output = Vec<S::Item>>
where
    S: Stream,
{
    token(b'%')
        .then(ascii::hexdigit())
        .append(ascii::hexdigit())
}

fn crlf<S>() -> impl Parser<Stream = S, Output = String>
where
    S: Stream,
{
    range("\r\n").as_string()
}

#[cfg(test)]
mod test {
    use super::*;
    use rparse::error::{Error, Info};
    use rparse::stream::IndexedStream;

    // TODO: [u8]
    #[test]
    fn test_http_method() {
        let expected_error = Error::expected(Error::one_of(
            [
                "GET", "PUT", "POST", "HEAD", "PATCH", "TRACE", "DELETE", "OPTIONS", "CONNECT",
            ]
            .into_iter()
            .map(|s| Info::Range(s.as_bytes()))
            .collect(),
        ));

        test_parser!(IndexedStream<&[u8]> => String | http_method(), {
            &b"GET"[..] => ok("GET".into(), (&b""[..], 3)),
            &b"HEAD\n/"[..] => ok("HEAD".into(), (&b"\n/"[..], 4)),
        });

        test_parser!(IndexedStream<&[u8]> => String | http_method(), {
            &b"PUPPYDOG"[..] => err(2, vec![
                Error::unexpected_token(b'P'),
                expected_error,
            ]),
        });

        assert_eq!(
            http_method().parse("TRACE it"),
            Ok((Some("TRACE".into()), " it"))
        );
    }

    #[test]
    fn test_percent_encoded() {
        test_parser!(&str => String | percent_encoded().collect::<String>(), {
            "%A9" => ok("%A9".into(), ""),
            "%0f/hello" => ok("%0f".into(), "/hello"),
            "" => err(vec![Error::unexpected_eoi(), Error::expected_token('%')]),
            "%%0f" => err(vec![
                Error::unexpected_token('%'),
                Error::expected("a hexadecimal digit"),
            ]),
            "%xy" => err(vec![
                Error::unexpected_token('x'),
                Error::expected("a hexadecimal digit"),
            ]),
        });
    }

    #[test]
    fn test_uri_path() {
        test_parser!(IndexedStream<&str> => String | uri_path(), {
            "/" => ok("/".into(), ("", 1)),
            "foo" => ok("foo".into(), ("", 3)),
            "/my_img.jpeg" => ok("/my_img.jpeg".into(), ("", 12)),
            "foo/x%20y/z.gif/" => ok("foo/x%20y/z.gif/".into(), ("", 16)),
            "/%%bc" => ok("/".into(), ("%%bc", 1)),
            "//a/" => ok("/".into(), ("/a/", 1)),
        });
    }
}
