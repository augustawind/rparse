#[macro_use]
extern crate rparse;

use rparse::parser::range::range;
use rparse::parser::seq::{many, many1};
use rparse::parser::token::{ascii, token};
use rparse::stream::IndexedStream;
use rparse::{Parser, Stream};

fn http_request_line<S>() -> impl Parser<Stream = S, Output = Vec<String>>
where
    S: Stream,
{
    seq![
        // an HTTP method
        http_method(),
        // followed by a URI
        token(b' ').and(uri()),
        // followed by an HTTP version
        token(b' ').and(http_version()),
        // terminated by a \r\n
        crlf(),
    ]
}

fn http_version<S>() -> impl Parser<Stream = S, Output = String>
where
    S: Stream,
{
    // an HTTP version is
    (
        // the text "HTTP/"
        range("HTTP/").as_string(),
        // followed by a version number
        choice![range("1.1"), range("1"), range("2")].as_string(),
    )
        .map(|(httpslash, version)| {
            format!("{}{}", httpslash, version)
        })
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
    (
        // a scheme
        uri_scheme(),
        // optionally followed by a path
        uri_path().optional(),
    )
        .map(|(r0, r1)| format!("{}{}", r0, r1))
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

fn main() {
    let stream = IndexedStream::from("GET http://foo.bar/I%20like%20/50 HTTP/1.1\r\n");
    match http_request_line().parse(stream) {
        (Ok(result), _) => {
            println!("Parsing succeeded!");
            dbg!(result);
        }
        (Err(err), _) => {
            println!("Parsing failed!");
            dbg!(err);
        }
    };
}

#[cfg(test)]
mod test {
    use super::*;
    use rparse::stream::IndexedStream;
    use rparse::Error::{self, *};

    // TODO: [u8]
    #[test]
    fn test_http_method() {
        test_parser!(IndexedStream<&[u8]> => String | http_method(), {
            &b"GET"[..] => ok("GET".into(), (&b""[..], 3)),
            &b"HEAD\n/"[..] => ok("HEAD".into(), (&b"\n/"[..], 4)),
        });

        test_parser!(IndexedStream<&[u8]> => String | http_method(), {
            &b"PUPPYDOG"[..] => err(2, vec![
                Error::unexpected_token(b'P'),
                Error::expected_range("PUT".as_bytes()),
            ]),
        });

        assert_eq!(
            http_method().parse("TRACE it"),
            (Ok(Some("TRACE".into())), " it")
        );
    }

    #[test]
    fn test_percent_encoded() {
        test_parser!(&str => String | percent_encoded().collect(), {
            "%A9" => ok("%A9".into(), ""),
            "%0f/hello" => ok("%0f".into(), "/hello"),
            "" => err(vec![Error::unexpected_eoi(), Error::expected_token('%')]),
            "%xy" => err(vec![
                Error::unexpected_token('x'),
                Expected("a hexadecimal digit".into()),
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
