#[macro_use]
extern crate rparse;

use rparse::parser::range::range;
use rparse::parser::seq::{many, many1};
use rparse::parser::token::{ascii, token};
use rparse::stream::{IndexedStream, StreamRange};
use rparse::{Parser, Stream};

fn http_request_line<S>() -> impl Parser<Stream = S, Output = Vec<String>>
where
    S: Stream,
{
    // an HTTP request line is an HTTP method
    http_method()
        // followed by a URI
        .then(token(b' ').and(uri()))
        // followed by an HTTP version
        .append(token(b' ').and(http_version()))
        .append(linebreak())
}

fn http_version<S>() -> impl Parser<Stream = S, Output = String>
where
    S: Stream,
{
    // an HTTP version is
    (
        // the text "HTTP/"
        range("HTTP/"),
        // followed by a version number
        choice![range("1.1"), range("1"), range("2")],
    )
        .map(|(httpslash, version): (S::Range, S::Range)| {
            format!("{}{}", httpslash.to_string(), version.to_string())
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
    .map(StreamRange::to_string)
}

fn uri<S>() -> impl Parser<Stream = S, Output = String>
where
    S: Stream,
{
    // a URI is
    (
        // a scheme
        uri_scheme(),
        // followed by a path
        uri_path(),
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
        choice![range("http"), range("https")],
        // followed by a delimiter
        range("://"),
    )
        .map(|(r0, r1): (S::Range, S::Range)| format!("{}{}", r0.to_string(), r1.to_string()))
}

// TODO:
//  - fn optional(p: Parser)    ~>  ignore result if subparser fails
//      *~> should this be first-class Parser functionality?
//
fn uri_path<S>() -> impl Parser<Stream = S, Output = String>
where
    S: Stream,
{
    choice![
        concat![
            token(b'/').optional(),
            uri_segment(),
            many(concat![
                token(b'/').wrap(),
                uri_segment(),
            ]).flatten(),
        ],
        token(b'/').wrap(),
        concat![
            uri_segment(),
            many(concat![
                token(b'/').wrap(),
                uri_segment(),
            ]).flatten(),
        ],
    ]
        .optional()
        .flatten()
        .map(|s: Vec<S::Item>| s.into_iter().map(Into::into).collect())
}

fn uri_segment<S>() -> impl Parser<Stream = S, Output = Vec<S::Item>>
where
    S: Stream,
{
    // a URI segment is one or more URI-safe characters
    many1(uri_token())
        // or a percent-encoded octet
        .or(percent_encoded())
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

fn linebreak<S>() -> impl Parser<Stream = S, Output = String>
where
    S: Stream,
{
    range("\r\n").map(StreamRange::to_string)
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
    use rparse::Error;

    // TODO: [u8]
    #[test]
    fn test_http_method() {
        let method_errors: Vec<Error<IndexedStream<&[u8]>>> = vec![
            "GET", "PUT", "POST", "HEAD", "PATCH", "TRACE", "DELETE", "OPTIONS", "CONNECT",
        ]
        .into_iter()
        .map(|method| Error::expected_range(method.as_bytes()))
        .collect();

        test_parser!(IndexedStream<&[u8]> => &[u8] | http_method(), {
            &b"GET"[..] => ok(Ok(&b"GET"[..]), (&b""[..], 3)),
            &b"HEAD\n/"[..] => ok(Ok(&b"HEAD"[..]), (&b"\n/"[..], 4)),
            &b"GARBLEDIGOOK"[..] => err(0, method_errors.clone()),
        });

        assert_eq!(http_method().parse("TRACE it"), (Ok("TRACE"), " it"));
    }

    #[test]
    fn test_percent_encoded() {
        test_parser!(&str => String | percent_encoded().collect(), {
            "%A9" => ok(Ok("%A9".to_string()), ""),
            "%0f/hello" => ok(Ok("%0f".to_string()), "/hello"),
            "" => err(vec![Error::unexpected_eoi(), Error::expected_token('%')]),
            "%xy" => err(vec![Error::unexpected_token('x')]),
        });
    }

    #[test]
    fn test_uri_path() {
        test_parser!(IndexedStream<&str> => String | uri_path().collect(), {
            "/" => ok(Ok("/".to_string()), ("", 1)),
            "/my_img.jpeg" => ok(Ok("/my_img.jpeg".to_string()), ("", 12)),
            "//a/b//``" => ok(Ok("//a/b//".to_string()), ("``", 7)),
            "/%%bc" => ok(Ok("/".to_string()), ("%%bc", 1)),
            "my_img.jpeg" => err(0, vec![Error::unexpected_token('m'), Error::expected_token('/')]),
        });
    }
}
