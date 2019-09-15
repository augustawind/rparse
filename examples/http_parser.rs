#[macro_use]
extern crate rparse;

use rparse::parser::range::range;
use rparse::parser::seq::{many, many1};
use rparse::parser::token::{ascii, token};
use rparse::{Parser, Stream};

// fn http_request_line<S>() -> impl Parser<Stream = S, Output = Vec<S::Range>>
// where
//     S: Stream,
// {
//     http_method()
//         .wrap()
//         .extend(concat![
//             token(b' ').and(uri_path()),
//             token(b' ').and(http_version()),
//         ])
//         .append(range("\r\n"))
// }

fn http_version<S>() -> impl Parser<Stream = S, Output = Vec<S::Range>>
where
    S: Stream,
{
    range("HTTP/").then(choice![range("1"), range("1.1"), range("2")])
}

fn http_method<S>() -> impl Parser<Stream = S, Output = S::Range>
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
}

// TODO:
//  - fn optional(p: Parser)    ~>  ignore result if subparser fails
//      *~> should this be first-class Parser functionality?
//
fn uri_path<S>() -> impl Parser<Stream = S, Output = Vec<S::Item>>
where
    S: Stream,
{
    // a URI path is a forward slash
    token(b'/').wrap().extend(
        // (optional) one or more path segments, which consist of any arrangement of
        many(
            // forward slashes and URI path segments
            many1(token(b'/')).or(uri_segment()),
        )
        .flatten(),
    )
}

fn uri_segment<S>() -> impl Parser<Stream = S, Output = Vec<S::Item>>
where
    S: Stream,
{
    // one or more URI-safe characters, or a percent-encoded octets
    many1(uri_token()).or(percent_encoded())
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

fn main() {}

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
