#[macro_use]
extern crate rparse;

use rparse::parser::range::range;
use rparse::parser::seq::{many, many1};
use rparse::parser::token::{ascii, token};
use rparse::{Parser, Stream};

fn http_version<'a, S>() -> impl Parser<Stream = S, Output = Vec<S::Range>>
where
    S: Stream,
{
    range("HTTP/").then(choice![range("1"), range("1.1"), range("2")])
}

fn http_method<'a, S>() -> impl Parser<Stream = S, Output = S::Range>
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
fn url_path<S>() -> impl Parser<Stream = S, Output = Vec<S::Item>>
where
    S: Stream,
    S::Item: From<char> + Into<char>,
{
    // a url path is a forward slash
    token(b'/').wrap().extend(
        // (optional) one or more path segments, which consist of any arrangement of
        many(
            // forward slashes and url path segments
            many1(token(b'/')).or(url_segment_part()),
        ).flatten(),
    )
}

fn url_segment_part<S>() -> impl Parser<Stream = S, Output = Vec<S::Item>>
where
    S: Stream,
    S::Item: From<char> + Into<char>,
{
    // one or more url-safe characters, or a percent-encoded octets
    many1(url_token()).or(percent_encoded())
}

fn url_token<S>() -> impl Parser<Stream = S, Output = S::Item>
where
    S: Stream,
    S::Item: From<char> + Into<char>,
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
    S::Item: From<char> + Into<char>,
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
        ].into_iter()
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
            "" => err(vec![Error::EOF, Error::expected_token('%')]),
            "%xy" => err(vec![Error::unexpected_token('x')]),
        });
    }

    #[test]
    fn test_url_path() {
        test_parser!(IndexedStream<&str> => String | url_path().collect(), {
            "/" => ok(Ok("/".to_string()), ("", 1)),
            "/my_img.jpeg" => ok(Ok("/my_img.jpeg".to_string()), ("", 12)),
            "//a/b//``" => ok(Ok("//a/b//".to_string()), ("``", 7)),
            "/%%bc" => ok(Ok("/".to_string()), ("%%bc", 1)),
            "my_img.jpeg" => err(0, vec![Error::unexpected_token('m'), Error::expected_token('/')]),
        });
    }
}
