#[macro_use]
extern crate rparse;

// use rparse::parser::choice::optional;
use rparse::parser::range::range;
use rparse::parser::seq::{many, many1};
use rparse::parser::token::{ascii, token};
use rparse::{Parser, Stream};

fn http_version<'a, S>() -> impl Parser<Stream = S, Output = Vec<S::Range>>
where
    S: Stream,
    S::Range: From<&'a str>,
{
    range("HTTP/".into()).then(choice![
        range("1".into()),
        range("1.1".into()),
        range("2".into())
    ])
}

fn http_method<'a, S>() -> impl Parser<Stream = S>
where
    S: Stream,
    S::Range: From<&'a str>,
{
    choice![
        range("GET".into()),
        (range("POST".into())),
        (range("PUT".into())),
        (range("PATCH".into())),
        (range("DELETE".into())),
        (range("HEAD".into())),
        (range("OPTIONS".into())),
        (range("CONNECT".into())),
        (range("TRACE".into())),
    ]
}

// FIXME: this needs new methods/functions/macros!
// TODO:
//  - fn parser.s()             ~>  convert parser output from token to stream
//  - fn parser.concat()        ~>  flatten nested parser output
//  - fn optional(p: Parser)    ~>  ignore result if subparser fails
//      *~> should this be first-class Parser functionality?
//  - macro seq!                ~>  chain multiple parsers
//
fn url_path<'a, S>() -> impl Parser<Stream = S, Output = Vec<S::Item>>
where
    S: Stream,
    S::Item: From<char> + Into<char>,
{
    // a url path is a forward slash
    token('/'.into()).wrap().extend(concat![
        // (optional) one or more path segments, which consist of any arrangement of
        many1(
            // forward slashes and url segments
            many(token('/'.into())).or(url_segment_chars()),
        ).flatten(),
        // (optional) and an extension, which is
        concat![
            // a period
            token('.'.into()).wrap(),
            // and one or more segment characters
            many1(url_segment_chars()).flatten(),
        ],
    ])
}

fn url_segment_chars<'a, S>() -> impl Parser<Stream = S, Output = Vec<S::Item>>
where
    S: Stream,
    S::Item: From<char> + Into<char>,
{
    // one or more url-safe characters, or a percent-encoded octets
    many1(url_token()).or(percent_encoded())
}

fn url_token<'a, S>() -> impl Parser<Stream = S, Output = S::Item>
where
    S: Stream,
    S::Item: From<char> + Into<char>,
{
    choice![
        ascii::alpha_num(),
        token('-'.into()),
        token('.'.into()),
        token('_'.into()),
        token('~'.into())
    ]
}

fn percent_encoded<S>() -> impl Parser<Stream = S, Output = Vec<S::Item>>
where
    S: Stream,
    S::Item: From<char> + Into<char>,
{
    token('%'.into())
        .then(ascii::hexdigit())
        .append(ascii::hexdigit())
}

fn main() {}
