#[macro_use]
extern crate rparse;

use std::iter::FromIterator;

// use rparse::parser::choice::optional;
use rparse::parser::seq::many; //{many, many1};
use rparse::parser::range::range;
use rparse::parser::token::{ascii, token};
use rparse::{Parser, Stream};

fn http_version<'a, S, O>() -> impl Parser<Stream = S, Output = O>
where
    S: Stream,
    S::Range: From<&'a str>,
    O: FromIterator<S::Range>,
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
// fn url_path<'a, S, O>() -> impl Parser<Stream = S, Output = O>
// where
//     S: Stream,
//     S::Item: From<char> + Into<char>,
//     S::Range: From<&'a str> + FromIterator<S::Item>,
//     O: FromIterator<S::Range>,
// {
//     // a url path is
//     seq![
//         // a forward slash
//         token('/').s(),
//         // optionally** followed by
//         seq![
//             // one or more path segments, which consist of any arrangement of
//             many1(choice![
//                 // forward slashes
//                 token('/').s(),
//                 // url-safe characters
//                 url_token().s(),
//                 // and percent encoded octets
//                 percent_encoded(),
//             ]).concat(),
//             // optionally** followed by an extension, which is
//             seq![
//                 // a period
//                 token('.').s(),
//                 // and some url-safe text
//                 many1(choice![url_token().s(), percent_encoded()]).concat(),
//             ],
//         ],
//     ]
// }

fn url_path_segment<'a, S, O>() -> impl Parser<Stream = S, Output = O>
where
    S: Stream,
    S::Item: From<char> + Into<char> + FromIterator<S::Item>,
    O: FromIterator<S::Item>,
{
    choice![many(url_token()), percent_encoded()]
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

fn percent_encoded<S, O>() -> impl Parser<Stream = S, Output = O>
where
    S: Stream,
    S::Item: From<char> + Into<char> + FromIterator<S::Item>,
    O: FromIterator<S::Item>,
{
    token('%'.into())
        .then(ascii::hexdigit())
        .then(ascii::hexdigit())
}

fn main() {}
