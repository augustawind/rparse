#[macro_use]
extern crate rparse;

use std::iter::FromIterator;

use rparse::parser::choice::optional;
use rparse::parser::combinator::{many, many1};
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
    range("GET".into())
        .or(range("POST".into()))
        .or(range("PUT".into()))
        .or(range("PATCH".into()))
        .or(range("DELETE".into()))
        .or(range("HEAD".into()))
        .or(range("OPTIONS".into()))
        .or(range("CONNECT".into()))
        .or(range("TRACE".into()))
}

// fn url_path<'a, S, O>() -> impl Parser<Stream = S, Output = O>
// where
//     S: Stream,
//     S::Item: From<char> + Into<char>,
//     S::Range: From<&'a str> + FromIterator<S::Item>,
//     O: FromIterator<S::Range>,
// {
//     range("/".into())
//         .then(many(range("/".into()).or(many(url_path_segment()))))
//         .then(optional(range(".".into()).then(many1(url_path_segment()))))
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
