#[macro_use]
extern crate lazy_static;
extern crate regex;

#[macro_use]
extern crate rparse;

mod common;
mod headers;
mod request_line;

use rparse::stream::IndexedStream;
use rparse::{Parser, Stream};

use common::crlf;
use headers::{headers, Headers};
use request_line::request_line;

fn request<S: Stream>() -> impl Parser<Stream = S, Output = ((String, String, String), Headers)> {
    (request_line(), crlf(), headers()).map(|(r, _, h)| (r, h))
}

fn main() {
    let stream = IndexedStream::from("GET http://foo.bar/I%20like%20/50 HTTP/1.1\r\n\r\n");
    match request().must_parse(stream) {
        Ok((result, _)) => {
            println!("Parsing succeeded!");
            dbg!(result);
        }
        Err((err, _)) => {
            println!("Parsing failed!");
            dbg!(err);
        }
    };
}
