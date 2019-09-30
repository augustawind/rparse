#[macro_use]
extern crate rparse;

mod common;
mod request_line;

use rparse::stream::IndexedStream;
use rparse::Parser;

use request_line::request_line;

fn main() {
    let stream = IndexedStream::from("GET http://foo.bar/I%20like%20/50 HTTP/1.1\r\n");
    match request_line().must_parse(stream) {
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
