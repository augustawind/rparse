use rparse::parser::{
    item::{one_of, satisfy},
    range::range,
    repeat::many1,
};
use rparse::stream::StreamItem;
use rparse::{Parser, Stream};

pub static SEPARATORS: &'static [u8] = &[
    b'(', b')', b'<', b'>', b'@', b',', b';', b':', b'\\', b'"', b'/', b'[', b']', b'?', b'=',
    b'{', b'}', b' ', b'\t',
];

pub fn token<S: Stream>() -> impl Parser<Stream = S, Output = S::Item> {
    satisfy(|item: &S::Item| {
        let separators = SEPARATORS
            .iter()
            .map(|&b| b.into())
            .collect::<Vec<S::Item>>();
        !(item.is_ascii_control() || separators.contains(item))
    })
}

pub fn lws<S: Stream>() -> impl Parser<Stream = S, Output = ()> {
    many1(one_of(&[b' ', b'\t']).map(|_| ()))
}

fn crlf<S: Stream>() -> impl Parser<Stream = S> {
    range("\r\n")
}

#[cfg(test)]
mod test {
    use super::*;
    use rparse::stream::IndexedStream;
    use rparse::Error;

    #[test]
    fn test_token() {
        for c in 0u8..=32 {
            let input = format!("{}_foo", c as char);
            let stream = IndexedStream::<&str>::from(input.as_ref());
            assert_eq!(
                token().parse(stream.clone()),
                Err(((0, vec![Error::unexpected_item(c as char)]).into(), stream)),
                "unexpectedly parsed '{}': should fail parsing control characters",
                c as char,
            );
        }
        for &item in SEPARATORS.iter() {
            let input = [item, b'\n'];
            let stream = IndexedStream::from(&input[..]);
            assert_eq!(
                token().parse(stream.clone()),
                Err(((0, vec![Error::unexpected_item(item)]).into(), stream)),
                "unexpectedly parsed '{}': should fail parsing SEPARATORS",
                item as char,
            );
        }
    }
}
