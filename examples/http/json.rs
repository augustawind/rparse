use serde_json::{Map, Number, Value};

use rparse::parser::{
    combinator::attempt,
    item::{ascii, item},
    parser,
    range::range,
    repeat::{many, many1, sep_by},
    seq::between,
};
use rparse::stream::RangeStream;
use rparse::{ParseResult, Parser, Stream};

use common::quoted_string;

trait JSONParser<S: Stream> = Parser<Stream = S, Output = Value>;

pub fn json_value<S: Stream>() -> fn(S) -> ParseResult<S, Value> {
    parser(|stream| choice![null(), boolean(), number(), string(), array(), object()].parse(stream))
}

fn null<S: Stream>() -> impl JSONParser<S> {
    attempt(range("null")).map(|_| Value::Null)
}

fn boolean<S: Stream>() -> impl JSONParser<S> {
    range("true")
        .or(range("false"))
        .expect("a JSON boolean value")
        .map(|r| Value::Bool(r == S::Range::from_str("true")))
}

fn number<S: Stream>() -> impl JSONParser<S> {
    concat![
        many1(ascii::digit()),
        item(b'.').wrap().optional(),
        many(ascii::digit()),
    ]
    .collect_string()
    .from_str::<f64>()
    .bind(|n: f64, stream: S| {
        let num = Value::Number(match Number::from_f64(n) {
            Some(num) => num,
            None => return stream.err(format!("{} is not a valid JSON number", n).into()),
        });
        stream.ok(num)
    })
}

fn string<S: Stream>() -> impl JSONParser<S> {
    quoted_string().map(|s| Value::String(s))
}

fn array<S: Stream>() -> impl JSONParser<S> {
    between(
        item(b'[').skip(sp()),
        item(b']'),
        sep_by(json_value().skip(sp()), item(b',').skip(sp())),
    )
    .map(|vec| Value::Array(vec))
}

fn object<S: Stream>() -> impl JSONParser<S> {
    between(
        item(b'{').skip(sp()),
        item(b'}'),
        sep_by::<Map<String, Value>, _, _>(
            (
                quoted_string().skip(sp()),
                item(b':').skip(sp()),
                json_value().skip(sp()),
            )
                .map(|(k, _, v)| (k, v)),
            item(b',').skip(sp()),
        ),
    )
    .map(|map| Value::Object(map))
}

fn sp<S: Stream>() -> impl Parser<Stream = S, Output = ()> {
    many::<(), _>(ascii::whitespace().map(|_| ()))
}

#[cfg(test)]
mod test {
    use super::*;
    use rparse::stream::IndexedStream;
    use rparse::Error;

    type IdxStr = IndexedStream<&'static str>;

    #[test]
    fn test_null() {
        let mut p = null();
        test_parser!(IdxStr => Value | p, {
            "null" => ok(Value::Null, ("", 4)),
            "null, " => ok(Value::Null, (", ", 4)),
            "" => err(Error::eoi().at(0).expected_range("null")),
            "nul" => err(Error::eoi().at(3).expected_range("null")),
            " null" => err(Error::item(' ').at(0).expected_range("null")),
        });
    }

    #[test]
    fn test_boolean() {
        let mut p = boolean();
        test_parser!(IdxStr => Value | p, {
            "true" => ok(Value::Bool(true), ("", 4)),
            "false" => ok(Value::Bool(false), ("", 5)),
            "true, " => ok(Value::Bool(true), (", ", 4)),
            "" => err(Error::eoi().at(0).expected("a JSON boolean value")),
            // FIXME: the error be eoi().at(3)
            "tru" => err(Error::item('t').at(0).expected("a JSON boolean value")),
            " false" => err(Error::item(' ').at(0).expected("a JSON boolean value")),
        });
    }

    #[test]
    fn test_sp() {
        let mut p = sp();
        test_parser!(IdxStr => () | p, {
            "" => ok((), ("", 0)),
            " " => ok((), ("", 1)),
            "\t\r\n 123" => ok((), ("123", 4)),
            "1\t" => ok((), ("1\t", 0)),
        });
    }
}
