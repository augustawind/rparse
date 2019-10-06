use serde_json::{Map, Number, Value};

use rparse::parser::{
    choice::optional,
    combinator::attempt,
    item::{
        ascii::{digit, whitespace},
        item, one_of, satisfy,
    },
    parser,
    range::range,
    repeat::{many, many1, sep_by},
    seq::between,
};
use rparse::stream::{RangeStream, StreamItem};
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
        .expect("a boolean value")
        .map(|r| Value::Bool(r == S::Range::from_str("true")))
}

fn number<S: Stream>() -> impl JSONParser<S> {
    let non_zero_digit = satisfy(|&b: &S::Item| b.is_ascii_digit() && b != b'0'.into());
    let exponent = one_of(&[b'e', b'E'])
        .then(optional(item(b'-')))
        .extend(many1(digit()));

    concat![
        // an optional minus sign,
        optional(item(b'-')).wrap(),
        // followed by either
        choice![
            // a zero
            item(b'0').wrap(),
            // or a non-zero digit followed by zero or more digits
            non_zero_digit.wrap().extend(many(digit())),
        ],
        // followed by an optional decimal point and zero or more digits
        optional(item(b'.').wrap().extend(many1(digit()))),
        // followed by an optional exponent
        optional(exponent)
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
    .expect("a number")
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
    many::<(), _>(whitespace().map(|_| ()))
}

#[cfg(test)]
mod test {
    use super::*;
    use rparse::stream::IndexedStream;
    use rparse::Error;

    #[test]
    fn test_json_value() {
        test_parser!(IndexedStream<&str> => Value | json_value(), {
            "null" => ok(json!(null), ("", 4)),
            "true" => ok(json!(true), ("", 4)),
            "1.5e-3" => ok(json!(1.5e-3), ("", 6)),
            r#""heyo","# => ok(json!("heyo"), (",", 6)),
            r#""""# => ok(json!(""), ("", 2)),
            r#"["hey", null, -3.2, {"foo": [false]}, [true]],"# => ok(
                json!(["hey", null, -3.2, {"foo": [false]}, [true]]),
                (",", 45),
            ),
        });
    }

    #[test]
    fn test_null() {
        test_parser!(IndexedStream<&str> => Value | null(), {
            "null" => ok(json!(null), ("", 4)),
            "null, " => ok(json!(null), (", ", 4)),
            "" => err(Error::eoi().at(0).expected_range("null")),
            "nul" => err(Error::eoi().at(3).expected_range("null")),
            " null" => err(Error::item(' ').at(0).expected_range("null")),
        });
    }

    #[test]
    fn test_boolean() {
        test_parser!(IndexedStream<&str> => Value | boolean(), {
            "true" => ok(json!(true), ("", 4)),
            "false" => ok(json!(false), ("", 5)),
            "true, " => ok(json!(true), (", ", 4)),
            "" => err(Error::eoi().at(0).expected("a boolean value")),
            // FIXME: the error be eoi().at(3)
            "tru" => err(Error::item('t').at(0).expected("a boolean value")),
            " false" => err(Error::item(' ').at(0).expected("a boolean value")),
        });
    }

    #[test]
    fn test_number() {
        test_parser!(IndexedStream<&str> => Value | number(), {
            "0" => ok(json!(0f64), ("", 1)),
            "3" => ok(json!(3f64), ("", 1)),
            "320" => ok(json!(320f64), ("", 3)),
            "1.5" => ok(json!(1.5), ("", 3)),
            "1.50" => ok(json!(1.5), ("", 4)),
            "1.55" => ok(json!(1.55), ("", 4)),
            "11.5" => ok(json!(11.5), ("", 4)),
            "0.5" => ok(json!(0.5), ("", 3)),
            "0.52" => ok(json!(0.52), ("", 4)),
            "-3" => ok(json!(-3f64), ("", 2)),
            "-320" => ok(json!(-320f64), ("", 4)),
            "-1.5" => ok(json!(-1.5), ("", 4)),
            "-1.50" => ok(json!(-1.5), ("", 5)),
            "-1.55" => ok(json!(-1.55), ("", 5)),
            "-11.5" => ok(json!(-11.5), ("", 5)),
            "-0.5" => ok(json!(-0.5), ("", 4)),
            "-0.52" => ok(json!(-0.52), ("", 5)),
            "3e2" => ok(json!(3e2f64), ("", 3)),
            "320e10" => ok(json!(320e10f64), ("", 6)),
            "1.5e-3" => ok(json!(1.5e-3), ("", 6)),
            "1.50E-0" => ok(json!(1.5e-0), ("", 7)),
            "-3e2" => ok(json!(-3e2f64), ("", 4)),
            "-320e10" => ok(json!(-320e10f64), ("", 7)),
            "-1.5e-3" => ok(json!(-1.5e-3), ("", 7)),
            "-1.50E-0" => ok(json!(-1.5e-0), ("", 8)),
            "000" => ok(json!(0f64), ("00", 1)),
            "1..0" => ok(json!(1f64), ("..0", 1)),
            "--1" => err(Error::item('-').at(1).expected("a number")),
        });
    }

    #[test]
    fn test_string() {
        test_parser!(IndexedStream<&str> => Value | string(), {
            r#""""# => ok(json!(""), ("", 2)),
            r#""heyo","# => ok(json!("heyo"), (",", 6)),
            r#""a\"b\"\n""# => ok(json!("a\"b\"\n"), ("", 10)),
            "foo" => err(Error::item('f').at(0).expected_item('"')),
            r#""foo"# => err(Error::eoi().at(4).expected_item('"')),
        });
    }

    #[test]
    fn test_sp() {
        test_parser!(IndexedStream<&str> => () | sp(), {
            "" => ok((), ("", 0)),
            " " => ok((), ("", 1)),
            "\t\r\n 123" => ok((), ("123", 4)),
            "1\t" => ok((), ("1\t", 0)),
        });
    }
}
