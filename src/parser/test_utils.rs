macro_rules! test_parser {
    ($stream_type:ty | $parser:expr, {
        $($stream:expr => $expected:tt),+
        $(,)*
    }) => {
        $(
            let result: $crate::error::ParseResult<$stream_type, _> =
                $parser.parse($stream.into());
            assert_parse_result_eq!(result => $expected);
        )+
    };

    ($stream_type:ty | $parser:expr, {
        $($stream:expr => $expected:tt),+
        $(,)*
    }, {
        $($stream2:expr => $expected2:tt),+
        $(,)*
    }) => {
        test_parser!($stream_type | $parser, {
            $($stream => $expected),+,
        });
        test_parser_errors!($stream_type | $parser, {
            $($stream2 => $expected2),+,
        });
    };
}

macro_rules! assert_parse_result_eq {
    ($result:expr => ($expected_result:expr, $expected_stream:expr, $expected_pos:expr)) => {
        let (parsed_result, parsed_stream) = $result;
        assert_eq!(parsed_result, $expected_result);
        let expected_stream = $crate::stream::State::new($expected_stream, $expected_pos);
        assert_eq!(parsed_stream, expected_stream);
    };
    ($result:expr => ($expected_result:expr, $expected_stream:expr)) => {
        let (parsed_result, parsed_stream) = $result;
        assert_eq!(parsed_result, $expected_result);
        assert_eq!(parsed_stream, $expected_stream);
    };
}

macro_rules! test_parser_errors {
    ($stream_type:ty | $parser:expr, {
        $($stream:expr => $expected:tt),+
        $(,)*
    }) => {
        $(
            let stream: $stream_type = $stream.into();
            let result: $crate::error::ParseResult<$stream_type, _> =
                $parser.parse(stream.clone());
            assert_parser_errors!(stream => result => $expected);
        )+
    };

    ($stream_type:ty | $parser:expr, {
        $($stream:expr => at $pos:expr; $expected:tt),+
        $(,)*
    }) => {
        $(
            let stream: $stream_type = $stream.into();
            let result: $crate::error::ParseResult<$stream_type, _> =
                $parser.parse(stream.clone());
            assert_parser_errors!(stream => result => $pos; $expected);
        )+
    };
}

macro_rules! assert_parser_errors {
    ($stream:expr => $result:expr => $pos:expr; $($predicate:expr),+) => {
        let (errors, stream) = unwrap_errors_with!($result, $($predicate),+);
        assert_eq!(errors.position, $pos.into());
        assert_eq!(stream, $stream);
    };
    ($stream:expr => $result:expr => $($predicate:expr),+) => {
        let (_, stream) = unwrap_errors_with!($result, $($predicate),+);
        assert_eq!(stream, $stream);
    };
    ($result:expr => $pos:expr; $($predicate:expr),+) => {
        let (errors, _) = unwrap_errors_with!($result, $($predicate),+);
        assert_eq!(errors.position, $pos.into());
    };
    ($result:expr => $($predicate:expr),+) => {
        unwrap_errors_with!($result, $($predicate),+);
    };
}

macro_rules! unwrap_errors_with {
    ($parsed:expr, $($predicate:expr),*) => {{
        let (parsed_err, stream) = $parsed;
        let errors = parsed_err.expect_err("assertion failed: expected Err(_)");
        assert_has_error_with!(errors, $($predicate),*);
        (errors, stream)
    }};
}

macro_rules! assert_has_error_with {
    ($errors:expr, $($predicate:expr)*) => {
        assert!(
            $errors.errors.iter().any(|err| {
                $(
                    if !$predicate(err) {
                        return false;
                    }
                )*
                true
            }),
            "no Errors satisfied all predicates",
        );
    };
}

// FIXME: phase this out
macro_rules! assert_parse_err {
    ($parsed:expr) => {
        let (parsed, _) = $parsed;
        assert!(parsed.is_err());
    };
    ($parsed:expr, $stream:expr) => {
        let (parsed, stream) = $parsed;
        assert!(parsed.is_err());
        assert_eq!(stream, $stream);
    };
    ($parsed:expr, $stream:expr, $type:ty | $position:expr) => {
        let (parsed, stream) = $parsed;
        assert!(parsed.is_err());
        assert_eq!(
            stream,
            $crate::stream::State::<_, $type>::new($stream, $position)
        );
    };
}

macro_rules! is_match {
    ($pattern:pat = $value:expr) => {
        match $value {
            $pattern => true,
            _ => false,
        }
    };
}
