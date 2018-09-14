macro_rules! test_parser {
    ($stream_type:ty | $p:expr, {
        $($stream:expr => $expected:tt);+;
    }) => {
        $(
            let result: $crate::error::ParseResult<$stream_type, _> =
                $p.parse($stream.into());
            assert_parse_result_eq!(result => $expected);
        )+
    };

    ($stream_type:ty | $p:expr, {
        $($stream:expr => $expected:tt);+;
    }, {
        $($stream2:expr => $expected_err:expr);+;
    }) => {
        test_parser!($stream_type | $p, {
            $($stream => $expected);+;
        });
        test_parser_errors!($stream_type | $p, {
            $($stream2 => $expected_err);+;
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
    ($stream_type:ty | $p:expr, {
        $($stream:expr => $expected:expr);+;
    }) => {
        $(
            let _ = test_parser_errors!(@test $stream_type, $p, $stream, $expected);
        )+
    };

    ($stream_type:ty | $p:expr, {
        $($stream:expr => at $pos:expr; $expected:expr);+;
    }) => {
        $(
            let (errors, _) = test_parser_errors!(@test $stream_type, $p, $stream, $expected);
            assert_eq!(errors.position, $pos.into());
        )+
    };

    (@test $stream_type:ty, $p:expr, $stream:expr, $expected:expr) => {{
        let input: $stream_type = $stream.into();
        let (result, stream): $crate::error::ParseResult<$stream_type, _> =
            $p.parse(input.clone());
        let errors = result.expect_err("assertion failed: expected an Err(_)");
        assert_eq!(errors.errors, $expected);
        assert_eq!(stream, input);
        (errors, stream)
    }};
}

macro_rules! assertions {
    (with $pattern:pat = $value:expr;  {  }) => {};

    (with $pattern:pat = $value:expr;  { $head:tt, $($tail:tt)* }) => {
        #[allow(unused_variables)]
        let $pattern = $value;
        assertions!(@expand $head);
        assertions!(with $pattern = $value; { $($tail)* });
    };

    (@expand $head:tt) => {
        assert!($head);
    };

    (@expand ($left:tt == $right:expr)) => {
        assert_eq!($left, $right);
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn test_assertions_macro() {
        assertions!(with (x, _) = (5, 6); {
            (x == 5),
            (x > 4),
            (x < 6),
        });
    }
}
