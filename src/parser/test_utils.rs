macro_rules! test_parser {
    ($stream_type:ty | $p:expr, {
        $($into_input:expr => $expected:expr);+;
    }) => {
        $(
            let result: $crate::ParseResult<$stream_type, _> = $p.parse($into_input.into());
            test_parser!(@ok $stream_type, result, $expected);
        )+
    };

    (@ok $stream_type:ty, $result:expr, $expected:expr) => {
        let (parsed_result, parsed_stream) = $result;
        let (expected_result, into_expected_stream) = $expected;
        let expected_stream: $stream_type = into_expected_stream.into();
        assert_eq!(parsed_result, expected_result);
        assert_eq!(parsed_stream, expected_stream);
    };

    ($stream_type:ty | $p:expr, {
        $($stream:expr => $expected:expr);+;
    }, {
        $($stream_err:expr => $expected_err:expr);+;
    }) => {
        test_parser!($stream_type | $p, {
            $($stream => $expected);+;
        });
        test_parser_errors!($stream_type | $p, {
            $($stream_err => $expected_err);+;
        });
    };
}

macro_rules! test_parser_errors {
    ($stream_type:ty | $p:expr, {
        $($stream:expr => $expected:expr);+;
    }) => {
        $(
            let input: $stream_type = $stream.into();
            let (result, stream): $crate::error::ParseResult<$stream_type, _> =
                $p.parse(input.clone());
            let errors = result.expect_err("assertion failed: expected an Err(_)");
            let expected_errors: $crate::error::Errors<$stream_type, <$stream_type as $crate::stream::Stream>::Position> = $expected.into();
            assert_eq!(errors, expected_errors);
            assert_eq!(stream, input);
        )+
    };
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
