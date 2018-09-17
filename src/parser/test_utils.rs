#[macro_export]
macro_rules! test_parser {
    ($stream_type:ty | $p:expr, {
        $($into_input:expr => $assertion:ident $expected:expr);+;
    }) => {
        $(
            test_parser!(@dispatch $assertion, $p, $stream_type, $into_input => $expected);
        )+
    };

    (@dispatch ok, $p:expr, $stream_type:ty, $into_input:expr => $expected:expr) => {
        let input: $stream_type = $into_input.into();
        let (result, stream): $crate::ParseResult<$stream_type, _> = $p.parse(input.clone());
        let (expected_result, into_expected_stream) = $expected;
        let expected_stream: $stream_type = into_expected_stream.into();
        assert_eq!(result, expected_result);
        assert_eq!(stream, expected_stream);
    };

    (@dispatch err, $p:expr, $stream_type:ty, $into_input:expr => $expected:expr) => {
        let input: $stream_type = $into_input.into();
        let (result, stream): $crate::ParseResult<$stream_type, _> = $p.parse(input.clone());
        let result = result.expect_err("assertion failed: expected an Err(_)");
        let expected_result: $crate::error::Errors<
            $stream_type,
            <$stream_type as $crate::stream::Stream>::Position,
        > = $expected.into();
        let expected_stream = input;
        assert_eq!(result, expected_result);
        assert_eq!(stream, expected_stream);
    };

    // Old API

    ($stream_type:ty | $p:expr, {
        $($stream:expr => $expected:expr);+;
    }, {
        $($stream_err:expr => $expected_err:expr);+;
    }) => {
        test_parser!($stream_type | $p, {
            $($stream => $expected);+;
        });
        test_parser!(@test_errors $stream_type | $p, {
            $($stream_err => $expected_err);+;
        });
    };

    ($stream_type:ty | $p:expr, {
        $($into_input:expr => $expected:expr);+;
    }) => {
        $(
            let result: $crate::ParseResult<$stream_type, _> = $p.parse($into_input.into());
            test_parser!(@ok $stream_type, result, $expected);
        )+
    };

    (@test_errors $stream_type:ty | $p:expr, {
        $($into_input:expr => $expected:expr);+;
    }) => {
        $(
            let input: $stream_type = $into_input.into();
            let result: $crate::ParseResult<$stream_type, _> = $p.parse(input.clone());
            test_parser!(@err $stream_type, result, $expected, input);
        )+
    };

    (@ok $stream_type:ty, $result:expr, $expected:expr) => {
        let (result, stream) = $result;
        let (expected_result, into_expected_stream) = $expected;
        let expected_stream: $stream_type = into_expected_stream.into();
        assert_eq!(result, expected_result);
        assert_eq!(stream, expected_stream);
    };

    (@err $stream_type:ty, $result:expr, $expected_result:expr, $into_expected_stream:expr) => {
        let (result, stream) = $result;
        let result = result.expect_err("assertion failed: expected an Err(_)");
        let expected_result: $crate::error::Errors<$stream_type, <$stream_type as $crate::stream::Stream>::Position> = $expected_result.into();
        let expected_stream: $stream_type = $into_expected_stream.into();
        assert_eq!(result, expected_result);
        assert_eq!(stream, expected_stream);
    };
}
