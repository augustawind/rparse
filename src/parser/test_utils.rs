#[macro_export]
macro_rules! test_parser {
    // New API

    (for<$stream_ty:ty, $output_ty:ty> | $p:expr, {
        $($into_input:expr => $assertion:ident $expected:expr),+ $(,)*
    }) => {
        $(
            test_parser!(@dispatch $assertion <$stream_ty, $output_ty> $p, $into_input => $expected);
        )+
    };

    (@dispatch ok <$stream_ty:ty, $output_ty:ty> $p:expr, $into_input:expr => $expected:expr) => {
        let input: $stream_ty = $into_input.into();
        let (result, stream): $crate::ParseResult<$stream_ty, $output_ty> = $p.parse(input.clone());
        let (expected_result, into_expected_stream) = $expected;
        let expected_stream: $stream_ty = into_expected_stream.into();
        assert_eq!(result, expected_result);
        assert_eq!(stream, expected_stream);
    };

    (@dispatch err <$stream_ty:ty, $output_ty:ty> $p:expr, $into_input:expr => $expected:expr) => {
        let input: $stream_ty = $into_input.into();
        let (result, stream): $crate::ParseResult<$stream_ty, $output_ty> = $p.parse(input.clone());
        let result = result.expect_err("assertion failed: expected an Err(_)");
        let expected_result: $crate::error::Errors<$stream_ty> = $expected.into();
        let expected_stream = input;
        assert_eq!(result, expected_result);
        assert_eq!(stream, expected_stream);
    };

    // Old API

    ($stream_ty:ty | $p:expr, {
        $($stream:expr => $expected:expr);+;
    }, {
        $($stream_err:expr => $expected_err:expr);+;
    }) => {
        test_parser!($stream_ty | $p, {
            $($stream => $expected);+;
        });
        test_parser!(@test_errors $stream_ty | $p, {
            $($stream_err => $expected_err);+;
        });
    };

    ($stream_ty:ty | $p:expr, {
        $($into_input:expr => $expected:expr);+;
    }) => {
        $(
            let result: $crate::ParseResult<$stream_ty, _> = $p.parse($into_input.into());
            test_parser!(@ok $stream_ty, result, $expected);
        )+
    };

    (@test_errors $stream_ty:ty | $p:expr, {
        $($into_input:expr => $expected:expr);+;
    }) => {
        $(
            let input: $stream_ty = $into_input.into();
            let result: $crate::ParseResult<$stream_ty, _> = $p.parse(input.clone());
            test_parser!(@err $stream_ty, result, $expected, input);
        )+
    };

    (@ok $stream_ty:ty, $result:expr, $expected:expr) => {
        let (result, stream) = $result;
        let (expected_result, into_expected_stream) = $expected;
        let expected_stream: $stream_ty = into_expected_stream.into();
        assert_eq!(result, expected_result);
        assert_eq!(stream, expected_stream);
    };

    (@err $stream_ty:ty, $result:expr, $expected_result:expr, $into_expected_stream:expr) => {
        let (result, stream) = $result;
        let result = result.expect_err("assertion failed: expected an Err(_)");
        let expected_result: $crate::error::Errors<$stream_ty> = $expected_result.into();
        let expected_stream: $stream_ty = $into_expected_stream.into();
        assert_eq!(result, expected_result);
        assert_eq!(stream, expected_stream);
    };
}
