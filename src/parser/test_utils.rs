#[cfg(test)]
use {ParseResult, Stream};

#[cfg(test)]
pub fn ok_result<S, O, I>(output: O, stream: I) -> ParseResult<S, O>
where
    S: Stream,
    I: Into<S>,
{
    Ok((Some(output), stream.into()))
}

#[cfg(test)]
pub fn none_result<S, O, I>(stream: I) -> ParseResult<S, O>
where
    S: Stream,
    I: Into<S>,
{
    Ok((None, stream.into()))
}

#[macro_export]
macro_rules! test_parser {
    // New API

    ($stream_ty:ty => $output_ty:ty | $p:expr, {
        $($into_input:expr => $assertion:ident $expected:expr),+ $(,)*
    }) => {
        $(
            test_parser!(@dispatch $assertion $stream_ty, $output_ty, $p, $into_input, $expected);
        )+
    };

    (@dispatch ok $stream_ty:ty, $output_ty:ty, $p:expr, $into_input:expr, $expected:expr) => {
        let input: $stream_ty = $into_input.into();
        let (result, stream): (Option<$output_ty>, $stream_ty) = $p.parse(input.clone()).unwrap();
        let (expected_result, into_expected_stream): ($output_ty, _) = $expected;
        let expected_stream: $stream_ty = into_expected_stream.into();
        assert_eq!(result.unwrap(), expected_result);
        assert_eq!(stream, expected_stream);
    };

    (@dispatch noop $stream_ty:ty, $output_ty:ty, $p:expr, $into_input:expr, $expected:expr) => {
        let input: $stream_ty = $into_input.into();
        let (result, stream): (Option<$output_ty>, $stream_ty) = $p.parse(input.clone()).unwrap();
        assert_eq!(result, None);
        assert_eq!(stream, input);
    };

    (@dispatch err $stream_ty:ty, $output_ty:ty, $p:expr, $into_input:expr, $expected:expr) => {
        let input: $stream_ty = $into_input.into();
        let (result, stream): ($crate::error::Error<$stream_ty>, $stream_ty) = $p.parse(input.clone()).unwrap_err();
        let expected_stream = input;
        assert_eq!(result, $expected);
        assert_eq!(stream, expected_stream);
    };
}
