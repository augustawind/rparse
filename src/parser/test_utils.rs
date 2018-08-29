macro_rules! test_parser {
    ($stream_type:ty | $parser:expr, {
        $($stream:expr => $expected:tt),+
        $(,)*
    }) => {
        $(
            let result: ParseResult<$stream_type, _> =
                $parser.parse($stream.into());
            assert_parse_result_eq!(result => $expected);
        )+
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

macro_rules! assert_parse_err {
    ($parsed:expr, $stream:expr) => {
        let (parsed, stream) = $parsed;
        assert!(parsed.is_err());
        assert_eq!(stream, $stream);
    };
    ($parsed:expr, $stream:expr, $type:ty | $position:expr) => {
        let (parsed, stream) = $parsed;
        assert!(parsed.is_err());
        assert_eq!(stream, State::<_, $type>::new($stream, $position));
    };
}
