macro_rules! test_parser {
    ($input_type:ty | $parser:expr, {
        $($input:expr => $expected:tt),+
        $(,)*
    }) => {
        $(
            let result: ParseResult<$input_type, _> =
                $parser.parse($input.into());
            assert_parse_result_eq!(result => $expected);
        )+
    };
}

macro_rules! assert_parse_result_eq {
    ($result:expr => ($expected_result:expr, $expected_input:expr, $expected_pos:expr)) => {
        let (parsed_result, parsed_input) = $result;
        assert_eq!(parsed_result, $expected_result);
        let expected_input = $crate::input::State::new($expected_input, $expected_pos);
        assert_eq!(parsed_input, expected_input);
    };
    ($result:expr => ($expected_result:expr, $expected_input:expr)) => {
        let (parsed_result, parsed_input) = $result;
        assert_eq!(parsed_result, $expected_result);
        assert_eq!(parsed_input, $expected_input);
    };
}

macro_rules! assert_parse_err {
    ($parsed:expr, $input:expr) => {
        let (parsed, input) = $parsed;
        assert!(parsed.is_err());
        assert_eq!(input, $input);
    };
    ($parsed:expr, $input:expr, $type:ty | $position:expr) => {
        let (parsed, input) = $parsed;
        assert!(parsed.is_err());
        assert_eq!(input, State::<_, $type>::new($input, $position));
    };
}
