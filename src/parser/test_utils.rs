macro_rules! test_parser {
    ($input_type:ty | $parser:expr, {
        $($input:expr => $expected:expr),+
        $(,)*
    }) => {
        $(
            let (parsed_result, parsed_input): ParseResult<$input_type, _> = $parser.parse($input.into());
            let (result, input) = $expected;
            assert_eq!(parsed_result, result);
            assert_eq!(parsed_input, input);
        )+
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
