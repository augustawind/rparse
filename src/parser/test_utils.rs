macro_rules! test_parser {
    (from $input_type:ty | $parser:expr, {
        $($input:expr => $result:expr),+
        $(,)*
    }) => {
        $(
            let (parsed, input): ParseResult<$input_type, _> = $parser.parse($input.into());
            assert!(parsed.is_ok());

            let (output, rest) = $result;
            assert_eq!(parsed.unwrap(), output);
            assert_eq!(input, rest);
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
