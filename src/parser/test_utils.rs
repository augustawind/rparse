macro_rules! assert_parse {
    (from $input_type:ty | $parsed:expr, { $output:expr => $input:expr }) => {
        let (parsed, input): ParseResult<$input_type, _> = $parsed;
        assert!(parsed.is_ok());
        assert_eq!(parsed.unwrap(), $output);
        let state: $input_type = $input.into();
        assert_eq!(input, state);
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
