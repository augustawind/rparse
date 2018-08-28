#[cfg_attr(rustfmt, rustfmt_skip)]
macro_rules! assert_parse {
    (from $input_type:ty | $parser:expr, {
        $input:expr => $output:expr
    }) => {
        let input: $input_type = $input.into();
        let (parsed, rest): ParseResult<$input_type, _> = $parser.parse(input);
        assert!(parsed.is_ok());

        let (output, into_rest) = $output;
        assert_eq!(parsed.unwrap(), output);
        assert_eq!(rest, into_rest);
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
