macro_rules! assert_parse_ok {
    (
        $parsed:expr, { output => $output:expr,stream => $input:expr,position => $pos_type:ty | $pos:expr, }
    ) => {
        let (parsed, input) = $parsed;
        assert!(parsed.is_ok());
        assert_eq!(parsed.unwrap(), $output);
        assert_eq!(input, State::<_, $pos_type>::new($input, $pos));
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
