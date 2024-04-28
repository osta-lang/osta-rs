use osta_func::monads::fallible_state_monad::FallibleStateMonad;

pub struct LiteralError<'a> {
    pub expected: &'a str,
    pub found: &'a str,
}

pub fn literal<'a>(expected: &'a str) -> impl FallibleStateMonad<'a, &'a str, &'a str, LiteralError<'a>> {
    move |input: &'a str| {
        if let Some(rest) = input.strip_prefix(expected) {
            (Ok(expected), rest)
        } else {
            (Err(LiteralError {
                expected,
                found: input
            }), input)
        }
    }
}

lazy_static::lazy_static! {
    static ref INT_RE: regex::Regex = regex::Regex::new("[0-9]*").unwrap();
    static ref STRING_RE: regex::Regex = regex::Regex::new("[_a-zA-Z][_a-zA-Z0-9]*").unwrap();
}


pub fn skip_whitespace<'a, Out: 'a, Err: 'a>(
    parser: impl FallibleStateMonad<'a, &'a str, Out, Err>,
) -> impl FallibleStateMonad<'a, &'a str, Out, Err> {
    move |mut input: &'a str| {
        while let Some(rest) = input.strip_prefix(|c: char| c.is_whitespace() || c.is_control()) {
            input = rest;
        }
        parser.apply(input)
    }
}

