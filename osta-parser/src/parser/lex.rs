use crate::parser::error::LexError;
use crate::parser::*;
use crate::parser::combinators::{map, regex};

#[allow(dead_code)]
pub fn integer<'a>() -> impl Parser<'a, i64, LexError> {
    map(
        regex(r"^[0-9]+"),
        |captures| captures.get(0).unwrap().as_str().parse().unwrap(),
        |_| LexError::ExpectedInteger,
    )
}

#[allow(dead_code)]
pub fn identifier<'a>() -> impl Parser<'a, &'a str, LexError> {
    map(
        regex(r"^[a-zA-Z_][a-zA-Z0-9_]*"),
        |captures| captures.get(0).unwrap().as_str(),
        |_| LexError::ExpectedIdentifier,
    )
}
