use crate::parser::error::ParseError;
use crate::parser::*;

pub fn integer_literal(input: &str) -> ParseResult<'_, i64, ParseError> {
    todo!()
}

// DESIGN(cdecompilador): Should this really a &str?? or a Span?? or an allocated String??
pub fn identifier(input: &str) -> ParseResult<'_, String, ParseError> {
    todo!()
}

// DESIGN(cdecompilador): Should this really a &str?? or a Span?? or an allocated String??
pub fn string_literal(input: &str) -> ParseResult<'_, String, ParseError> {
    todo!()
}
