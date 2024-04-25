#![allow(dead_code)]

use crate::parser::combinators::{Either, map, some};
use crate::parser::error::{LexError, ParseError};
use crate::parser::lex::{identifier, integer};
use crate::parser::Parser;

#[derive(Debug, PartialEq)]
enum Term<'a> {
    Integer(i64),
    Identifier(&'a str),
}

fn term<'a>() -> impl Parser<'a, Term<'a>, (ParseError, ParseError)> {
    map(
        some(integer(), identifier()),
        |either| match either {
            Either::Left(int) => Term::Integer(int),
            Either::Right(ident) => Term::Identifier(ident),
        },
        |(int_err, ident_err)| (
            ParseError::LexError(int_err),
            ParseError::LexError(ident_err),
        ),
    )
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_term() {
        assert_eq!(term().parse("123"), Ok((Term::Integer(123), "")));
        assert_eq!(term().parse("foo"), Ok((Term::Identifier("foo"), "")));
        assert_eq!(term().parse("123foo"), Ok((Term::Integer(123), "foo")));
        assert_eq!(term().parse("foo123"), Ok((Term::Identifier("foo123"), "")));

        assert_eq!(
            term().parse(""),
            Err((
                ParseError::LexError(LexError::ExpectedInteger),
                ParseError::LexError(LexError::ExpectedIdentifier),
            ))
        );
    }
}