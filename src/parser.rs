pub mod ast;
pub mod combinators;
pub mod cst;
pub mod error;
pub mod lex;

pub type ParseResult<'a, Out, Err = Vec<error::ParseError>> = Result<(Out, &'a str), Err>;

pub trait Parser<'a, Out, Err = Vec<error::ParseError>> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Out, Err>;
}

impl<'a, F, Out, Err> Parser<'a, Out, Err> for F
where
    F: Fn(&'a str) -> ParseResult<'a, Out, Err>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Out, Err> {
        self(input)
    }
}
