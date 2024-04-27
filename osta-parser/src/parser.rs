pub mod ast;
pub mod combinators;
pub mod error;
pub mod lex;

pub type ParseResult<'a, Out, Err = ()> = Result<(Out, &'a str), Err>;

pub trait Parser<'a, Out, Err = ()> {
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

impl<'a, P, Out, Err> Parser<'a, Out, Err> for std::rc::Rc<P>
where
    P: Parser<'a, Out, Err>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Out, Err> {
        self.as_ref().parse(input)
    }
}

pub struct BoxedParser<'a, Out, Err = ()>(Box<dyn Parser<'a, Out, Err> + 'a>);

impl<'a, Out, Err> Parser<'a, Out, Err> for BoxedParser<'a, Out, Err> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Out, Err> {
        self.0.parse(input)
    }
}

impl<'a, P, Out, Err> From<Box<P>> for BoxedParser<'a, Out, Err>
where
    P: Parser<'a, Out, Err> + 'a,
{
    fn from(parser: Box<P>) -> Self {
        Self(parser)
    }
}
