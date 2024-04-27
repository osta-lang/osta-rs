// pub mod ast;
// pub mod combinators;
// pub mod error;
// pub mod lex;

pub type ParseResult<'a, Out, Err = ()> = Result<(Out, &'a str), Err>;

pub trait Parser<'a, Out, Err = ()>: Sized {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Out, Err>;

    fn map<NewOut, F>(self, map_fn: F) -> MapOutParser<'a, Self, Out, NewOut, Err, F>
    where
        F: Fn(Out) -> NewOut,
    {
        MapOutParser {
            inner: self,
            map_fn,
            marker: std::marker::PhantomData::default(),
        }
    }

    fn map_err<NewErr, F>(self, map_fn: F) -> MapErrParser<'a, Self, Out, Err, NewErr, F>
    where
        F: Fn(Err) -> NewErr,
    {
        MapErrParser {
            inner: self,
            map_fn,
            marker: std::marker::PhantomData::default(),
        }
    }

    fn and_then<P, F, NewOut, NewErr>(
        self,
        and_then_fn: F,
    ) -> AndThen<'a, Self, P, F, Out, NewOut, Err, NewErr>
    where
        P: Parser<'a, NewOut, NewErr>,
        F: Fn(Out) -> P,
    {
        AndThen {
            inner: self,
            and_then_fn,
            marker: std::marker::PhantomData::default(),
        }
    }
}

impl<'a, F, Out, Err> Parser<'a, Out, Err> for F
where
    F: Fn(&'a str) -> ParseResult<'a, Out, Err> + Sized,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Out, Err> {
        self(input)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LiteralError<'a> {
    pub expected: &'a str,
    pub found: &'a str,
}

#[derive(Clone, Copy)]
pub struct LiteralParser<'a> {
    to_match: &'a str,
}

impl<'a> Parser<'a, &'a str, LiteralError<'a>> for LiteralParser<'a> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, &'a str, LiteralError<'a>> {
        if let Some(rest) = input.strip_prefix(self.to_match) {
            Ok((self.to_match, rest))
        } else {
            Err(LiteralError {
                expected: self.to_match,
                found: input,
            })
        }
    }
}

pub fn literal<'a>(to_match: &'a str) -> LiteralParser<'a> {
    LiteralParser { to_match }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}

#[derive(Clone, Copy)]
pub struct PairParser<P1, P2> {
    left: P1,
    right: P2,
}

impl<'a, P1, P2, Out1, Out2, Err1, Err2> Parser<'a, (Out1, Out2), Either<Err1, Err2>>
    for PairParser<P1, P2>
where
    P1: Parser<'a, Out1, Err1>,
    P2: Parser<'a, Out2, Err2>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, (Out1, Out2), Either<Err1, Err2>> {
        match self.left.parse(input) {
            Ok((left_result, rest)) => match self.right.parse(rest) {
                Ok((right_result, rest)) => Ok(((left_result, right_result), rest)),
                Err(err) => Err(Either::Right(err)),
            },
            Err(err) => Err(Either::Left(err)),
        }
    }
}

pub fn pair<'a, P1, P2, Out1, Out2, Err1, Err2>(left: P1, right: P2) -> PairParser<P1, P2>
where
    P1: Parser<'a, Out1, Err1>,
    P2: Parser<'a, Out2, Err2>,
{
    PairParser { left, right }
}

#[derive(Clone, Copy)]
pub struct EitherParser<P1, P2> {
    left: P1,
    right: P2,
}

impl<'a, P1, P2, Out1, Out2, Err1, Err2> Parser<'a, Either<Out1, Out2>, (Err1, Err2)>
    for EitherParser<P1, P2>
where
    P1: Parser<'a, Out1, Err1>,
    P2: Parser<'a, Out2, Err2>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Either<Out1, Out2>, (Err1, Err2)> {
        match self.left.parse(input) {
            Ok((left_result, rest)) => Ok((Either::Left(left_result), rest)),
            Err(err1) => match self.right.parse(input) {
                Ok((right_result, rest)) => Ok((Either::Right(right_result), rest)),
                Err(err2) => Err((err1, err2)),
            },
        }
    }
}

pub fn either<'a, P1, P2, Out1, Out2, Err1, Err2>(left: P1, right: P2) -> EitherParser<P1, P2>
where
    P1: Parser<'a, Out1, Err1>,
    P2: Parser<'a, Out2, Err2>,
{
    EitherParser { left, right }
}

#[derive(Clone, Copy)]
pub struct MapOutParser<'a, P, Out, NewOut, Err, F>
where
    P: Parser<'a, Out, Err> + 'a,
    F: Fn(Out) -> NewOut,
{
    inner: P,
    // maybe try with fn() -> since the closure doesn't need to capture
    map_fn: F,
    marker: std::marker::PhantomData<&'a (Out, NewOut, Err)>,
}

impl<'a, P, F, Out, NewOut, Err> Parser<'a, NewOut, Err>
    for MapOutParser<'a, P, Out, NewOut, Err, F>
where
    P: Parser<'a, Out, Err> + 'a,
    F: Fn(Out) -> NewOut,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, NewOut, Err> {
        self.inner
            .parse(input)
            .map(|(out, rest)| ((self.map_fn)(out), rest))
    }
}

#[derive(Clone, Copy)]
pub struct MapErrParser<'a, P, Out, Err, NewErr, F>
where
    P: Parser<'a, Out, Err> + 'a,
    F: Fn(Err) -> NewErr,
{
    inner: P,
    map_fn: F,
    marker: std::marker::PhantomData<&'a (Out, Err, NewErr)>,
}

impl<'a, P, F, Out, Err, NewErr> Parser<'a, Out, NewErr>
    for MapErrParser<'a, P, Out, Err, NewErr, F>
where
    P: Parser<'a, Out, Err> + 'a,
    F: Fn(Err) -> NewErr,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Out, NewErr> {
        self.inner.parse(input).map_err(|err| (self.map_fn)(err))
    }
}

#[derive(Clone, Copy)]
pub struct AndThen<'a, P, NewP, F, Out, NewOut, Err, NewErr>
where
    NewP: Parser<'a, NewOut, NewErr>,
    P: Parser<'a, Out, Err>,
    F: Fn(Out) -> NewP,
{
    inner: P,
    and_then_fn: F,
    marker: std::marker::PhantomData<&'a (NewP, Out, NewOut, Err, NewErr)>,
}

impl<'a, P, NewP, F, Out, NewOut, Err, NewErr> Parser<'a, NewOut, Either<Err, NewErr>>
    for AndThen<'a, P, NewP, F, Out, NewOut, Err, NewErr>
where
    NewP: Parser<'a, NewOut, NewErr>,
    P: Parser<'a, Out, Err>,
    F: Fn(Out) -> NewP,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, NewOut, Either<Err, NewErr>> {
        match self.inner.parse(input) {
            Ok((result, rest)) => (self.and_then_fn)(result)
                .parse(rest)
                .map_err(Either::Right),
            Err(err) => Err(Either::Left(err)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works<'a>() {
        assert_eq!(
            pair(literal("foo"), literal("bar")).parse("foobar"),
            Ok((("foo", "bar"), ""))
        );

        let p = either(literal("foo"), literal("bar"));
        assert_eq!(p.parse("foo"), Ok((Either::Left("foo"), "")));
        assert_eq!(p.parse("bar"), Ok((Either::Right("bar"), "")));

        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        struct Foo;
        let p = p.map(|_| Foo);
        assert_eq!(p.parse("foo"), Ok((Foo, "")));

        assert_eq!(
            (move |input| { literal("foo").parse(input) }).parse("foo"),
            Ok(("foo", ""))
        );

        assert_eq!(
            osta_proc_macros::sequence!(p, p, p).parse("foobarfoo"),
            Ok(((Foo, Foo, Foo), ""))
        );

        assert_eq!(
            literal("foo")
                .and_then(|s| literal(s))
                .map(|_| Foo)
                .parse("foofoo"),
            Ok((Foo, ""))
        )
    }
}
