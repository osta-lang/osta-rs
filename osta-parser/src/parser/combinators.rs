//! This module contains the fundamental combinators used through all the parsing pipeline
#![allow(dead_code)]

use regex::{Captures, Regex};
use osta_proc_macros::impl_either_unwrap;

use super::*;

// =============================================================================
// Base combinators
// =============================================================================

#[derive(Debug, PartialEq)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}

impl_either_unwrap!(3);

pub fn either<'a, Out1, Out2, Err1, Err2>(
    first: impl Parser<'a, Out1, Err1>,
    second: impl Parser<'a, Out2, Err2>,
) -> impl Parser<'a, Either<Out1, Out2>, (Err1, Err2)> {
    move |input| match first.parse(input) {
        Ok((result1, rest1)) => Ok((Either::Left(result1), rest1)),
        Err(err1) => match second.parse(input) {
            Ok((result2, rest2)) => Ok((Either::Right(result2), rest2)),
            Err(err2) => Err((err1, err2)),
        },
    }
}

pub fn pair<'a, Out1, Out2, Err1, Err2>(
    first: impl Parser<'a, Out1, Err1>,
    second: impl Parser<'a, Out2, Err2>,
) -> impl Parser<'a, (Out1, Out2), Either<Err1, Err2>> {
    move |input| match first.parse(input) {
        Ok((first_result, rest)) => match second.parse(rest) {
            Ok((second_result, rest)) => Ok(((first_result, second_result), rest)),
            Err(err) => Err(Either::Right(err)),
        },
        Err(err) => Err(Either::Left(err)),
    }
}

pub fn first<'a, Out1, Out2, Err>(
    parser: impl Parser<'a, (Out1, Out2), Err>,
) -> impl Parser<'a, Out1, Err> {
    move |input| parser.parse(input).map(|((result, _), rest)| (result, rest))
}

pub fn second<'a, Out1, Out2, Err>(
    parser: impl Parser<'a, (Out1, Out2), Err>,
) -> impl Parser<'a, Out2, Err> {
    move |input| parser.parse(input).map(|((_, result), rest)| (result, rest))
}

pub fn map<'a, In, Out, InErr, OutErr>(
    parser: impl Parser<'a, In, InErr>,
    out_f: impl Fn(In) -> Out,
    err_f: impl Fn(InErr) -> OutErr,
) -> impl Parser<'a, Out, OutErr> {
    move |input| match parser.parse(input) {
        Ok((result, rest)) => Ok((out_f(result), rest)),
        Err(err) => Err(err_f(err)),
    }
}

pub fn map_out<'a, In, Out, Err>(
    parser: impl Parser<'a, In, Err>,
    f: impl Fn(In) -> Out,
) -> impl Parser<'a, Out, Err> {
    move |input| parser.parse(input).map(|(result, rest)| (f(result), rest))
}

pub fn map_err<'a, Out, InErr, OutErr>(
    parser: impl Parser<'a, Out, InErr>,
    f: impl Fn(InErr) -> OutErr,
) -> impl Parser<'a, Out, OutErr> {
    move |input| parser.parse(input).map_err(|err| f(err))
}

pub fn and_then<'a, P, In, Out, InErr, OutErr>(
    parser: impl Parser<'a, In, InErr>,
    f: impl Fn(In) -> P,
) -> impl Parser<'a, Out, Either<InErr, OutErr>>
where
    P: Parser<'a, Out, OutErr>,
{
    move |input| match parser.parse(input) {
        Ok((result, rest)) => f(result).parse(rest).map_err(Either::Right),
        Err(err) => Err(Either::Left(err)),
    }
}

pub fn some<'a, Out: 'a, Err: 'a>(
    parser: impl Parser<'a, Out, Err> + 'a,
) -> impl Parser<'a, Vec<Out>, Err> {
    let parser_rc = std::rc::Rc::new(parser);
    boxed(map(
        pair(
            parser_rc.clone(),
            optional(either(parser_rc.clone(), defer(move || some(parser_rc.clone())))),
        ),
        |(first, second)| match second {
            Some(either) => match either {
                Either::Left(second) => vec![first, second],
                Either::Right(second) => {
                    let mut vec = vec![first];
                    vec.extend(second);
                    vec
                }
            },
            None => vec![first],
        }, |err| match err {
            Either::Left(err) => err,
            Either::Right(_) => unreachable!(),
        }
    ))
}

pub fn many<'a, Out: 'a, Err: 'a>(
    parser: impl Parser<'a, Out, Err> + 'a,
) -> impl Parser<'a, Vec<Out>, Err> {
    map_out(optional(some(parser)), |opt| opt.unwrap_or_default())
}

// =============================================================================
// Utility combinators
// =============================================================================

pub fn boxed<'a, Out, Err>(
    parser: impl Parser<'a, Out, Err> + 'a,
) -> BoxedParser<'a, Out, Err> {
    BoxedParser::from(Box::new(parser))
}

pub fn defer<'a, Out, Err, P: Parser<'a, Out, Err> + Sized>(
    factory: impl Fn() -> P,
) -> impl Parser<'a, Out, Err> {
    move |input| factory().parse(input)
}

pub fn optional<'a, Out, Err>(
    parser: impl Parser<'a, Out, Err>,
) -> impl Parser<'a, Option<Out>, Err> {
    move |input| match parser.parse(input) {
        Ok((result, rest)) => Ok((Some(result), rest)),
        Err(_) => Ok((None, input)),
    }
}

pub fn skip_whitespace<'a, Out, Err>(
    parser: impl Parser<'a, Out, Err>,
) -> impl Parser<'a, Out, Err> {
    move |mut input: &'a str| {
        while let Some(rest) = input.strip_prefix(|c: char| c.is_whitespace() || c.is_control()) {
            input = rest;
        }
        let (result, rest) = parser.parse(input)?;

        Ok((result, rest))
    }
}

// =============================================================================
// Basic parsers
// =============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LiteralError<'a> {
    pub expected: &'static str,
    pub found: &'a str,
}

pub fn literal<'a>(literal: &'static str) -> impl Parser<'a, &'static str, LiteralError<'a>> {
    move |input: &'a str| {
        if let Some(rest) = input.strip_prefix(literal) {
            Ok((literal, rest))
        } else {
            Err(LiteralError {
                expected: literal,
                found: input,
            })
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RegexError<'a> {
    pub re: &'static str,
    pub found: &'a str,
}

pub fn regex<'a>(re_str: &'static str) -> impl Parser<'a, Captures<'a>, RegexError<'a>> {
    let re = Regex::new(re_str).unwrap();
    move |input: &'a str| match re.captures(input) {
        Some(captures) => {
            let match_length = captures.get(0).unwrap().end();
            Ok((captures, &input[match_length..]))
        }
        None => Err(RegexError {
            re: re_str,
            found: input,
        }),
    }
}

#[cfg(test)]
mod tests {
    use osta_proc_macros::sequence;
    use super::*;

    #[test]
    fn test_either_unwrap() {
        let either: Either<i32, Either<i32, i32>> = Either::Right(Either::Left(1));
        assert_eq!(either.unwrap(), 1);
    }

    #[test]
    fn test_either() {
        let parser = either(literal("foo"), literal("bar"));
        assert_eq!(parser.parse("foo"), Ok((Either::Left("foo"), "")));
        assert_eq!(parser.parse("bar"), Ok((Either::Right("bar"), "")));
        assert_eq!(
            parser.parse("baz"),
            Err((
                LiteralError {
                    found: "baz",
                    expected: "foo"
                },
                LiteralError {
                    found: "baz",
                    expected: "bar"
                }
            ))
        );
    }

    #[test]
    fn test_pair() {
        let parser = pair(literal("foo"), literal("bar"));
        assert_eq!(parser.parse("foobar"), Ok((("foo", "bar"), "")));
        assert_eq!(parser.parse("foobarbaz"), Ok((("foo", "bar"), "baz")));
        assert_eq!(
            parser.parse("foo"),
            Err(Either::Right(LiteralError {
                found: "",
                expected: "bar"
            }))
        );
        assert_eq!(
            parser.parse("bar"),
            Err(Either::Left(LiteralError {
                found: "bar",
                expected: "foo"
            }))
        );
    }

    #[test]
    fn test_first() {
        let parser = first(pair(literal("foo"), literal("bar")));
        assert_eq!(parser.parse("foobar"), Ok(("foo", "")));
        assert_eq!(parser.parse("foobarbaz"), Ok(("foo", "baz")));
        assert_eq!(
            parser.parse("foo"),
            Err(Either::Right(LiteralError {
                found: "",
                expected: "bar"
            }))
        );
        assert_eq!(
            parser.parse("bar"),
            Err(Either::Left(LiteralError {
                found: "bar",
                expected: "foo"
            }))
        );
    }

    #[test]
    fn test_second() {
        let parser = second(pair(literal("foo"), literal("bar")));
        assert_eq!(parser.parse("foobar"), Ok(("bar", "")));
        assert_eq!(parser.parse("foobarbaz"), Ok(("bar", "baz")));
        assert_eq!(
            parser.parse("foo"),
            Err(Either::Right(LiteralError {
                found: "",
                expected: "bar"
            }))
        );
        assert_eq!(
            parser.parse("bar"),
            Err(Either::Left(LiteralError {
                found: "bar",
                expected: "foo"
            }))
        );
    }

    #[test]
    fn test_map_out() {
        let parser = map_out(literal("foo"), |_| 1);
        assert_eq!(parser.parse("foo"), Ok((1, "")));
        assert_eq!(
            parser.parse("bar"),
            Err(LiteralError {
                found: "bar",
                expected: "foo"
            })
        );
    }

    #[test]
    fn test_map_err() {
        let parser = map_err(literal("foo"), |_| 1);
        assert_eq!(parser.parse("foo"), Ok(("foo", "")));
        assert_eq!(parser.parse("bar"), Err(1));
    }

    #[test]
    fn test_and_then() {
        let parser = and_then(literal("foo"), |_| literal("bar"));
        assert_eq!(parser.parse("foobar"), Ok(("bar", "")));
        assert_eq!(
            parser.parse("foo"),
            Err(Either::Right(LiteralError {
                found: "",
                expected: "bar"
            }))
        );
        assert_eq!(
            parser.parse("bar"),
            Err(Either::Left(LiteralError {
                found: "bar",
                expected: "foo"
            }))
        );
    }

    #[test]
    fn test_defer() {
        let parser = defer(|| literal("foo"));
        assert_eq!(parser.parse("foo"), Ok(("foo", "")));
    }

    #[test]
    fn test_optional() {
        let parser = optional(literal("foo"));
        assert_eq!(parser.parse("foo"), Ok((Some("foo"), "")));
        assert_eq!(parser.parse("bar"), Ok((None, "bar")));
    }

    #[test]
    fn test_skip_whitespace() {
        assert_eq!(
            skip_whitespace(literal("foo")).parse(" \x0a \t\r\nfoobar"),
            Ok(("foo", "bar"))
        );
    }

    #[test]
    fn test_sequence<'a>() {
        let parser = sequence!(literal("foo"), literal("bar"), literal("baz"));
        assert_eq!(parser.parse("foobarbaz"), Ok((("foo", "bar", "baz"), "")));
        assert_eq!(parser.parse("barbaz"), Err(Either::Left(LiteralError { expected: "foo", found: "barbaz" })));
        assert_eq!(parser.parse("foobaz"), Err(Either::Right(Either::Left(LiteralError { expected: "bar", found: "baz" }))));
        assert_eq!(parser.parse("foobar"), Err(Either::Right(Either::Right(LiteralError { expected: "baz", found: "" }))));
    }
}
