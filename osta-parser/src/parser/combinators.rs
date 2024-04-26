//! This module contains the fundamental combinators used through all the parsing pipeline
#![allow(dead_code)]

use regex::{Captures, Regex};

use super::*;

// =============================================================================
// Base combinators
// =============================================================================

pub fn empty() -> impl Parser<'static, (), ()> {
    move |input| Ok(((), input))
}

#[derive(Debug, PartialEq)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<S> Either<S, S> {
    pub fn unwrap(self) -> S {
        match self {
            Either::Left(v) => v,
            Either::Right(v) => v,
        }
    }
}

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

pub fn map<'a, In, Out, InErr, OutErr>(
    parser: impl Parser<'a, In, InErr>,
    out_f: impl Fn(In) -> Out,
    err_f: impl Fn(InErr) -> OutErr,
) -> impl Parser<'a, Out, OutErr> {
    move |input| match parser.parse(input) {
        Ok((result, rest)) => Ok((out_f(result), rest)),
        Err(error) => Err(err_f(error)),
    }
}

pub fn map_out<'a, In, Out, Err>(
    parser: impl Parser<'a, In, Err>,
    f: impl Fn(In) -> Out,
) -> impl Parser<'a, Out, Err> {
    map(parser, f, |err| err)
}

pub fn map_err<'a, Out, InErr, OutErr>(
    parser: impl Parser<'a, Out, InErr>,
    f: impl Fn(InErr) -> OutErr,
) -> impl Parser<'a, Out, OutErr> {
    map(parser, |out| out, f)
}

// =============================================================================
// Combinators for sequences
// =============================================================================

macro_rules! sequence {
    ($e:expr) => {
        $e
    };
    ($e:expr, $($rest:expr),+) => {
        pair($e, sequence!($($rest),+))
    };
}

// =============================================================================
// Utility combinators
// =============================================================================

pub fn defer<'a, Out, Err, P: Parser<'a, Out, Err> + Sized>(
    factory: impl Fn() -> P,
) -> impl Parser<'a, Out, Err> {
    move |input| factory().parse(input)
}

pub fn optional<'a, Out, Err>(
    parser: impl Parser<'a, Out, Err>,
) -> impl Parser<'a, Option<Out>> {
    move |input| match parser.parse(input) {
        Ok((result, rest)) => Ok((Some(result), rest)),
        Err(_) => Ok((None, input)),
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
    use super::*;

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
    fn test_some() {
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
    fn test_skip_whitespace() {
        assert_eq!(
            skip_whitespace(literal("foo")).parse(" \x0a \t\r\nfoobar"),
            Ok(("foo", "bar"))
        );
    }

    fn test_sequence() {
        let parser = sequence!(literal("foo"));
        assert_eq!(parser.parse("foo"), Ok(("foo", "")));
        let parser = sequence!(literal("foo"), literal("bar"), literal("baz"));
        match parser.parse("foobarbaz") {
            Ok(((a, (b, c)), rest)) => {

            }
            Err(ee) => match ee {
                Either::Left(e) => {
                    println!("Left: {:?}", e);
                }
                Either::Right(ee) => match ee {
                    Either::Left(e) => {
                        println!("Right Left: {:?}", e);
                    }
                    Either::Right(e) => {
                        println!("Right Right: {:?}", e);
                    }
                }
            }
        }
        assert_eq!(parser.parse("foobarbaz"), Ok((("foo", ("bar", "baz")), "")));
        assert_eq!(parser.parse("foobar"), Err(Either::Right(Either::Right(LiteralError { expected: "baz", found: "" }))));
    }
}