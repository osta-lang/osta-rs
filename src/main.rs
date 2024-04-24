use std::cmp::min;
use std::fmt::{Debug, Display, Formatter};
use regex::{Captures, Regex};


#[derive(Debug, PartialEq)]
enum ParserError<'a> {
    LiteralError(&'static str, &'a str),
    RegexError(&'static str, &'a str),
    SomeError(Vec<ParserError<'a>>, Vec<ParserError<'a>>),
}

type ParserResult<'a, O> = Result<(O, &'a str), Vec<ParserError<'a>>>;

trait Parser<'a, O> {
    fn parse(&self, input: &'a str) -> ParserResult<'a, O>;
}

impl<'a, F, O> Parser<'a, O> for F
where
    F: Fn(&'a str) -> ParserResult<'a, O>,
{
    fn parse(&self, input: &'a str) -> ParserResult<'a, O> {
        self(input)
    }
}

fn pair<'a, O1, O2>(first: impl Parser<'a, O1>, second: impl Parser<'a, O2>) -> impl Parser<'a, (O1, O2)> {
    move |input: &'a str| {
        match first.parse(input) {
            Ok((first_result, rest)) => {
                match second.parse(rest) {
                    Ok((second_result, rest)) => {
                        Ok(((first_result, second_result), rest))
                    }
                    Err(errors) => Err(errors),
                }
            }
            Err(errors) => Err(errors),
        }
    }
}

fn some<'a, O>(first: impl Parser<'a, O>, second: impl Parser<'a, O>) -> impl Parser<'a, O> {
    move |input: &'a str| {
        match first.parse(input) {
            Ok((result, rest)) => Ok((result, rest)),
            Err(first_errors) => {
                match second.parse(input) {
                    Ok((result, rest)) => Ok((result, rest)),
                    Err(second_errors) => {
                        Err(vec![ParserError::SomeError(first_errors, second_errors)])
                    }
                }
            },
        }
    }
}

fn map<'a, I, O>(parser: impl Parser<'a, I>, f: impl Fn(I) -> O) -> impl Parser<'a, O> {
    move |input: &'a str| {
        match parser.parse(input) {
            Ok((result, rest)) => Ok((f(result), rest)),
            Err(errors) => Err(errors),
        }
    }
}

fn literal<'a>(literal: &'static str) -> impl Parser<'a, &'static str> {
    move |input: &'a str| {
        if input.starts_with(literal) {
            Ok((literal, &input[literal.len()..]))
        } else {
            Err(vec![ParserError::LiteralError(literal, &input[0..min(literal.len(), input.len())])])
        }
    }
}

fn regex<'a>(re_str: &'static str) -> impl Parser<'a, Captures<'a>> {
    let re = Regex::new(re_str).unwrap();
    move |input: &'a str| {
        match re.captures(input) {
            Some(captures) => {
                let match_length = captures.get(0).unwrap().end();
                Ok((captures, &input[match_length..]))
            },
            None => {
                let next_line = input.find(&['\n', '\r']).unwrap_or(min(10, input.len()));
                Err(vec![ParserError::RegexError(re_str, &input[0..next_line])])
            },
        }
    }
}

fn execute<'a, O: Debug>(parser: impl Parser<'a, O>, input: &'a str) {
    match parser.parse(input) {
        Ok((result, rest)) => {
            println!("{:?} {:?}", result, rest);
        }
        Err(errors) => {
            for error in errors {
                println!("{:?}", error);
            }
        }
    }
}

fn main() {
    let mut file = std::fs::File::open("main.osta").unwrap();
    let mmap = unsafe { memmap2::Mmap::map(&file).unwrap() };
    let input = unsafe { std::str::from_utf8_unchecked(&mmap) };

    execute(pair(literal("hello"), pair(regex(r"\s+"), literal("world"))), input);
}

#[cfg(test)]
mod tests {
    use crate::ParserError::SomeError;
    use super::*;

    #[test]
    fn test_pair() {
        let parser = pair(literal("foo"), literal("bar"));
        assert_eq!(parser.parse("foobar"), Ok((("foo", "bar"), "")));
        assert_eq!(parser.parse("foobarbaz"), Ok((("foo", "bar"), "baz")));
        assert_eq!(parser.parse("foo"), Err(vec![ParserError::LiteralError("bar", "")]));
        assert_eq!(parser.parse("bar"), Err(vec![ParserError::LiteralError("foo", "bar")]));
    }

    #[test]
    fn test_some() {
        let parser = some(literal("foo"), literal("bar"));
        assert_eq!(parser.parse("foo"), Ok(("foo", "")));
        assert_eq!(parser.parse("bar"), Ok(("bar", "")));
        assert_eq!(parser.parse("baz"), Err(vec![SomeError(vec![ParserError::LiteralError("foo", "baz")], vec![ParserError::LiteralError("bar", "baz")])]));
    }

    #[test]
    fn test_map() {
        let parser = map(literal("foo"), |s| s.len());
        assert_eq!(parser.parse("foo"), Ok((3, "")));
        assert_eq!(parser.parse("bar"), Err(vec![ParserError::LiteralError("foo", "bar")]));
    }
}
