use crate::error::*;
use crate::token::*;

use osta_func::FallibleStateMonad;

lazy_static::lazy_static! {
    static ref RE_INT: regex::Regex = regex::Regex::new("^[0-9]+").unwrap();
    static ref RE_IDENTIFIER: regex::Regex = regex::Regex::new("^[_a-zA-Z][_a-zA-Z0-9]*").unwrap();
}

pub fn token<'a>(
    expected: &'a str,
    token_kind: TokenKind,
) -> impl FallibleStateMonad<'a, &'a str, Token<'a>, TokenizerError<'a>> {
    move |input: &'a str| {
        if let Some(rest) = input.strip_prefix(expected) {
            (
                Ok(Token {
                    lexeme: expected,
                    kind: token_kind,
                }),
                rest,
            )
        } else {
            (
                Err(TokenizerError {
                    kind: TokenizerErrorKind::ExpectedLiteral(expected),
                    found: input,
                }),
                input,
            )
        }
    }
}

pub fn regex<'a>(
    regex: &'a regex::Regex,
    token_kind: TokenKind,
) -> impl FallibleStateMonad<'a, &'a str, Token<'a>, TokenizerError<'a>> {
    move |input: &'a str| {
        if let Some(matched) = RE_INT.find(input){
            (Ok(Token {
                lexeme: &input[matched.start()..matched.end()],
                kind: token_kind,
            }), &input[matched.end()..])
        } else {
            (Err(TokenizerError {
                found: input,
                kind: TokenizerErrorKind::ExpectedRegex(regex.as_str())
            }), input)
        }
    }
}

pub fn keyword(
    expected: &str,
    token_kind: TokenKind
) -> impl FallibleStateMonad<&str, Token, TokenizerError> {
    token(expected, token_kind).map_err(|err| TokenizerError {
        found: err.found,
        kind: TokenizerErrorKind::ExpectedKeyword(expected)
    })
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

pub fn integer<'a>() -> impl FallibleStateMonad<'a, &'a str, Token<'a>, TokenizerError<'a>> {
    regex(&RE_INT, TokenKind::Int)
}

pub fn identifier<'a>() -> impl FallibleStateMonad<'a, &'a str, Token<'a>, TokenizerError<'a>> {
    regex(&RE_IDENTIFIER, TokenKind::Identifier)
}

#[cfg(test)]
mod tests {
    use super::*;
    use osta_func::StateMonad;

    #[test]
    fn token() {
        assert_eq!(
            super::token("foo", TokenKind::Test).apply("foo"),
            (
                Ok(Token {
                    lexeme: "foo",
                    kind: TokenKind::Test
                }),
                ""
            )
        );
        assert_eq!(
            super::token("foo", TokenKind::Test).apply("foobar"),
            (
                Ok(Token {
                    lexeme: "foo",
                    kind: TokenKind::Test
                }),
                "bar"
            )
        );
        assert_eq!(
            super::token("bar", TokenKind::Test).apply("foo"),
            (
                Err(TokenizerError {
                    found: "foo",
                    kind: TokenizerErrorKind::ExpectedLiteral("bar")
                }),
                "foo"
            )
        );
    }

    #[test]
    fn skip_whitespace<'a>() {
        let monad = |input: &'a str| {
            let (out, rest) = input.split_at(1);
            (Ok::<&'a str, ()>(out), rest)
        };

        assert_eq!(
            super::skip_whitespace(monad).apply("  \t\n  a"),
            (Ok("a"), "")
        );
        assert_eq!(super::skip_whitespace(monad).apply("a"), (Ok("a"), ""));
    }

    #[test]
    fn integer() {
        assert_eq!(
            super::integer().apply("1"),
            (
                Ok(Token {
                    lexeme: "1",
                    kind: TokenKind::Int,
                }),
                ""
            )
        );
        assert_eq!(
            super::integer().apply("1234567890"),
            (Ok(Token { lexeme: "1234567890", kind: TokenKind::Int }), "")
        );
        assert_eq!(
            super::integer().apply("123 foo"),
            (Ok(Token { lexeme: "123", kind: TokenKind::Int }), " foo")
        );
        assert_eq!(
            super::integer().apply("f123oo"),
            (Err(TokenizerError {
                found: "f123oo",
                kind: TokenizerErrorKind::ExpectedInt
            }), "f123oo")
        );
    }

    #[test]
    fn ident() {
        assert_eq!(
            super::identifier().apply("a"),
            (
                Ok(Token {
                    lexeme: "a",
                    kind: TokenKind::Identifier,
                }),
                ""
            )
        );
        assert_eq!(
            super::identifier().apply("a1_Bababab"),
            (Ok(Token { lexeme: "a1_Bababab", kind: TokenKind::Identifier}), "")
        );
        assert_eq!(
            super::identifier().apply("_foo foo"),
            (Ok(Token { lexeme: "_foo", kind: TokenKind::Identifier }), " foo")
        );
        assert_eq!(
            super::identifier().apply("123foo"),
            (Err(TokenizerError {
                found: "123foo",
                kind: TokenizerErrorKind::ExpectedIdentifier
            }), "123foo")
        );
    }
}
