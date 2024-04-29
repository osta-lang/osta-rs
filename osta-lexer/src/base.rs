use crate::error::*;
use crate::token::*;

use osta_func::FallibleStateMonad;

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
        if let Some(matched) = regex.find(input){
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
}
