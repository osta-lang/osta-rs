use osta_func::FallibleStateMonad;

use crate::base::*;
use crate::error::*;
use crate::token::*;

lazy_static::lazy_static! {
    static ref RE_INT: regex::Regex = regex::Regex::new("^[0-9]+").unwrap();
    static ref RE_IDENTIFIER: regex::Regex = regex::Regex::new("^[_a-zA-Z][_a-zA-Z0-9]*").unwrap();
}

fn keyword(
    expected: &str,
    token_kind: TokenKind
) -> impl FallibleStateMonad<&str, Token, TokenizerError> {
    token(expected, token_kind).map_err(|err| TokenizerError {
        found: err.found,
        kind: TokenizerErrorKind::ExpectedKeyword(expected)
    })
}

macro_rules! emitter {
    ($name:ident, $body:expr) => {
        pub fn $name<'a>() -> impl TokenEmitter<'a> {
            skip_whitespace($body)
        }
    };
}

emitter!(kw_while, keyword("while", TokenKind::While));
emitter!(kw_if, keyword("if", TokenKind::If));
emitter!(kw_else, keyword("else", TokenKind::Else));
emitter!(kw_return, keyword("return", TokenKind::Return));
emitter!(integer, regex(&RE_INT, TokenKind::Int)
    .map_err(|err| TokenizerError {
        found: err.found,
        kind: TokenizerErrorKind::ExpectedInt
    }));
emitter!(identifier, regex(&RE_IDENTIFIER, TokenKind::Identifier)
    .map_err(|err| TokenizerError {
        found: err.found,
        kind: TokenizerErrorKind::ExpectedIdentifier
    }));
emitter!(lparen, token("(", TokenKind::LParen));
emitter!(rparen, token(")", TokenKind::RParen));
emitter!(lbrace, token("{", TokenKind::LBrace));
emitter!(rbrace, token("}", TokenKind::RBrace));
emitter!(plus, token("+", TokenKind::Plus));
emitter!(minus, token("-", TokenKind::Minus));
emitter!(star, token("*", TokenKind::Star));
emitter!(semicolon, token(";", TokenKind::Semicolon));
emitter!(colon, token(":", TokenKind::Colon));
emitter!(comma, token(",", TokenKind::Comma));
emitter!(eq, token("=", TokenKind::Eq));
emitter!(bang, token("!", TokenKind::Bang));
emitter!(bin_op, plus()
    .or_else(|_| minus())
    .or_else(|_| star())
    .map_out(|out| out.unwrap()));
emitter!(unary_op, minus()
    .or_else(|_| bang())
    .map_out(|out| out.unwrap()));

#[cfg(test)]
mod tests {
    use osta_func::StateMonad;

    use super::*;

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

    #[test]
    fn tokens() {
        assert_eq!(
            super::lparen().apply("  ("),
            (Ok(Token {
                lexeme: "(",
                kind: TokenKind::LParen
            }), "")  
        );
        assert_eq!(
            super::identifier().apply(" foo bar"),
            (Ok(Token {
                lexeme: "foo",
                kind: TokenKind::Identifier
            }), " bar")
        );
    }

    #[test]
    fn ops() {
        assert_eq!(
            super::bin_op().apply("+"),
            (Ok(Token {
                lexeme: "+",
                kind: TokenKind::Plus
            }), "")
        );
        assert_eq!(
            super::bin_op().apply("-"),
            (Ok(Token {
                lexeme: "-",
                kind: TokenKind::Minus
            }), "")
        );
        assert_eq!(
            super::bin_op().apply("*foo"),
            (Ok(Token {
                lexeme: "*",
                kind: TokenKind::Star
            }), "foo")
        );
        assert_eq!(
            super::unary_op().apply("!foo"),
            (Ok(Token {
                lexeme: "!",
                kind: TokenKind::Bang
            }), "foo")
        );
        assert_eq!(
            super::unary_op().apply("-foo"),
            (Ok(Token {
                lexeme: "-",
                kind: TokenKind::Minus
            }), "foo")
        );
    }
}
