use osta_func::{monads::{state_monad::StateMonad, fallible_state_monad::FallibleStateMonad}, combinators::foundational::optional};

use crate::combinators::{literal, skip_whitespace};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub lexeme: &'a str,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // Keywords
    While,
    If,
    Else,

    // Single character tokens
    Plus,
    Minus,
    Star,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Eq,
    Bang,

    // Multriple character tokens
    EqEq,
    BangEq,

    // Literals
    String,
    Int,

    // Other
    Eof
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TokenizerError<'a> {
    pub kind: TokenizerErrorKind<'a>,
    pub data: &'a str,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenizerErrorKind<'a> {
    ExpectedOneOf(&'a [TokenKind], char),
    UnexpectedCharacter(char, char),
    ExpectedKeyword,
}

#[derive(Copy, Clone)]
pub struct Tokenizer<'a> {
    input: &'a str,
}

impl<'a> StateMonad<'a, &'a str, Result<Token<'a>, TokenizerError<'a>>> for Tokenizer<'a> {
    fn apply(self, input: &'a str) -> (Result<Token<'a>, TokenizerError<'a>>, &'a str) {
        literal("while").map_out(|out| Token { lexeme: out, kind: TokenKind::While })
            .or_else(|_| literal("if").map_out(|out| Token { lexeme: out, kind: TokenKind::If }))
            .or_else(|_| literal("else").map_out(|out| Token { lexeme: out, kind: TokenKind::Else }))
            .or_else(|_| literal("+").map_out(|out| Token { lexeme: out, kind: TokenKind::Plus }))
            .or_else(|_| literal("-").map_out(|out| Token { lexeme: out, kind: TokenKind::Minus }))
            .or_else(|_| literal("*").map_out(|out| Token { lexeme: out, kind: TokenKind::Star }))
            .or_else(|_| literal(";").map_out(|out| Token { lexeme: out, kind: TokenKind::Semicolon }))
            .or_else(|_| literal("(").map_out(|out| Token { lexeme: out, kind: TokenKind::LParen }))
            .or_else(|_| literal(")").map_out(|out| Token { lexeme: out, kind: TokenKind::RParen }))
            .or_else(|_| literal("{").map_out(|out| Token { lexeme: out, kind: TokenKind::LBrace }))
            .or_else(|_| literal("}").map_out(|out| Token { lexeme: out, kind: TokenKind::RBrace }))
            .or_else(|_| FallibleStateMonad::and_then(literal("="), 
                move |left_out| optional(literal("=")).map(move |right_out| match right_out {
                    Some(right_out) => Ok::<Token<'_>, TokenizerError<'_>>(Token { lexeme: right_out, kind: TokenKind::EqEq }),
                    None => Ok(Token { lexeme: left_out, kind: TokenKind::Eq })
                })))
            .or_else(|_| FallibleStateMonad::and_then(literal("!"), 
                move |left_out| optional(literal("=")).map(move |right_out| match right_out {
                    Some(right_out) => Ok::<Token<'_>, TokenizerError<'_>>(Token { lexeme: right_out, kind: TokenKind::Bang }),
                    None => Ok(Token { lexeme: left_out, kind: TokenKind::BangEq })
                })))
            .map_out(|out| out.unwrap())
            .map_err(|_| TokenizerError { data: input, kind: TokenizerErrorKind::ExpectedKeyword })
            .apply(input)    
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.acquire())
    }
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
        }
    }

    pub fn acquire(&mut self) -> Token<'a> {
        match self.apply(self.input) {
            (Ok(token), input) => {
                self.input = input;
                token
            }
            (Err(err), input) => {
                todo!("Implement Tokenizer::acquire error handling")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn it_works() {
        assert_eq!(
            Tokenizer::new("while").acquire(),
            Token {
                lexeme: "while",
                kind: TokenKind::While 
            }
        )
    }
}
