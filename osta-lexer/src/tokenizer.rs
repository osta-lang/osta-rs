use crate::token::*;
use crate::error::*;
use crate::combinators::*;

use osta_func::*;

#[derive(Copy, Clone)]
pub struct Tokenizer<'a> {
    input: &'a str,
}

impl<'a> StateMonad<'a, &'a str, Result<Token<'a>, TokenizerError<'a>>> for Tokenizer<'a> {
    fn apply(self, input: &'a str) -> (Result<Token<'a>, TokenizerError<'a>>, &'a str) {
        keyword("while", TokenKind::While)
            .or_else(|_| keyword("if", TokenKind::If))
            .or_else(|_| keyword("else", TokenKind::Else))
            .or_else(|_| integer())
            .or_else(|_| identifier())
            .or_else(|_| token("+", TokenKind::Plus))
            .or_else(|_| token("-", TokenKind::Minus))
            .or_else(|_| token("*", TokenKind::Star))
            .or_else(|_| token(";", TokenKind::Semicolon))
            .or_else(|_| token("(", TokenKind::LParen))
            .or_else(|_| token(")", TokenKind::RParen))
            .or_else(|_| token("{", TokenKind::LBrace))
            .or_else(|_| token("}", TokenKind::RBrace))
            /*
            TODO:
            .or_else(|_| maybe_two(keyword("=", TokenKind::Eq), token("=", TokenKind::EqEq)))
            .or_else(|_| maybe_two(keyword("!", TokenKind::Bang), token("=", TokenKind::BangEq)))

            REMOVE:
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
            */
            .map_out(|out| out.unwrap())
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
