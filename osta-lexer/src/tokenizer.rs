use crate::token::*;
use crate::error::*;
use crate::combinators::*;

use osta_func::*;
use osta_func::fallible::foundational::composition;
use osta_func::foundational::optional;

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
            .or_else(|_| composition(token("=", TokenKind::Eq), token("=", TokenKind::EqEq))
                .map_out(|out| out.unwrap()))
            .or_else(|_| composition(token("!", TokenKind::Bang), token("=", TokenKind::BangEq))
                .map_out(|out| out.unwrap()))
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
