use osta_func::monads::state_monad::StateMonad;

#[derive(Debug, Copy, Clone)]
pub struct Token<'a> {
    kind: TokenKind,
    lexeme: &'a str,
}

#[derive(Debug, Copy, Clone)]
pub enum TokenKind {
    Keyword,
}

#[derive(Debug, Copy, Clone)]
pub struct TokenizerError<'a> {
    pub kind: TokenizerErrorKind<'a>,
    pub input: &'a str,
}

#[derive(Debug, Copy, Clone)]
pub enum TokenizerErrorKind<'a> {
    ExpectedOneOf(&'a [TokenKind], char),
    UnexpectedCharacter(char, char),
}

#[derive(Copy, Clone)]
pub struct Tokenizer<'a> {
    input: &'a str,
}

impl<'a> StateMonad<'a, &'a str, Result<Token<'a>, TokenizerError<'a>>> for Tokenizer<'a> {
    fn apply(self, input: &'a str) -> (Result<Token<'a>, TokenizerError<'a>>, &'a str) {
        todo!("Implement Tokenizer::apply")
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
