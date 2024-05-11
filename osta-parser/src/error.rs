// TODO(cdecompilador): we need a way not only to create errors from parsing
// but to register them on a non-fallible way

use osta_lexer::token::TokenKind;
use osta_lexer::error::TokenizerError;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ParserError<'a> {
    TokenizerError(TokenizerError<'a>),
    UnexpectedToken { expected: TokenKind, found: TokenKind },
    Unknown
}
