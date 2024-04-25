pub enum ParseError {
    LexError(LexError),
    SintacticError(SintacticError),
}

pub enum LexError {
    ExpectedCharacter(char),
    ExpectedPattern(String),
    ExpectedOneOf(Vec<String>),
    InvalidIntLiteral,
    UnclosedStringLiteral,
}

pub enum SintacticError {}

// TODO: Semantic error
