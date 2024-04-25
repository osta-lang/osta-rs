#[derive(Debug, PartialEq)]
pub enum ParseError {
    LexError(LexError),
    SyntacticError(SyntacticError),
}

#[derive(Debug, PartialEq)]
pub enum LexError {
    ExpectedInteger,
    ExpectedIdentifier,
}

#[derive(Debug, PartialEq)]
pub enum SyntacticError {}

// TODO: Semantic error
