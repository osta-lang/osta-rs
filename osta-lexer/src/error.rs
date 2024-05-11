#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TokenizerError<'a> {
    pub kind: TokenizerErrorKind<'a>,
    pub found: &'a str,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenizerErrorKind<'a> {
    // NOTE(cdecompilador): This kind of errors will appear like 
    // expected: +, -, ;, ::, !
    ExpectedLiteral(&'a str),

    // NOTE(johanvonelectrum): This kind of errors will appear like:
    // expected regex: [0-9]+
    ExpectedRegex(&'a str),

    // NOTE(cdecompilador): This kind of errors will appear with an example:
    // expected keyword: if, while
    // ...example for if
    // ...example for while 
    ExpectedKeyword(&'a str),

    /// An identifier was expected, this will most likely never be a fallible error
    /// on the tokenizer, since will be the parser the one that will say if that
    /// identifier was expected to be a type, variable name, ...
    ExpectedIdentifier,

    // NOTE(cdecompilador): This kind of erros will appear like:
    // expected integer
    ExpectedInt,

    /// Unexpected input, the compiler will print the TokenizerError.found to
    /// the console with no further suggestions
    Unexpected,
}
