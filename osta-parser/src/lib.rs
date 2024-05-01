mod rules;

use std::cell::{RefCell, RefMut};
use std::rc::Rc;
use osta_ast::{AstBuilder, Data, DataRef, NodeKind, NodeRef};
use osta_func::{StateMonad, FallibleStateMonad};
use osta_lexer::token::TokenKind;
use osta_proc_macros::sequence;

#[derive(Debug, Clone)]
pub struct ParserInput<'a> {
    input: &'a str,
    builder: Rc<RefCell<AstBuilder<'a>>>
}
type ParserOutput<'a> = (NodeRef, Option<DataRef>);

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ParserError<'a> {
    TokenizerError(osta_lexer::error::TokenizerError<'a>),
    UnexpectedToken { expected: TokenKind, found: TokenKind },
    Unknown
}

pub trait Parser<'a>: FallibleStateMonad<'a, ParserInput<'a>, ParserOutput<'a>, ParserError<'a>> {
    fn and<P: Parser<'a> + 'a, F>(self, parser_factory: F) -> impl Parser<'a>
    where
        F: FnOnce(ParserOutput) -> P + Copy + 'a
    {
        FallibleStateMonad::and_then(self, parser_factory).map_err(|err| err.unwrap())
    }

    fn or<P: Parser<'a> + 'a>(self, parser: P) -> impl Parser<'a> {
        move |input: ParserInput<'a>| {
            input.builder.borrow_mut().checkpoint();
            let (result, rest) = self.apply(input.clone());
            let mut builder = input.builder.borrow_mut();
            match result {
                Ok(out) => {
                    builder.commit();
                    (Ok(out), rest)
                },
                Err(err) => {
                    if let Err(err) = builder.rollback(err) {
                        return (Err(err), input.clone());
                    }
                    drop(builder);
                    let (result, rest) = parser.apply(input.clone());
                    match result {
                        Ok(out) => (Ok(out), rest),
                        Err(err) => (Err(err), input.clone())
                    }
                }
            }
        }
    }
}

impl<'a, M> Parser<'a> for M
where
    M: FallibleStateMonad<'a, ParserInput<'a>, ParserOutput<'a>, ParserError<'a>>
{}
