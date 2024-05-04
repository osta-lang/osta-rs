use std::{cell::RefCell, rc::Rc};

use osta_ast::{NodeRef, AstBuilder};
use osta_func::FallibleStateMonad;

#[derive(Debug, Clone)]
pub struct ParserInput<'a> {
    pub input: &'a str,
    pub builder: Rc<RefCell<AstBuilder<'a>>>
}

pub type ParserOutput = NodeRef;

/// Trait alias of FallibleStateMonad specialized for the parsing step
pub trait Parser<'a>: FallibleStateMonad<'a, ParserInput<'a>, ParserOutput, crate::error::ParserError<'a>> {
    /// Creates a new parser that attempts to parse with both alterntives, since this monads are not
    /// pure and have external effects when one fails its changes are be rollbacked so no dangling
    /// nodes end up on the final AST
    ///
    /// # NOTE
    /// Never use '.or_else()' with a 'Parser' since it does't do rollbacks
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

    /// Utility method that transform errors to a null NodeRef
    fn optional(self) -> impl Parser<'a> {
        move |input: ParserInput<'a>| {
            let (result, rest) = self.apply(input.clone());
            match result {
                Ok(out) => (Ok(out), rest),
                Err(_) => (Ok(NodeRef::NULL), input)
            }
        }
    }
}
impl<'a, M> Parser<'a> for M
where
    M: FallibleStateMonad<'a, ParserInput<'a>, ParserOutput, crate::error::ParserError<'a>>
{}

#[cfg(test)]
mod tests {
    use crate::rules::{tests::*, do_parse};
    use osta_ast::*;
    use osta_func::*;
    use super::*;
    
    #[test]
    fn node_rollback() {
        let p = (do_parse! {
            crate::rules::identifier();
            crate::rules::integer();
        }).or(crate::rules::identifier());
        let input = input!("foo $");
        let rest = assert_ast!(
            p, input,
            [ 
                Node { kind: NodeKind::Identifier(DataRef(0)), .. }
            ],
            [
                ..
            ]
        );
        assert_eq!(rest.input, " $");
    }
}
