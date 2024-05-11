use std::cell::RefMut;

use osta_ast::{AstBuilder, NodeRef};
use osta_func::*;
use osta_lexer::{token::Token, base::TokenEmitter};

use crate::{*, error::ParserError};

mod expr;
mod stmt;
mod block;
mod flow;

pub use expr::*;
pub use stmt::*;
pub use block::*;

macro_rules! do_parse {
    ($($r:tt)*) => { (osta_func::do_fallible!($($r)*)).map_err(move |err| err.unwrap()) };
}
pub(crate) use do_parse;

/// Combinator that transforms a token emitter monad to a valid parser monad
fn from_emitter<'a, E>(emitter: E) -> impl FallibleStateMonad<'a, ParserInput<'a>, Token<'a>, ParserError<'a>>
where
    E: TokenEmitter<'a>
{
    move |input: ParserInput<'a>| {
        let (result, rest) = emitter
            .map_err(ParserError::TokenizerError)
            .apply(input.input);
        (result, ParserInput { input: rest, builder: input.builder.clone() })
    }
}

/// Non-fallible utility combinator that lets you take a mutable reference to the AstBuilder
pub fn with_builder<'a, F>(with_builder_fn: F) -> impl Parser<'a> 
where 
    F: Fn(RefMut<'_, AstBuilder<'a>>) -> NodeRef + Copy + 'a
{
    move |input: ParserInput<'a>| {
        let builder = input.builder.borrow_mut();
        (Ok(with_builder_fn(builder)), input)
    }
}

#[cfg(test)]
pub mod tests {
    macro_rules! input {
        ($str_input:expr) => {{
            crate::ParserInput {
                input: $str_input,
                builder: std::rc::Rc::new(std::cell::RefCell::new(osta_ast::AstBuilder::new()))
            }
        }};
    }
    pub(crate) use input;

    macro_rules! assert_ast {
        ($p:expr, $input:expr, $nodes:pat, $datas:pat) => {{
            let (_, rest) = dbg!($p.apply($input));

            // NOTE(cdecompilador): using matches! here may make some literal matching painful
            // for example we can't write !0 inside
            assert!(matches!(
                &rest.builder.borrow().ast.nodes[..],
                $nodes
            ));
            assert!(matches!(
                &rest.builder.borrow().ast.datas[..],
                $datas
            ));

            rest
        }};
    }
    pub(crate) use assert_ast;
}
