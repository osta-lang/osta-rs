use osta_ast::{Data, NodeKind};
use osta_lexer::{base::*, tokens::integer};
use crate::{Parser, ParserError, ParserInput};

fn data<'a, E>(emitter: E) -> impl Parser<'a>
where
    E: TokenEmitter<'a>
{
    move |input: ParserInput<'a>| {
        let (result, rest) = emitter.apply(input.input);
        match result {
            Ok(out) => {
                let mut builder = input.builder.borrow_mut();
                let data_ref = builder.push_data(Data::Token(out));
                let node_ref = builder.push_node(NodeKind::Data(data_ref), !0); // TODO: Node must have a daddy
                (Ok((node_ref, Some(data_ref))), ParserInput { input: rest, builder: input.builder.clone() })
            },
            Err(err) => (Err(ParserError::TokenizerError(err)), input)
        }
    }
}

/*
pub fn term<'a>() -> impl Parser<'a> {
    data(integer())
}
*/

// TODO(cdecompilador): This tests testing Ast and AstBuilder don't follow SRP, so technically they
// should be on osta-ast, but in osta-ast we don't have any real parser to test so for now their
// tests live within rules
#[cfg(test)]
mod tests {
    use osta_lexer::token::*;
    use osta_ast::*;
    use osta_func::*;
    use crate::Parser;

    macro_rules! input {
        ($str_input:expr) => {{
            crate::ParserInput {
                input: $str_input,
                builder: std::rc::Rc::new(std::cell::RefCell::new(crate::AstBuilder::new()))
            }
        }};
    }

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
    
    #[test]
    fn data() {
        let input = input!("  123");
        assert_ast!(
            super::data(osta_lexer::tokens::integer()), input,
            [
                Node { kind: NodeKind::Data(0), .. }
            ], 
            [
                Data::Token(Token { kind: TokenKind::Int, .. })
            ]
        );
    }

    #[test]
    fn data_sequence() {
        let input = input!("123 foo");
        let rest = assert_ast!(
            super::data(osta_lexer::tokens::integer()), input,
            [
                Node { kind: NodeKind::Data(0), .. }
            ], 
            [
                Data::Token(Token { kind: TokenKind::Int, .. })
            ]
        );

        assert_ast!(
            super::data(osta_lexer::tokens::identifier()), rest,
            [
                _,
                Node { kind: NodeKind::Data(1), .. }
            ],
            [
                _,
                Data::Token(Token { kind: TokenKind::Identifier, .. })
            ]
        );
    }

    // FIXME:
    #[test]
    fn data_rollback() {
        let input = input!("foo");
        assert_ast!(
            FallibleStateMonad::and_then(
                super::data(osta_lexer::tokens::identifier()),
                |_| super::data(osta_lexer::tokens::integer())
            )
                .map_err(|err| err.unwrap())
                .or(super::data(osta_lexer::tokens::identifier())),
            input,
            [ 
                Node { kind: NodeKind::Data(0), .. } 
            ],
            [
                Data::Token(Token { kind: TokenKind::Identifier, .. })
            ]
        );
    }
}
