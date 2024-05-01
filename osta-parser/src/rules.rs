use osta_ast::*;
use osta_func::*;
use osta_lexer::{base::*, tokens};

use crate::{Parser, ParserError, ParserInput};

fn token<'a, E, F>(emitter: E, map_fn: F) -> impl Parser<'a>
    where
        E: TokenEmitter<'a>,
        F: Fn(DataRef) -> NodeKind + Copy + 'a
{
    move |input: ParserInput<'a>| {
        let (result, rest) = emitter.apply(input.input);
        match result {
            Ok(out) => {
                let mut builder = input.builder.borrow_mut();
                let data_ref = builder.push_data(Data::Token(out));
                let node_ref = builder.push_node(map_fn(data_ref), !0);
                (Ok((node_ref, Some(data_ref))), ParserInput { input: rest, builder: input.builder.clone() })
            }
            Err(err) => (Err(ParserError::TokenizerError(err)), input)
        }
    }
}

pub fn integer<'a>() -> impl Parser<'a> {
    token(tokens::integer(), |data_ref| NodeKind::IntegerLiteral(data_ref))
}

pub fn identifier<'a>() -> impl Parser<'a> {
    token(tokens::identifier(), |data_ref| NodeKind::Identifier(data_ref))
}

pub fn term<'a>() -> impl Parser<'a> {
    integer()
        .or(identifier())
        .and(move |(pre_node_ref, _)| move |input: ParserInput<'a>| {
            let mut builder = input.builder.borrow_mut();
            let node_ref = builder.push_node(NodeKind::Term(pre_node_ref), !0);
            builder.ast.nodes[pre_node_ref].parent = node_ref;
            drop(builder);
            (Ok((node_ref, None)), input)
        })
}

macro_rules! defer {
    ($d:expr) => {{
        move |input| $d.apply(input)
    }};
}

pub fn expr<'a>() -> impl Parser<'a> {
    term().and(move |(left_node_ref, _)| move |mut input: ParserInput<'a>| {
            let (op_result, rest) = tokens::plus()
                .map_err(ParserError::TokenizerError)
                .apply(input.input);
            let token = match op_result {
                Ok(token) => token,
                Err(err) => return (Err(err), input)
            };
            input.input = rest;
            expr().and(move |(right_node_ref, _)| move |input: ParserInput<'a>| {
                let mut builder = input.builder.borrow_mut();
                let expr_kind = NodeKind::BinExpr {
                    left: left_node_ref,
                    op: builder.push_data(Data::Token(token)),
                    right: right_node_ref
                };
                let expr_ref = builder.push_node(expr_kind, !0);
                builder.ast.nodes[left_node_ref].parent = expr_ref;
                builder.ast.nodes[right_node_ref].parent = expr_ref;
                drop(builder);
                (Ok((expr_ref, None)), input)
            }).apply(input)
        }).or(term())
}

// TODO(cdecompilador): This tests testing Ast and AstBuilder don't follow SRP, so technically they
// should be on osta-ast, but in osta-ast we don't have any real parser to test so for now their
// tests live within rules
#[cfg(test)]
mod tests {
    use osta_ast::*;
    use osta_func::*;
    use osta_lexer::token::*;

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
    fn test_integer() {
        let input = input!("123");
        assert_ast!(
            super::integer(), input,
            [
                Node { kind: NodeKind::IntegerLiteral(0), .. }
            ], 
            [
                Data::Token(Token { kind: TokenKind::Int, .. })
            ]
        );
    }

    #[test]
    fn test_identifier() {
        let input = input!("foo");
        let rest = assert_ast!(
            super::identifier(), input,
            [
                Node { kind: NodeKind::Identifier(0), .. }
            ], 
            [
                Data::Token(Token { kind: TokenKind::Identifier, .. })
            ]
        );
        assert_eq!(rest.input, "");
    }

    #[test]
    fn data_rollback() {
        let input = input!("foo $");
        let rest = assert_ast!(
            super::identifier().and(|_| super::integer()).or(super::identifier()), input,
            [ 
                Node { kind: NodeKind::Identifier(0), .. }
            ],
            [
                Data::Token(Token { kind: TokenKind::Identifier, .. })
            ]
        );
        assert_eq!(rest.input, " $");
    }

    #[test]
    fn expr() {
        let input = input!("123 + 456");
        let rest = assert_ast!(
            super::expr(), input,
            [
                Node { kind: NodeKind::IntegerLiteral(0), parent: 1 },
                Node { kind: NodeKind::Term(0), parent: 4 },
                Node { kind: NodeKind::IntegerLiteral(1), parent: 3 },
                Node { kind: NodeKind::Term(2), parent:  4 },
                Node { kind: NodeKind::BinExpr { left: 1, op: 2, right: 3 }, .. }
            ],
            [
                Data::Token(Token { kind: TokenKind::Int, .. }),
                Data::Token(Token { kind: TokenKind::Int, .. }),
                Data::Token(Token { kind: TokenKind::Plus, .. })
            ]
        );
        assert_eq!(rest.input, "");
    }
}
