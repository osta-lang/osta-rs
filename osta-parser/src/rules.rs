use std::cell::RefMut;

use osta_ast::*;
use osta_func::*;
use osta_func::foundational::optional;
use osta_lexer::{base::*, tokens};
use osta_lexer::token::Token;
use osta_proc_macros::sequence;

use crate::{Parser, ParserError, ParserInput};

macro_rules! do_parse {
    ($($r:tt)*) => { (do_fallible!($($r)*)).map_err(move |err| err.unwrap()) };
}

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

/// Non-fallible utility combinator that lets you take a mutable reference to the AstBuilder
// NOTE(cdecompilador): If we do typing on a separate part of the nodes in a attrbute-like 
// way we could return from just the node ref, which in fact is the only thing we use on later stages
pub fn with_builder<'a, F>(with_builder_fn: F) -> impl Parser<'a> 
where 
    F: Fn(RefMut<'_, AstBuilder<'a>>) -> (NodeRef, Option<DataRef>) + Copy + 'a
{
    move |input: ParserInput<'a>| {
        let builder = input.builder.borrow_mut();
        (Ok(with_builder_fn(builder)), input)
    }
} 

pub fn integer<'a>() -> impl Parser<'a> {
    do_parse! {
        token = from_emitter(tokens::integer());
        with_builder(move |mut builder| {
            builder.push_integer(token)
        });
    }
}

pub fn identifier<'a>() -> impl Parser<'a> {
    do_parse! {
        token = from_emitter(tokens::identifier());
        with_builder(move |mut builder| {
            builder.push_identifier(token) 
        });
    }
}

pub fn term<'a>() -> impl Parser<'a> {
    let build_term = move |child_ref| with_builder(move |mut builder| {
        (builder.push_term(child_ref), None)
    });

    (do_parse! {
        (child_ref, _) = integer();
        build_term(child_ref);
    }).or(do_parse! {
        (child_ref, _) = identifier();
        build_term(child_ref);
    }).or(do_parse! {
        from_emitter(tokens::lparen()); (child_ref, _) = defer!(expr()); from_emitter(tokens::rparen());
        build_term(child_ref);
    }).or(do_parse! {
        token = from_emitter(tokens::unary_op());
        (child_ref, _) = defer!(term());
        with_builder(move |mut builder| {
            (builder.push_unary(token, child_ref), None)
        });
    })
}

pub fn params<'a>() -> impl Parser<'a> {
    (do_parse! {
        (expr_ref, _) = expr();
        from_emitter(tokens::comma());
        (next_ref, _) = defer!(params());
        with_builder(move |mut builder| {
            (builder.push_param(expr_ref, Some(next_ref)), None)
        });
    }).or(do_parse! {
        (expr_ref, _) = expr();
        with_builder(move |mut builder| {
            (builder.push_param(expr_ref, None), None)
        });
    })
}

pub fn function_call_expr<'a>() -> impl Parser<'a> {
    do_parse!(
        (name, _) = identifier();
        from_emitter(tokens::lparen());
        do_parse! {
            (params_ref, _) = params();
            from_emitter(tokens::rparen());
            with_builder(move |mut builder| {
                (builder.push_function_call_expr(name, Some(params_ref)), None)
            });
        }.or(do_parse! {
            from_emitter(tokens::rparen());
            with_builder(move |mut builder| {
                (builder.push_function_call_expr(name, None), None)
            });
        });
    )
}

pub fn expr<'a>() -> impl Parser<'a> {
    defer!(function_call_expr()).or(do_parse! {
        (left_ref, _) = term();
        token = from_emitter(tokens::plus());
        (right_ref, _) = defer!(expr());
        with_builder(move |mut builder| {
            (builder.push_bin_expr(left_ref, token, right_ref), None)
        });
    }).or(term())
}

pub fn expr_stmt<'a>() -> impl Parser<'a> {
    do_parse! {
        (expr_ref, _) = expr();
        from_emitter(tokens::semicolon());
        with_builder(move |mut builder| {
            (builder.push_expr_stmt(expr_ref), None)
        });
    }
}

pub fn assign_stmt<'a>() -> impl Parser<'a> {
    do_parse! {
        (name_ref, _) = identifier();
        from_emitter(tokens::eq());
        (expr_ref, _) = expr();
        from_emitter(tokens::semicolon());
        with_builder(move |mut builder| {
            (builder.push_assign_stmt(name_ref, expr_ref), None)
        });
    }
}

pub fn block_stmt<'a>() -> impl Parser<'a> {
    expr_stmt().or(assign_stmt())
}

pub fn inner_block<'a>() -> impl Parser<'a> {
    do_parse! {
        (stmt_ref, _) = block_stmt();
        (next_ref, _) = defer!(inner_block());
        with_builder(move |mut builder| {
            if next_ref == NULL_REF {
                (builder.push_stmt(stmt_ref, None), None)
            } else {
                (builder.push_stmt(stmt_ref, Some(next_ref)), None)
            }
        });
    }.or(move |input| (Ok((NULL_REF, None)), input))
}

// NOTE(cdecompilador): this may result in O(2^n) not funny
pub fn block<'a>() -> impl Parser<'a> {
    do_parse! {
        from_emitter(tokens::lbrace());
        (first_stmt_ref, _) = inner_block();
        (first_stmt_ref, _) = do_parse! {
            (return_expr_ref, _) = expr();
            from_emitter(tokens::rbrace());
            with_builder(move |mut builder| {
                if first_stmt_ref == NULL_REF {
                    (builder.push_stmt(return_expr_ref, None), None)
                } else {
                    (builder.push_stmt(return_expr_ref, Some(first_stmt_ref)), None)
                }
            });
        }.or(from_emitter(tokens::rbrace()).map_out(move |_| (first_stmt_ref, None)));
        with_builder(move |mut builder| {
            (builder.push_block(first_stmt_ref), None)
        });
    }.or(do_parse! {
        from_emitter(tokens::lbrace());
        (first_stmt_ref, _) = inner_block();
        from_emitter(tokens::rbrace());
        with_builder(move |mut builder| {
            (builder.push_block(first_stmt_ref), None)
        });
    })
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
                Node { kind: NodeKind::IntegerLiteral(0), parent: NULL_REF }
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
    fn term() {
        let input = input!("(123)");
        assert_ast!(
            super::term(), input,
            [
                Node { kind: NodeKind::IntegerLiteral(0), parent: 1 },
                Node { kind: NodeKind::Term(0), parent: 2 },
                Node { kind: NodeKind::Term(1), parent: NULL_REF }
            ],
            [
                Data::Token(Token { kind: TokenKind::Int, .. })
            ]
        );

        let input = input!("(123 + foo)");
        assert_ast!(
            super::term(), input,
            [
                Node { kind: NodeKind::IntegerLiteral(0), parent: 1 },
                Node { kind: NodeKind::Term(0), parent: 4 },
                Node { kind: NodeKind::Identifier(1), parent: 3 },
                Node { kind: NodeKind::Term(2), parent:  4 },
                Node { kind: NodeKind::BinExpr { left: 1, op: 2, right: 3 }, parent: 5 },
                Node { kind: NodeKind::Term(4), parent: NULL_REF }
            ],
            [
                Data::Token(Token { kind: TokenKind::Int, .. }),
                Data::Token(Token { kind: TokenKind::Identifier, .. }),
                Data::Token(Token { kind: TokenKind::Plus, .. })
            ]
        );

        let input = input!("-123");
        assert_ast!(
            super::term(), input,
            [
                Node { kind: NodeKind::IntegerLiteral(0), parent: 1 },
                Node { kind: NodeKind::Term(0), parent: 2 },
                Node { kind: NodeKind::UnaryTerm { op: 1, child: 1 }, parent: NULL_REF }
            ],
            [
                Data::Token(Token { kind: TokenKind::Int, .. }),
                Data::Token(Token { kind: TokenKind::Minus, .. })
            ]
        );
    }

    #[test]
    fn node_rollback() {
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
    fn binary_expr() {
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

    #[test]
    fn params() {
        let input = input!("10");
        assert_ast!(
            super::params(), input,
            [
                Node { kind: NodeKind::IntegerLiteral(0), parent: 1 },
                Node { kind: NodeKind::Term(0), parent: 2 },
                Node { kind: NodeKind::Param { child: 1, next: None }, parent: NULL_REF }
            ],
            [..]
        );

        let input = input!("10,foo");
        assert_ast!(
            super::params(), input,
            [
                Node { kind: NodeKind::IntegerLiteral(0), .. },
                Node { kind: NodeKind::Term(0), .. },
                Node { kind: NodeKind::Identifier(1), .. },
                Node { kind: NodeKind::Term(2), .. },
                Node { kind: NodeKind::Param { child: 3, next: None }, parent: 5 },
                Node { kind: NodeKind::Param { child: 1, next: Some(4)}, parent: NULL_REF },
            ],
            [..]
        );
    }

    #[test]
    fn function_call_expr() {
        let input = input!("exit()");
        assert_ast!(
            super::function_call_expr(), input,
            [
                Node { kind: NodeKind::Identifier(0), parent: 1 },
                Node { kind: NodeKind::FuncCallExpr { name: 0, params: None }, .. }
            ],
            [..]
        );

        let input = input!("print(hello)");
        assert_ast!(
            super::function_call_expr(), input,
            [
                Node { kind: NodeKind::Identifier(0), parent: 4 },
                Node { kind: NodeKind::Identifier(1), .. },
                Node { kind: NodeKind::Term(1), .. },
                Node { kind: NodeKind::Param { child: 2, next: None }, .. },
                Node { kind: NodeKind::FuncCallExpr { name: 0, params: Some(3) }, .. }
            ],
            [..]
        );
    }

    #[test]
    fn assign_stmt() {
        let input = input!("a = 10;");
        assert_ast!(
            super::block_stmt(), input,
            [
                Node { kind: NodeKind::Identifier(0), .. },
                Node { kind: NodeKind::IntegerLiteral(1), .. },
                Node { kind: NodeKind::Term(1), .. },
                Node { kind: NodeKind::AssignStmt { name: 0, expr: 2 }, ..}
            ],
            [..]
        );
    }

    #[test]
    fn expr_stmt() {
        let input = input!("1 + exit();");
        assert_ast!(
            super::block_stmt(), input,
            [
                _,
                _,
                _,
                Node { kind: NodeKind::FuncCallExpr { .. }, .. },
                Node { kind: NodeKind::BinExpr { .. }, .. },
                Node { kind: NodeKind::ExprStmt { expr: 4 }, .. }
            ],
            [..]
        );
    }

    #[test]
    fn block() {
        let input = input!("{} ");
        assert_ast!(
            super::block(), input,
            [
               Node { kind: NodeKind::Block { first: None }, .. } 
            ],
            [..]
        );

        let input = input!("{ 1; 2; }");
        assert_ast!(
            super::block(), input,
            [
                _,
                _,
                Node { parent: 7, .. },
                _,
                _,
                Node { parent: 6, .. },
                Node { kind: NodeKind::Stmt { child: 5, next: None }, parent: 7 },
                Node { kind: NodeKind::Stmt { child: 2, next: Some(6) }, parent: 8 },
                Node { kind: NodeKind::Block { first: Some(7) }, .. }
            ],
            [..]
        );

        // NOTE(cdecompilador): the ordering is wrong
        let input = input!("{ 1; 2; 3 }");
        assert_ast!(
            super::block(), input,
            [
                _,
                _,
                Node { parent: 7, .. },
                _,
                _,
                Node { parent: 6, .. },
                Node { kind: NodeKind::Stmt { child: 5, next: None }, parent: 7 },
                Node { kind: NodeKind::Stmt { child: 2, next: Some(6) }, parent: 8 },
                Node { kind: NodeKind::Block { first: Some(7) }, .. }
            ],
            [..]
        );
    } 
}
