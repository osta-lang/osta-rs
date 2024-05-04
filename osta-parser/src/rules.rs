use std::cell::RefMut;

use osta_ast::*;
use osta_func::*;
use osta_lexer::{base::*, tokens};
use osta_lexer::token::Token;

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

/// Non-fallible utility combinator that lets you take a mutable reference to the AstBuilder
// NOTE(cdecompilador): If we do typing on a separate part of the nodes in a attrbute-like 
// way we could return from just the node ref, which in fact is the only thing we use on later stages
pub fn with_builder<'a, F>(with_builder_fn: F) -> impl Parser<'a> 
where 
    F: Fn(RefMut<'_, AstBuilder<'a>>) -> NodeRef + Copy + 'a
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
        builder.push_term(child_ref)
    });

    (do_parse! {
        child_ref = integer();
        build_term(child_ref);
    }).or(do_parse! {
        child_ref = identifier();
        build_term(child_ref);
    }).or(do_parse! {
        from_emitter(tokens::lparen()); child_ref = defer!(expr()); from_emitter(tokens::rparen());
        build_term(child_ref);
    }).or(do_parse! {
        token = from_emitter(tokens::unary_op());
        child_ref = defer!(term());
        with_builder(move |mut builder| {
            builder.push_unary(token, child_ref)
        });
    }).or(do_parse! {
        child_ref = defer!(block());
        build_term(child_ref);
    })
}

pub fn params<'a>() -> impl Parser<'a> {
    do_parse! {
        expr_ref = expr();
        next_ref = do_parse! {
            from_emitter(tokens::comma());
            defer!(params());
        }.optional();
        with_builder(move |mut builder| {
            builder.push_param(expr_ref, next_ref)
        });
    }
}

pub fn function_call_expr<'a>() -> impl Parser<'a> {
    do_parse!(
        name_ref = identifier();
        from_emitter(tokens::lparen());
        do_parse! {
            from_emitter(tokens::rparen());
            with_builder(move |mut builder| {
                builder.push_function_call_expr(name_ref, NodeRef::NULL)
            });
        }.or(do_parse! {
            params_ref = params();
            from_emitter(tokens::rparen());
            with_builder(move |mut builder| {
                builder.push_function_call_expr(name_ref, params_ref)
            });
        });
    )
}

pub fn expr<'a>() -> impl Parser<'a> {
    defer!(function_call_expr()).or(do_parse! {
        left_ref = term();
        token = from_emitter(tokens::bin_op());
        right_ref = defer!(expr());
        with_builder(move |mut builder| {
            builder.push_bin_expr(left_ref, token, right_ref)
        });
    }).or(term())
}

pub fn expr_stmt<'a>() -> impl Parser<'a> {
    do_parse! {
        expr_ref = expr();
        from_emitter(tokens::semicolon());
        with_builder(move |mut builder| {
            builder.push_expr_stmt(expr_ref)
        });
    }
}

pub fn assign_stmt<'a>() -> impl Parser<'a> {
    do_parse! {
        name_ref = identifier();
        from_emitter(tokens::eq());
        expr_ref = expr();
        from_emitter(tokens::semicolon());
        with_builder(move |mut builder| {
            builder.push_assign_stmt(name_ref, expr_ref)
        });
    }
}

pub fn return_stmt<'a>() -> impl Parser<'a> {
    (do_parse! {
        from_emitter(tokens::kw_return());
        expr_ref = expr();
        from_emitter(tokens::semicolon());
        with_builder(move |mut builder| {
            builder.push_return_stmt(expr_ref)
        });
    }).or(do_parse! {
        expr_ref = expr();
        with_builder(move |mut builder| {
            builder.push_return_stmt(expr_ref)
        });
    })
}

pub fn stmt<'a>() -> impl Parser<'a> {
    expr_stmt().or(assign_stmt()).or(return_stmt())
}

pub fn stmts_in_block<'a>() -> impl Parser<'a> {
    do_parse! {
        stmt_ref = stmt();
        next_ref = defer!(stmts_in_block()).optional();
        with_builder(move |mut builder| {
            builder.push_stmt(stmt_ref, next_ref)
        });
    }
}

pub fn block<'a>() -> impl Parser<'a> {
    do_parse! {
        from_emitter(tokens::lbrace());
        first_stmt_ref = stmts_in_block().optional();
        from_emitter(tokens::rbrace());
        with_builder(move |mut builder| {
            builder.push_block(first_stmt_ref)
        });
    }
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
                Node { kind: NodeKind::IntegerLiteral(DataRef(0)), parent_ref: NodeRef::NULL }
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
                Node { kind: NodeKind::Identifier(DataRef(0)), .. }
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
                Node { kind: NodeKind::IntegerLiteral(DataRef(0)), parent_ref: NodeRef(1) },
                Node { kind: NodeKind::Term(NodeRef(0)), parent_ref: NodeRef(2) },
                Node { kind: NodeKind::Term(NodeRef(1)), parent_ref: NodeRef::NULL }
            ],
            [
                Data::Token(Token { kind: TokenKind::Int, .. })
            ]
        );

        let input = input!("(123 + foo)");
        assert_ast!(
            super::term(), input,
            [
                Node { kind: NodeKind::IntegerLiteral(DataRef(0)), parent_ref: NodeRef(1) },
                Node { kind: NodeKind::Term(NodeRef(0)), parent_ref: NodeRef(4) },
                Node { kind: NodeKind::Identifier(DataRef(1)), parent_ref: NodeRef(3) },
                Node { kind: NodeKind::Term(NodeRef(2)), parent_ref:  NodeRef(4) },
                Node { kind: NodeKind::BinExpr { 
                    left_ref: NodeRef(1), op_ref: DataRef(2), right_ref: NodeRef(3)
                    }, 
                    parent_ref: NodeRef(5)
                },
                Node { kind: NodeKind::Term(NodeRef(4)), parent_ref: NodeRef::NULL }
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
                Node { kind: NodeKind::IntegerLiteral(DataRef(0)), parent_ref: NodeRef(1) },
                Node { kind: NodeKind::Term(NodeRef(0)), parent_ref: NodeRef(2) },
                Node { kind: NodeKind::UnaryTerm { op_ref: DataRef(1), child_ref: NodeRef(1) }, parent_ref: NodeRef::NULL }
            ],
            [
                Data::Token(Token { kind: TokenKind::Int, .. }),
                Data::Token(Token { kind: TokenKind::Minus, .. })
            ]
        );

        let input = input!("{123}");
        assert_ast!(
            super::term(), input,
            [
                Node { kind: NodeKind::IntegerLiteral(DataRef(0)), parent_ref: NodeRef(1) },
                Node { kind: NodeKind::Term(NodeRef(0)), parent_ref: NodeRef(2) },
                Node { kind: NodeKind::ReturnStmt { expr_ref: NodeRef(1) }, parent_ref: NodeRef(3) },
                Node { kind: NodeKind::Stmt { child_ref: NodeRef(2), next_ref: NodeRef::NULL }, parent_ref: NodeRef(4) },
                Node { kind: NodeKind::Block { first_stmt_ref: NodeRef(3) }, parent_ref: NodeRef(5) },
                Node { kind: NodeKind::Term(NodeRef(4)), parent_ref: NodeRef::NULL }
            ],
            [
                Data::Token(Token { kind: TokenKind::Int, .. }),
            ]
        );
    }

    #[test]
    fn node_rollback() {
        let input = input!("foo $");
        let rest = assert_ast!(
            super::identifier().and(|_| super::integer()).or(super::identifier()), input,
            [ 
                Node { kind: NodeKind::Identifier(DataRef(0)), .. }
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
                Node { kind: NodeKind::IntegerLiteral(DataRef(0)), .. },
                Node { kind: NodeKind::Term(NodeRef(0)), parent_ref: NodeRef(4) },
                Node { kind: NodeKind::IntegerLiteral(DataRef(1)), .. },
                Node { kind: NodeKind::Term(NodeRef(2)), parent_ref:  NodeRef(4) },
                Node { kind: NodeKind::BinExpr { left_ref: NodeRef(1), op_ref: DataRef(2), right_ref: NodeRef(3) }, .. }
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
                Node { kind: NodeKind::IntegerLiteral(_), .. },
                Node { kind: NodeKind::Term(_), .. },
                Node { kind: NodeKind::Param { child_ref: NodeRef(1), next_ref: NodeRef::NULL }, parent_ref: NodeRef::NULL }
            ],
            [..]
        );

        let input = input!("10,foo");
        assert_ast!(
            super::params(), input,
            [
                Node { kind: NodeKind::IntegerLiteral(_), .. },
                Node { kind: NodeKind::Term(_), .. },
                Node { kind: NodeKind::Identifier(_), .. },
                Node { kind: NodeKind::Term(_), .. },
                Node { kind: NodeKind::Param { child_ref: NodeRef(3), next_ref: NodeRef::NULL }, parent_ref: NodeRef(5) },
                Node { kind: NodeKind::Param { child_ref: NodeRef(1), next_ref: NodeRef(4)}, parent_ref: NodeRef::NULL },
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
                Node { kind: NodeKind::Identifier(_), parent_ref: NodeRef(1) },
                Node { kind: NodeKind::FuncCallExpr { name_ref: NodeRef(0), first_param_ref: NodeRef::NULL }, .. }
            ],
            [..]
        );

        let input = input!("print(hello)");
        assert_ast!(
            super::function_call_expr(), input,
            [
                Node { kind: NodeKind::Identifier(_), parent_ref: NodeRef(4) },
                Node { kind: NodeKind::Identifier(_), .. },
                Node { kind: NodeKind::Term(_), .. },
                Node { kind: NodeKind::Param { child_ref: NodeRef(2), next_ref: NodeRef::NULL }, parent_ref: NodeRef(4) },
                Node { kind: NodeKind::FuncCallExpr { name_ref: NodeRef(0), first_param_ref: NodeRef(3) }, .. }
            ],
            [..]
        );
    }

    #[test]
    fn assign_stmt() {
        let input = input!("a = 10;");
        assert_ast!(
            super::stmt(), input,
            [
                Node { kind: NodeKind::Identifier(_), .. },
                Node { kind: NodeKind::IntegerLiteral(_), .. },
                Node { kind: NodeKind::Term(_), .. },
                Node { kind: NodeKind::AssignStmt { name_ref: NodeRef(0), expr_ref: NodeRef(2) }, ..}
            ],
            [..]
        );
    }

    #[test]
    fn expr_stmt() {
        let input = input!("1 + exit();");
        assert_ast!(
            super::stmt(), input,
            [
                _,
                _,
                _,
                Node { kind: NodeKind::FuncCallExpr { .. }, .. },
                Node { kind: NodeKind::BinExpr { .. }, parent_ref: NodeRef(5) },
                Node { kind: NodeKind::ExprStmt { expr_ref: NodeRef(4) }, .. }
            ],
            [..]
        );
    }

    #[test]
    fn simple_block() {
        let input = input!("{ 1; 2; }");
        assert_ast!(
            super::block(), input,
            [
                _,
                _,
                Node { parent_ref: NodeRef(7), .. },
                _,
                _,
                Node { parent_ref: NodeRef(6), .. },
                Node { kind: NodeKind::Stmt { child_ref: NodeRef(5), next_ref: NodeRef::NULL }, parent_ref: NodeRef(7) },
                Node { kind: NodeKind::Stmt { child_ref: NodeRef(2), next_ref: NodeRef(6) }, parent_ref: NodeRef(8) },
                Node { kind: NodeKind::Block { first_stmt_ref: NodeRef(7) }, .. }
            ],
            [..]
        );
    }

    #[test]
    fn empty_block() {
        let input = input!("{}");
        assert_ast!(
            super::block(), input,
            [
               Node { kind: NodeKind::Block { first_stmt_ref: NodeRef::NULL }, .. } 
            ],
            [..]
        );
    }

    #[test]
    fn block_implicit_return() {
        let input = input!("{ 1; 2; 3 }");
        assert_ast!(
            super::block(), input,
            [
                _,
                _,
                Node { parent_ref: NodeRef(11), .. },
                _,
                _,
                Node { parent_ref: NodeRef(10), .. },
                _,
                _,
                Node { kind: NodeKind::ReturnStmt { expr_ref: NodeRef(7) }, parent_ref: NodeRef(9) },
                Node { kind: NodeKind::Stmt { child_ref: NodeRef(8), next_ref: NodeRef::NULL }, parent_ref: NodeRef(10) },
                Node { kind: NodeKind::Stmt { child_ref: NodeRef(5), next_ref: NodeRef(9) }, parent_ref: NodeRef(11) },
                Node { kind: NodeKind::Stmt { child_ref: NodeRef(2), next_ref: NodeRef(10) }, parent_ref: NodeRef(12) },
                Node { kind: NodeKind::Block { first_stmt_ref: NodeRef(11) }, .. }
            ],
            [..]
        );
    } 
}
