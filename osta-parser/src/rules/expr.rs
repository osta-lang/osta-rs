use osta_lexer::tokens;

use super::*;
use crate::rules::block::*;

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

fn term<'a>() -> impl Parser<'a> {
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

fn params<'a>() -> impl Parser<'a> {
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

fn function_call_expr<'a>() -> impl Parser<'a> {
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

#[cfg(test)]
mod tests {
    use osta_ast::*;
    use osta_func::*;
    use osta_lexer::token::*;
    use crate::rules::tests::*;
    
    #[test]
    fn integer() {
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
    fn identifier() {
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
    fn simple_term() {
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
    }

    #[test]
    fn term_with_expr() {
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
    }

    #[test]
    fn term_with_expr_block() {
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
    fn simple_binary_expr() {
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
    fn function_call_expr() {
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
}
