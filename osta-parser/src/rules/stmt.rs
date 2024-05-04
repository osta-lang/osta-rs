use osta_lexer::tokens;

use super::*;
use crate::rules::expr::*;

fn expr_stmt<'a>() -> impl Parser<'a> {
    do_parse! {
        expr_ref = expr();
        from_emitter(tokens::semicolon());
        with_builder(move |mut builder| {
            builder.push_expr_stmt(expr_ref)
        });
    }
}

fn assign_stmt<'a>() -> impl Parser<'a> {
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

fn return_stmt<'a>() -> impl Parser<'a> {
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

#[cfg(test)]
mod tests {
    use osta_ast::*;
    use osta_func::*;
    use crate::rules::tests::*;
    
    #[test]
    fn simple_assign_stmt() {
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
    fn simple_expr_stmt() {
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
    fn simple_return_stmt() {
        todo!() 
    }

    #[test]
    fn empty_return_stmt() {
        todo!()
    }
}
