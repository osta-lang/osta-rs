use osta_lexer::tokens;

use super::*;
use crate::rules::stmt::*;

pub fn if_stmt<'a>() -> impl Parser<'a> {
    do_parse! {
        from_emitter(tokens::kw_if());
        from_emitter(tokens::lparen());
        cond_ref = expr();
        from_emitter(tokens::rparen());
        then_block_ref = block();
        else_block_ref = do_parse! {
            from_emitter(tokens::kw_else());
            block();
        }.optional();
        with_builder(move |mut builder| {
            builder.push_if_stmt(cond_ref, then_block_ref, else_block_ref)
        });
    }
}

#[cfg(test)]
mod tests {
    use osta_ast::*;
    use osta_func::*;
    use crate::rules::tests::*;

    #[test]
    fn simple_if_stmt() {
        let input = input!("if (1) { 2; }");
        assert_ast!(
            super::if_stmt(), input,
            [
                Node { kind: NodeKind::IntegerLiteral(DataRef(0)), parent_ref: NodeRef(1) },
                Node { kind: NodeKind::Term(NodeRef(0)), parent_ref: NodeRef(7) },
                Node { kind: NodeKind::IntegerLiteral(DataRef(1)), parent_ref: NodeRef(3) },
                Node { kind: NodeKind::Term(NodeRef(2)), parent_ref: NodeRef(4) },
                Node { kind: NodeKind::ExprStmt { expr_ref: NodeRef(3) }, parent_ref: NodeRef(5) },
                Node { kind: NodeKind::Stmt { child_ref: NodeRef(4), next_ref: NodeRef::NULL }, parent_ref: NodeRef(6) },
                Node { kind: NodeKind::Block { first_stmt_ref: NodeRef(5) }, parent_ref: NodeRef(7) },
                Node { kind: NodeKind::IfStmt { cond_ref: NodeRef(1), then_block_ref: NodeRef(6), else_block_ref: NodeRef::NULL }, parent_ref: NodeRef::NULL }
            ],
            [..]
        );
    }

    #[test]
    fn if_else_stmt() {
        let input = input!("if (1) { 2; } else { 3; }");
        assert_ast!(
            super::if_stmt(), input,
            [
                Node { kind: NodeKind::IntegerLiteral(DataRef(0)), parent_ref: NodeRef(1) },
                Node { kind: NodeKind::Term(NodeRef(0)), parent_ref: NodeRef(12) },
                Node { kind: NodeKind::IntegerLiteral(DataRef(1)), parent_ref: NodeRef(3) },
                Node { kind: NodeKind::Term(NodeRef(2)), parent_ref: NodeRef(4) },
                Node { kind: NodeKind::ExprStmt { expr_ref: NodeRef(3) }, parent_ref: NodeRef(5) },
                Node { kind: NodeKind::Stmt { child_ref: NodeRef(4), next_ref: NodeRef::NULL }, parent_ref: NodeRef(6) },
                Node { kind: NodeKind::Block { first_stmt_ref: NodeRef(5) }, parent_ref: NodeRef(12) },
                Node { kind: NodeKind::IntegerLiteral(DataRef(2)), parent_ref: NodeRef(8) },
                Node { kind: NodeKind::Term(NodeRef(7)), parent_ref: NodeRef(9) },
                Node { kind: NodeKind::ExprStmt { expr_ref: NodeRef(8) }, parent_ref: NodeRef(10) },
                Node { kind: NodeKind::Stmt { child_ref: NodeRef(9), next_ref: NodeRef::NULL }, parent_ref: NodeRef(11) },
                Node { kind: NodeKind::Block { first_stmt_ref: NodeRef(10) }, parent_ref: NodeRef(12) },
                Node { kind: NodeKind::IfStmt { cond_ref: NodeRef(1), then_block_ref: NodeRef(6), else_block_ref: NodeRef(11) }, parent_ref: NodeRef::NULL }
            ],
            [..]
        );
    }
}