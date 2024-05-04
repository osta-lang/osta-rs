use osta_lexer::tokens;

use super::*;
use crate::rules::stmt::*;

fn stmts_in_block<'a>() -> impl Parser<'a> {
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

#[cfg(test)]
mod tests {
    use osta_ast::*;
    use osta_func::*;
    use crate::rules::tests::*;
    
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
