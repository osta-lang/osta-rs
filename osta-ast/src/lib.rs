use osta_lexer::token::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NodeRef(pub usize);

impl NodeRef {
    pub const NULL: NodeRef = NodeRef(!0);
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct DataRef(pub usize);

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NodeKind {
    IntegerLiteral(DataRef),
    Identifier(DataRef),
    BinExpr { left_ref: NodeRef, op_ref: DataRef, right_ref: NodeRef },
    Term(NodeRef),
    UnaryTerm { op_ref: DataRef, child_ref: NodeRef },
    FuncCallExpr { name_ref: NodeRef, first_param_ref: NodeRef },
    Param { child_ref: NodeRef, next_ref: NodeRef },
    Stmt { child_ref: NodeRef, next_ref: NodeRef },
    ExprStmt { expr_ref: NodeRef },
    AssignStmt { name_ref: NodeRef, expr_ref: NodeRef },
    ReturnStmt { expr_ref: NodeRef },
    Block { first_stmt_ref: NodeRef },
    IfStmt { cond_ref: NodeRef, then_block_ref: NodeRef, else_block_ref: NodeRef },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Data<'a> {
    Token(Token<'a>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Node {
    pub kind: NodeKind,
    pub parent_ref: NodeRef,
}

#[derive(Debug)]
pub struct Ast<'a> {
    pub nodes: Vec<Node>,
    pub datas: Vec<Data<'a>>
}

#[derive(Debug)]
pub struct AstBuilder<'a> {
    pub ast: Ast<'a>,
    checkpoints: Vec<(usize, usize)>
}

impl<'a> AstBuilder<'a> {
    pub fn new() -> Self {
        Self {
            ast: Ast {
                nodes: Vec::new(),
                datas: Vec::new()
            },
            checkpoints: Vec::new()
        }
    }

    pub fn checkpoint(&mut self) {
        self.checkpoints.push((self.ast.nodes.len(), self.ast.datas.len()));
    }

    pub fn rollback<Err>(&mut self, err: Err) -> Result<(), Err> {
        let (node_len, data_len) = self.checkpoints.pop().ok_or(err)?;
        self.ast.nodes.truncate(node_len);
        self.ast.datas.truncate(data_len);
        Ok(())
    }

    pub fn commit(&mut self) {
        self.checkpoints.pop().or_else(|| panic!("No checkpoints to commit"));
    }

    /// It doesn't let you set the parent since the AST is built bottom up so at the step
    /// that you call this function you don't know the parent 
    pub fn push_node(&mut self, kind: NodeKind) -> NodeRef {
        let node_ref = NodeRef(self.ast.nodes.len());
        self.ast.nodes.push(Node { kind, parent_ref: NodeRef::NULL });

        node_ref
    }

    pub fn set_parent(&mut self, child: NodeRef, parent_ref: NodeRef) {
        debug_assert!(child != NodeRef::NULL);
        
        self.ast.nodes[child.0].parent_ref = parent_ref;
    }

    pub fn push_data(&mut self, data: Data<'a>) -> DataRef {
        let data_ref = DataRef(self.ast.datas.len());
        self.ast.datas.push(data);
        data_ref
    }

    pub fn push_integer(&mut self, token: Token<'a>) -> NodeRef {
        debug_assert!(token.kind == TokenKind::Int);

        let data_ref = self.push_data(Data::Token(token));
        let node_ref = self.push_node(NodeKind::IntegerLiteral(data_ref));
        
        node_ref
    }

    pub fn push_identifier(&mut self, token: Token<'a>) -> NodeRef {
        debug_assert!(token.kind == TokenKind::Identifier);

        let data_ref = self.push_data(Data::Token(token));
        let node_ref = self.push_node(NodeKind::Identifier(data_ref));
        
        node_ref
    }

    pub fn push_term(&mut self, child_ref: NodeRef) -> NodeRef {
        let node_ref = self.push_node(NodeKind::Term(child_ref));
        self.set_parent(child_ref, node_ref); 

        node_ref
    }

    pub fn push_unary(&mut self, op_token: Token<'a>, child_ref: NodeRef) -> NodeRef {
        debug_assert!(child_ref != NodeRef::NULL);
        
        let op_ref = self.push_data(Data::Token(op_token));
        let node_ref = self.push_node(NodeKind::UnaryTerm { op_ref, child_ref });
        self.set_parent(child_ref, node_ref);

        node_ref
    }

    pub fn push_bin_expr(&mut self, left_ref: NodeRef, op_token: Token<'a>, right_ref: NodeRef) -> NodeRef {
        debug_assert!(right_ref != NodeRef::NULL && left_ref != NodeRef::NULL);
        
        let op_ref = self.push_data(Data::Token(op_token));
        let node_ref = self.push_node(NodeKind::BinExpr { left_ref, op_ref, right_ref });
        self.set_parent(left_ref, node_ref);
        self.set_parent(right_ref, node_ref);

        node_ref
    }

    pub fn push_param(&mut self, child_ref: NodeRef, next_ref: NodeRef) -> NodeRef {
        debug_assert!(child_ref != NodeRef::NULL);
        
        let node_ref = self.push_node(NodeKind::Param { child_ref, next_ref });
        self.set_parent(child_ref, node_ref);
        if next_ref != NodeRef::NULL { self.set_parent(next_ref, node_ref); }

        node_ref
    }

    pub fn push_function_call_expr(&mut self, name_ref: NodeRef, first_param_ref: NodeRef) -> NodeRef {
        debug_assert!(name_ref != NodeRef::NULL);
        
        let node_ref = self.push_node(NodeKind::FuncCallExpr { name_ref, first_param_ref });
        self.set_parent(name_ref, node_ref);
        if first_param_ref != NodeRef::NULL { self.set_parent(first_param_ref, node_ref) }

        node_ref
    }

    pub fn push_expr_stmt(&mut self, expr_ref: NodeRef) -> NodeRef {
        debug_assert!(expr_ref != NodeRef::NULL);
        
        let node_ref = self.push_node(NodeKind::ExprStmt { expr_ref });
        self.set_parent(expr_ref, node_ref);

        node_ref
    }

    pub fn push_assign_stmt(&mut self, name_ref: NodeRef, expr_ref: NodeRef) -> NodeRef {
        debug_assert!(name_ref != NodeRef::NULL && expr_ref != NodeRef::NULL);
        
        let node_ref = self.push_node(NodeKind::AssignStmt { name_ref, expr_ref });
        self.set_parent(name_ref, node_ref);
        self.set_parent(expr_ref, node_ref);

        node_ref
    }

    /// Create a ReturnStmt node on the AST
    ///
    /// # NOTE
    /// The 'expr' may be NULL for the case of an 'return;' on a void function
    /// and always valid on the case of implicit return expression { stmt;* expr }
    pub fn push_return_stmt(&mut self, expr_ref: NodeRef) -> NodeRef {
        let node_ref = self.push_node(NodeKind::ReturnStmt { expr_ref });
        if expr_ref != NodeRef::NULL { self.set_parent(expr_ref, node_ref) }

        node_ref
    }

    pub fn push_stmt(&mut self, child_ref: NodeRef, next_ref: NodeRef) -> NodeRef {
        debug_assert!(child_ref != NodeRef::NULL);
        
        let node_ref = self.push_node(NodeKind::Stmt { child_ref, next_ref });
        self.set_parent(child_ref, node_ref);
        if next_ref != NodeRef::NULL { self.set_parent(next_ref, node_ref); }

        node_ref
    }

    /// Create a Block node on the AST
    ///
    /// # NOTE
    /// This block may be empty {} and that must trigger a warning (TODO) but its valid
    /// syntax 
    pub fn push_block(&mut self, first_stmt_ref: NodeRef) -> NodeRef {
        let node_ref = self.push_node(NodeKind::Block {  first_stmt_ref  });
        if first_stmt_ref != NodeRef::NULL { self.set_parent(first_stmt_ref, node_ref); }

        node_ref
    }

    pub fn push_if_stmt(&mut self, cond_ref: NodeRef, then_block_ref: NodeRef, else_block_ref: NodeRef) -> NodeRef {
        debug_assert!(cond_ref != NodeRef::NULL && then_block_ref != NodeRef::NULL);

        let node_ref = self.push_node(NodeKind::IfStmt { cond_ref, then_block_ref, else_block_ref });
        self.set_parent(cond_ref, node_ref);
        self.set_parent(then_block_ref, node_ref);
        if else_block_ref != NodeRef::NULL { self.set_parent(else_block_ref, node_ref); }

        node_ref
    }

    pub fn build(self) -> Ast<'a> {
        self.ast
    }
}
