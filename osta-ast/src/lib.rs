use osta_lexer::token::*;

pub type NodeRef = usize;
pub type DataRef = usize;

pub const NULL_REF: usize = !0;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NodeKind {
    IntegerLiteral(DataRef),
    Identifier(DataRef),
    BinExpr { left: NodeRef, op: DataRef, right: NodeRef },
    Term(NodeRef),
    UnaryTerm { op: DataRef, child: NodeRef },
    FuncCallExpr { name: NodeRef, params: Option<NodeRef> },
    // NOTE(cdecompilador): this is like a linked list
    Param { child: NodeRef, next: Option<NodeRef> },
    ExprStmt { expr: NodeRef },
    AssignStmt { name: NodeRef, expr: NodeRef }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Data<'a> {
    Token(Token<'a>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Node {
    pub kind: NodeKind,
    pub parent: NodeRef,
}

#[derive(Debug)]
pub struct Ast<'a> {
    pub nodes: Vec<Node>,
    pub datas: Vec<Data<'a>>
}

#[derive(Debug)]
pub struct AstBuilder<'a> {
    pub ast: Ast<'a>,
    checkpoints: Vec<(NodeRef, DataRef)>
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

    pub fn push_node(&mut self, kind: NodeKind, parent: NodeRef) -> NodeRef {
        let node_ref = self.ast.nodes.len();
        self.ast.nodes.push(Node { kind, parent });
        node_ref
    }

    pub fn push_data(&mut self, data: Data<'a>) -> DataRef {
        let data_ref = self.ast.datas.len();
        self.ast.datas.push(data);
        data_ref
    }

    pub fn push_integer(&mut self, token: Token<'a>) -> (NodeRef, Option<DataRef>) {
        debug_assert!(token.kind == TokenKind::Int);

        let data_ref = self.push_data(Data::Token(token));
        let node_ref = self.push_node(NodeKind::IntegerLiteral(data_ref), NULL_REF);
        
        (node_ref, Some(data_ref))
    }

    pub fn push_identifier(&mut self, token: Token<'a>) -> (NodeRef, Option<DataRef>) {
        debug_assert!(token.kind == TokenKind::Identifier);

        let data_ref = self.push_data(Data::Token(token));
        let node_ref = self.push_node(NodeKind::Identifier(data_ref), NULL_REF);
        
        (node_ref, Some(data_ref))
    }

    pub fn push_term(&mut self, child_ref: NodeRef) -> NodeRef {
        let node_ref = self.push_node(NodeKind::Term(child_ref), !0);
        self.ast.nodes[child_ref].parent = node_ref;

        node_ref
    }

    pub fn push_unary(&mut self, unary_op_token: Token<'a>, child_ref: NodeRef) -> NodeRef {
        let unary_op_ref = self.push_data(Data::Token(unary_op_token));
        let node_ref = self.push_node(NodeKind::UnaryTerm {
            op: unary_op_ref,
            child: child_ref
        }, NULL_REF);
        self.ast.nodes[child_ref].parent = node_ref;

        node_ref
    }

    pub fn push_bin_expr(&mut self, left_ref: NodeRef, op_token: Token<'a>, right_ref: NodeRef) -> NodeRef {
        let op_ref = self.push_data(Data::Token(op_token));
        let node_ref = self.push_node(NodeKind::BinExpr {
            left: left_ref,
            op: op_ref,
            right: right_ref
        }, NULL_REF);
        self.ast.nodes[left_ref].parent = node_ref;
        self.ast.nodes[right_ref].parent = node_ref;

        node_ref
    }

    pub fn push_param(&mut self, child_ref: NodeRef, next_ref: Option<NodeRef>) -> NodeRef {
        let node_ref = self.push_node(NodeKind::Param {
            child: child_ref,
            next: next_ref
        }, NULL_REF);
        self.ast.nodes[child_ref].parent = node_ref;
        if let Some(next_ref) = next_ref {
            self.ast.nodes[next_ref].parent = node_ref;
        }

        node_ref
    }

    pub fn push_function_call_expr(&mut self, name: NodeRef, params: Option<NodeRef>) -> NodeRef {
        let node_ref = self.push_node(NodeKind::FuncCallExpr {
            name,
            params 
        }, NULL_REF);
        self.ast.nodes[name].parent = node_ref;
        if let Some(params) = params {
            self.ast.nodes[params].parent = node_ref;
            // TODO(cdecompilador): should we iterate over all the params and set the func_call node
            // as its parent to avoid indirection while traversing the tree bottom-up?
        }

        node_ref
    }

    pub fn push_expr_stmt(&mut self, expr: NodeRef) -> NodeRef {
        let node_ref = self.push_node(NodeKind::ExprStmt {
            expr
        }, NULL_REF);
        self.ast.nodes[expr].parent = node_ref;

        node_ref
    }

    pub fn push_assign_stmt(&mut self, name: NodeRef, expr: NodeRef) -> NodeRef {
        let node_ref = self.push_node(NodeKind::AssignStmt {
            name,
            expr
        }, NULL_REF);
        self.ast.nodes[name].parent = node_ref;
        self.ast.nodes[expr].parent = node_ref;

        node_ref
    }

    pub fn build(self) -> Ast<'a> {
        self.ast
    }
}
