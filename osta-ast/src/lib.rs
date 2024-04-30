use osta_lexer::token::Token;

pub type NodeRef = usize;
pub type DataRef = usize;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NodeKind {
    BinExpr { left: NodeRef, op: NodeRef, right: NodeRef },
    Term(NodeRef),
    BangExpr(NodeRef),
    FuncCall,
    Data(DataRef)
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

    pub fn build(self) -> Ast<'a> {
        self.ast
    }
}
