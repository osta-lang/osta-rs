use osta_lexer::token::Token;

pub type NodeRef = usize;
pub type DataRef = usize;

pub enum NodeKind {
    BinExpr { left: NodeRef, op: NodeRef, right: NodeRef },
    Term(NodeRef),
    BangExpr(NodeRef),
    FuncCall,
    Data(DataRef)
}

pub enum NodeData<'a> {
    Token(Token<'a>),
}

pub struct Node {
    kind: NodeKind,
    parent: NodeRef,
}

pub struct Tree<'a> {
    nodes: Vec<Node>,
    node_data: Vec<NodeData<'a>>
}
