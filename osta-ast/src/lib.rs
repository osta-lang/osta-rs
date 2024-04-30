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
    kind: NodeKind,
    parent: NodeRef,
}

pub struct Tree<'a> {
    nodes: Vec<Node>,
    datas: Vec<Data<'a>>
}
