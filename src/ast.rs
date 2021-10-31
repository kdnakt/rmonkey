// Internal
use crate::{
    ast::StatementNode::*,
    ast::ExpressionNode::*,
    token::Token,
};

pub trait Node {
    fn token_literal(&self) -> String;
}

pub enum ExpressionNode {
    Identifier {
        token: Token,
        value: String
    },
}

pub enum StatementNode {
    LetStatement {
        token: Token,
        name: ExpressionNode, //Identifier,
        //value: ExpressionNode
    },
    ReturnStatement {
        token: Token,
        // retVal: ExpressionNode,
    },
}

pub struct Program {
    pub statements: Vec<StatementNode>,
}

impl Node for StatementNode {
    fn token_literal(&self) -> String {
        match self {
            LetStatement{token, ..} => format!("{}", token.literal),
            ReturnStatement{token, ..} => format!("{}", token.literal),
        }
    }
}

impl Node for ExpressionNode {
    fn token_literal(&self) -> String {
        match self {
            Identifier{token, ..} => format!("{}", token.literal),
        }
    }
}
