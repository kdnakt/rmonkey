// Internal
use crate::{
    token::Token,
};

pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait Expression: Node {}
pub trait Statement: Node {}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

#[derive(Clone, Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    //value: dyn Expression,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        format!("{}", self.token.literal)
    }
}

impl Statement for LetStatement {}

#[derive(Clone, Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        format!("{}", self.token.literal)
    }
}

impl Expression for Identifier {}
