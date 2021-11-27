// Internal
use crate::{
    ast::AstNode::*,
    ast::StatementNode::*,
    ast::ExpressionNode::*,
    token::Token,
};

pub trait Node {
    fn token_literal(&self) -> String;
    fn to_string(&self) -> String;
}

pub enum AstNode {
    Statement {
        node: StatementNode,
    },
    Expression {
        node: ExpressionNode,
    },
    Program {
        statements: Vec<StatementNode>,
    },
}

#[derive(Debug, PartialEq)]
pub enum ExpressionNode {
    IdentifierExpression {
        token: Token,
        value: String
    },
    IntegerLiteral {
        token: Token,
        value: i64,
    },
    PrefixExpression {
        token: Token,
        operator: String,
        right: Box<Option<ExpressionNode>>,
    },
    InfixExpression {
        token: Token,
        left: Box<Option<ExpressionNode>>,
        operator: String,
        right: Box<Option<ExpressionNode>>,
    },
    Boolean {
        token: Token,
        value: bool,
    },
    IfExpression {
        token: Token,
        condition: Box<Option<ExpressionNode>>,
        consequence: Box<StatementNode>, // BlockStatement
        alternative: Box<Option<StatementNode>>, // BlockStatement
    },
    FunctionLiteral {
        token: Token,
        parameters: Vec<ExpressionNode>,
        body: Box<StatementNode>, // BlockStatement
    },
    CallExpression {
        token: Token,
        function: Box<Option<ExpressionNode>>, // Identifier or FunctionLiteral
        arguments: Vec<ExpressionNode>,
    },
}

#[derive(Debug, PartialEq)]
pub enum StatementNode {
    LetStatement {
        token: Token,
        name: ExpressionNode, // Identifier,
        value: Option<ExpressionNode>,
    },
    ReturnStatement {
        token: Token,
        return_value: Option<ExpressionNode>,
    },
    ExpressionStatement {
        token: Token,
        expression: Option<ExpressionNode>,
    },
    BlockStatement {
        token: Token,
        statements: Vec<StatementNode>,
    },
}

impl Node for AstNode {
    fn token_literal(&self) -> String {
        match self {
            Program{statements, ..} => {
                if statements.len() > 0 {
                    statements.first().unwrap().token_literal()
                } else {
                    "".to_string()
                }
            },
            Statement{node, ..} => node.token_literal(),
            Expression{node, ..} => node.token_literal(),
        }
    }

    fn to_string(&self) -> String {
        match self {
            Program{statements, ..} => {
                let mut out = String::new();
                for s in statements {
                    out.push_str(&s.to_string());
                }
                out
            },
            Statement{node, ..} => node.to_string(),
            Expression{node, ..} => node.to_string(),
        }
    }
}

impl Node for StatementNode {
    fn token_literal(&self) -> String {
        let t = match self {
            LetStatement{token, ..} => token,
            ReturnStatement{token, ..} => token,
            ExpressionStatement{token, ..} => token,
            BlockStatement{token, ..} => token,
        };
        format!("{}", t.literal)
    }

    fn to_string(&self) -> String {
        let mut out = String::from(self.token_literal());
        out.push(' ');

        match self {
            LetStatement{name, value, ..} => {
                out.push_str(&name.to_string());
                out.push_str(" = ");
                match value {
                    Some(e) => out.push_str(&e.to_string()),
                    None => (),
                };
            },
            ReturnStatement{return_value, ..} => {
                match return_value {
                    Some(e) => out.push_str(&e.to_string()),
                    None => (),
                };
            },
            ExpressionStatement{expression, ..} => {
                match expression {
                    Some(e) => return e.to_string(),
                    None => return "".to_string(),
                };
            },
            BlockStatement{statements, ..} => {
                let mut buf = String::new();
                for s in statements {
                    buf.push_str(&s.to_string());
                }
                return buf;
            },
        }
        out.push(';');
        out
    }

}

impl Node for ExpressionNode {
    fn token_literal(&self) -> String {
        let t = match self {
            IdentifierExpression{token, ..} => token,
            IntegerLiteral{token, ..} => token,
            PrefixExpression{token, ..} => token,
            InfixExpression{token, ..} => token,
            Boolean{token, ..} => token,
            IfExpression{token, ..} => token,
            FunctionLiteral{token, ..} => token,
            CallExpression{token, ..} => token,
        };
        format!("{}", t.literal)
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        match self {
            IdentifierExpression{value, ..} => out.push_str(value),
            IntegerLiteral{token, ..} => out.push_str(&token.literal),
            PrefixExpression{operator, right, ..} => {
                out.push('(');
                out.push_str(operator);
                match right.as_ref() {
                    Some(e) => out.push_str(&e.to_string()),
                    None => (),
                }
                out.push(')');
            },
            InfixExpression{left, operator, right, ..} => {
                out.push('(');
                match left.as_ref() {
                    Some(e) => out.push_str(&e.to_string()),
                    None => (),
                }
                out.push(' ');
                out.push_str(operator);
                out.push(' ');
                match right.as_ref() {
                    Some(e) => out.push_str(&e.to_string()),
                    None => (),
                }
                out.push(')');
            },
            Boolean{token, ..} => out.push_str(&token.literal),
            IfExpression{condition, consequence, alternative, ..} => {
                out.push_str("if");
                match condition.as_ref() {
                    Some(e) => out.push_str(&e.to_string()),
                    None => (),
                }
                out.push(' ');
                out.push_str(&consequence.to_string());
                match alternative.as_ref() {
                    Some(e) => {
                        out.push_str("else ");
                        out.push_str(&e.to_string());
                    },
                    None => (),
                }
            },
            FunctionLiteral{parameters, body, token, ..} => {
                out.push_str(&token.literal.to_string());
                out.push('(');
                out.push_str(&parameters.iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>().join(", "));
                out.push(')');
                out.push_str(&body.to_string());
            },
            CallExpression{function, arguments, ..} => {
                match function.as_ref() {
                    Some(e) => out.push_str(&e.to_string()),
                    None => (),
                }
                out.push('(');
                out.push_str(&arguments.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>().join(", "));
                out.push(')');
            }
        }
        out
    }

}

#[cfg(test)]
mod tests {
    use super::{
        Program,
        LetStatement,
        IdentifierExpression,
        Node,
    };
    use crate::token::{
        Token,
        TokenType::*,
    };
    #[test]
    fn it_returns_string() {
        let mut statements = Vec::new();
        let let_stmt = LetStatement {
            token: Token { typ: LET, literal: "let".to_string() },
            name: IdentifierExpression {
                token: Token { typ: IDENT, literal: "myVar".to_string() },
                value: "myVar".to_string(),
            },
            value: Some(IdentifierExpression {
                token: Token { typ: IDENT, literal: "anotherVar".to_string() },
                value: "anotherVar".to_string(),
            }),
        };
        statements.push(let_stmt);
        let program = Program { statements: statements };

        assert_eq!("let myVar = anotherVar;", program.to_string());
    }
}
