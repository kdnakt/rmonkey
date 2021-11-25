// Internal
use crate::{
    ast::StatementNode::*,
    ast::ExpressionNode::*,
    ast::AstNode::*,
    token::Token,
};

pub trait Node {
    fn token_literal(&self) -> String;
    fn to_string(&self) -> String;
}

pub enum AstNode<'a> {
    Statement {
        node: StatementNode<'a>,
    },
    Expression {
        node: ExpressionNode<'a>,
    },
    Program {
        statements: Vec<StatementNode<'a>>,
    },
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ExpressionNode<'a> {
    IdentifierExpression {
        token: &'a Token<'a>,
        value: &'a String
    },
    IntegerLiteral {
        token: &'a Token<'a>,
        value: i64,
    },
    PrefixExpression {
        token: &'a Token<'a>,
        operator: &'a String,
        right: &'a Option<ExpressionNode<'a>>,
    },
    InfixExpression {
        token: &'a Token<'a>,
        left: &'a Option<ExpressionNode<'a>>,
        operator: &'a String,
        right: &'a Option<ExpressionNode<'a>>,
    },
    Boolean {
        token: &'a Token<'a>,
        value: bool,
    },
    IfExpression {
        token: &'a Token<'a>,
        condition: &'a Option<ExpressionNode<'a>>,
        consequence: &'a StatementNode<'a>, // BlockStatement
        alternative: &'a Option<StatementNode<'a>>, // BlockStatement
    },
    FunctionLiteral {
        token: &'a Token<'a>,
        parameters: &'a Vec<ExpressionNode<'a>>,
        body: &'a StatementNode<'a>, // BlockStatement
    },
    CallExpression {
        token: &'a Token<'a>,
        function: &'a Option<ExpressionNode<'a>>, // Identifier or FunctionLiteral
        arguments: &'a Vec<ExpressionNode<'a>>,
    },
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum StatementNode<'a> {
    LetStatement {
        token: &'a Token<'a>,
        name: ExpressionNode<'a>, // Identifier,
        value: Option<ExpressionNode<'a>>,
    },
    ReturnStatement {
        token: &'a Token<'a>,
        return_value: Option<ExpressionNode<'a>>,
    },
    ExpressionStatement {
        token: &'a Token<'a>,
        expression: Option<ExpressionNode<'a>>,
    },
    BlockStatement {
        token: &'a Token<'a>,
        statements: &'a Vec<StatementNode<'a>>,
    },
}

impl Node for AstNode<'_> {
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

impl Node for StatementNode<'_> {
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
                for s in *statements {
                    buf.push_str(&s.to_string());
                }
                return buf;
            },
        }
        out.push(';');
        out
    }

}

impl Node for ExpressionNode<'_> {
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
            token: &Token { typ: &LET, literal: &"let".to_string() },
            name: IdentifierExpression {
                token: &Token { typ: &IDENT, literal: &"myVar".to_string() },
                value: &"myVar".to_string(),
            },
            value: Some(IdentifierExpression {
                token: &Token { typ: &IDENT, literal: &"anotherVar".to_string() },
                value: &"anotherVar".to_string(),
            }),
        };
        statements.push(let_stmt);
        let program = Program { statements: statements };

        assert_eq!("let myVar = anotherVar;", program.to_string());
    }
}
