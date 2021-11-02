// Internal
use crate::{
    ast::StatementNode::*,
    ast::ExpressionNode::*,
    token::Token,
};

pub trait Node {
    fn token_literal(&self) -> String;
    fn to_string(&self) -> String;
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
}

pub struct Program {
    pub statements: Vec<StatementNode>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements.first().unwrap().token_literal()
        } else {
            "".to_string()
        }
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        for s in &self.statements {
            out.push_str(&s.to_string());
        }
        out
    }
}

impl Node for StatementNode {
    fn token_literal(&self) -> String {
        let t = match self {
            LetStatement{token, ..} => token,
            ReturnStatement{token, ..} => token,
            ExpressionStatement{token, ..} => token,
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
        }
        out.push(';');
        out
    }

}

impl Node for ExpressionNode {
    fn token_literal(&self) -> String {
        match self {
            Identifier{token, ..} => format!("{}", token.literal),
        }
    }

    fn to_string(&self) -> String {
        let mut out = String::new();
        match self {
            Identifier{value, ..} => out.push_str(value),
        }
        out
    }

}

#[cfg(test)]
mod tests {
    use super::{
        Program,
        LetStatement,
        Identifier,
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
            name: Identifier {
                token: Token { typ: IDENT, literal: "myVar".to_string() },
                value: "myVar".to_string(),
            },
            value: Some(Identifier {
                token: Token { typ: IDENT, literal: "anotherVar".to_string() },
                value: "anotherVar".to_string(),
            }),
        };
        statements.push(let_stmt);
        let program = Program { statements: statements };

        assert_eq!("let myVar = anotherVar;", program.to_string());
    }
}
